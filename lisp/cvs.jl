;;;; cvs.jl -- CVS interface for Jade
;;;  Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
;;;  $Id$

;;; This file is part of Jade.

;;; Jade is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; Jade is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with Jade; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'summary)
(provide 'cvs)


;; Configuration

(defvar cvs-program "cvs"
  "The name of the program used to execute CVS commands.")

(defvar cvs-option-alist '(("diff" . ("-c")))
  "Alist of (COMMAND-NAME . OPTION-LIST) defining extra command options to
give to CVS commands.")

(defvar cvs-cvsroot nil
  "When non-nil the directory to use as the root of the CVS repository.")


;; Variables

(defvar cvs-output-buffer (make-buffer "*cvs-output*")
  "Buffer used for output from CVS commands.")
(set-buffer-special cvs-output-buffer t)

(defvar cvs-process (make-process)
  "Process object used to run CVS commands.")

(defvar cvs-update-char-map '((?U . updated)
			      (?A . added)
			      (?R . removed)
			      (?M . modified)
			      (?C . conflict)
			      (?? . unknown))
  "Alist mapping `cvs update' status characters to cvs-mode status tags.")
  
(defvar cvs-file-list 'invalid
  "The list of known CVS file structures, or the symbol `invalid' designating
that the list needs to be rebuilt.")

(defvar cvs-update-pending nil
  "Previous output from `cvs update' that didn't end in a newline character
and hence hasn't been processed yet; or nil.")

(defvar cvs-keymap (copy-sequence summary-keymap))
(bind-keys cvs-keymap
  "%" 'cvs-summary-clean
  "o" 'cvs-summary-select-other-view
  "a" 'cvs-add
  "A" 'cvs-change-log-other-view
  "b" 'cvs-diff-backup
  "c" 'cvs-commit
  "d" 'cvs-diff-cvs
  "f" 'cvs-find-file
  "g" 'cvs-update-no-prompt
  "G" 'cvs-update
  "i" 'cvs-ignore
  "l" 'cvs-log
  "p" 'cvs-update-pwd
  "P" 'cvs-update-parent
  "r" 'cvs-remove
  "R" 'cvs-revert
  "s" 'cvs-status
  "~" 'cvs-undo-modification
  "`" 'cvs-next-conflict-marker)

(bind-keys ctrl-x-keymap
  "c" '(setq next-keymap-path '(cvs-keymap)))

(defvar cvs-callback-ctrl-c-keymap (make-keylist))
(bind-keys cvs-callback-ctrl-c-keymap
  "Ctrl-c" 'cvs-callback-finished)

(defvar cvs-summary-functions '((print . cvs-summary-print)
				(after-move . (lambda ()
						(goto-glyph
						 (pos cvs-cursor-column nil))))
				(list . cvs-summary-list)
				(after-marking . (lambda ()
						   (summary-next-item 1)))
				(select . cvs-summary-select)
				(on-quit . bury-buffer))
  "Alist of summary-mode functions for CVS.")

(defvar cvs-cursor-column 19
  "The glyph-column at which the cursor is positioned in the CVS summary.")

(defvar cvs-callback-function nil
  "The function called as (FUNCTION MESSAGE) when a CVS callback buffer has
been completed.")
(make-variable-buffer-local 'cvs-callback-function)


;; CVS support functions

;; Each file we deal with has a structure as the following
;; [DIRECTORY FILENAME FULLNAME STATUS ..what else?]
;; where STATUS is one of the following:
;;   unchanged, updated, modified, added, removed, conflict, unknown
;; If FILENAME is nil, then the item refers to a directory

(defmacro cvs-make-file-struct (dir file fullname status)
  `(vector ,dir ,file ,fullname ,status))

(defmacro cvs-file-get-dirname (f)
  `(aref ,f 0))

(defmacro cvs-file-get-filename (f)
  `(aref ,f 1))

(defmacro cvs-file-get-fullname (f)
  `(aref ,f 2))

(defmacro cvs-file-get-status (f)
  `(aref ,f 3))

(defmacro cvs-file-set-status (f status)
  `(aset ,f 3 ,status))

(defun cvs-get-filenames-by-dir (files)
  "From the list of CVS file structures FILES, return a list of the files in
each level of the directory hierarchy, i.e. ((DIR FILENAMES...) ...), such
that each of the FILENAMES contains no directory specifiers."
  (let*
      ((dir-files (cons (list (cvs-file-get-dirname (car files))
			      (cvs-file-get-filename (car files))) nil)))
    (setq files (cdr files))
    (while files
      (if (string= (cvs-file-get-dirname (car files)) (car (car dir-files)))
	  (setcdr (car dir-files) (cons (cvs-file-get-filename (car files))
					(cdr (car dir-files))))
	(setq dir-files (cons (list (cvs-file-get-dirname (car files))
				    (cvs-file-get-filename (car files)))
			      dir-files)))
      (setq files (cdr files)))
    ;; Now dir-files is a list of ((DIR . FILES...) ...)
    dir-files))

;; Function receiving output from the `cvs update' command
(defun cvs-update-filter (o)
  (let
      ((out (concat cvs-update-pending o))
       (point 0)
       (not-done t))
    (while not-done
      (cond
       ((string-looking-at ". ([^\n]+)\n" out point)
	;; Found a status line
	(let
	    ((name (expand-last-match "\\1")))
	  (setq cvs-file-list (cons (cvs-make-file-struct
				     (directory-file-name
				      (file-name-directory name))
				     (file-name-nondirectory name)
				     name
				     (cdr (assq (aref out point)
						cvs-update-char-map)))
				    cvs-file-list)))
	(setq point (match-end)))
       ((string-match "\n" out point)
	;; Something else. Display it
	(message (substring out point (match-start)) t)
	(setq point (match-end)))
       (t
	;; An unfinished line
	(setq not-done nil)
	(unless (= point (length out))
	  (setq cvs-update-pending (substring out point))))))))

(defun cvs-update-file-list ()
  "Rebuild the `cvs-file-list' by calling `cvs update' and parsing its output.
Returns the new value of the list."
  (save-some-buffers)
  (setq cvs-file-list nil)
  (cvs-command '() "update" '() t 'cvs-update-filter nil t)
  (setq cvs-update-pending nil
	cvs-file-list (nreverse cvs-file-list)))

(defun cvs-command (cvs-opts command command-opts
		    &optional ignore-errors output-stream directory
		    dont-clear-output)
  "Execute a CVS command, something like `cvs CVS-OPTS COMMAND COMMAND-OPTS'.
Unless IGNORE-ERRORS is non-nil, an error will be signalled if the command
returns anything less than total success.

All output (both stdout and stderr from the command will be directed to
OUTPUT-STREAM of the `*cvs-output*' buffer if this is undefined. The command
will be run in the directory DIRECTORY, or the `default-directory' of the
`*cvs*' buffer, if DIRECTORY is undefined.

Finally, unless the DONT-CLEAR-OUTPUT parameter is non-nil, the
`*cvs-output*' buffer will be cleared before the command is invoked."
  (unless dont-clear-output
    (clear-buffer cvs-output-buffer))
  (let
      ((arg-list (append cvs-opts (and cvs-cvsroot (list "-d" cvs-cvsroot))
			 (list command)
			 (cdr (assoc command cvs-option-alist)) command-opts)))
    (set-process-dir cvs-process (or directory
				     (with-buffer (get-buffer "*cvs*")
				       default-directory)))
    (set-process-output-stream cvs-process (or output-stream
					       cvs-output-buffer))
    (set-process-error-stream cvs-process (or output-stream
					      cvs-output-buffer))
    (format t "Running CVS command: %s..." arg-list)
    (unless (or (zerop (apply 'call-process cvs-process
			      nil cvs-program arg-list))
		 ignore-errors)
      (error "CVS returned non-zero")))
  (write t " done"))

(defun cvs-show-output-buffer (&optional activate)
  "Ensure that the `*cvs-output*' buffer is visible in the current window,
probably in the other view. If ACTIVATE is non-nil, the view displaying the
buffer will be activated."
  (unless (with-buffer cvs-output-buffer
	    (equal (start-of-buffer) (end-of-buffer)))
    (let
	((view (or (get-buffer-view cvs-output-buffer) (other-view))))
      (with-view view
	(goto-buffer cvs-output-buffer)
	(goto (start-of-buffer))
	(shrink-view-if-larger-than-buffer))
      (when activate
	(set-current-view view)))))

(defun cvs-callback-with-message (title function)
  "Arrange for FUNCTION to be called with its sole argument a piece of text
entered in a new buffer, under the heading TITLE."
  (let
      ((buffer (open-buffer (concat "*cvs:" title ?*) t)))
    (goto-buffer buffer)
    (set-buffer-special buffer t)
    (text-mode)
    (setq cvs-callback-function function
	  mildly-special-buffer t
	  ctrl-c-keymap cvs-callback-ctrl-c-keymap)))

(defun cvs-callback-finished ()
  "Signal that the message in current buffer is complete, and therefore
the CVS command waiting for it can be invoked."
  (interactive)
  (goto (end-of-buffer))
  (when (and (> (buffer-length) 1) (= (pos-col (cursor-pos)) 0))
    (goto (forward-char -1)))
  (let
      ((function cvs-callback-function)
       (message (copy-area (start-of-buffer) (cursor-pos))))
    (kill-buffer (current-buffer))
    (funcall function message)))

(defun cvs-get-working-revisions (filenames)
  "Returns a list of strings defining the working revisions of all files
whose names are in the list FILENAMES (in the same order)."
  (cvs-command nil "status" filenames)
  ;; Now grovel in the output for revision numbers
  (let
      ((revs nil)
       (point (pos 0 0)))
    (while (re-search-forward
	    "^[ \t]*Working revision:[ \t]*([0-9]+(\\.[0-9]+)*)"
	    point cvs-output-buffer t)
      (setq revs (cons (expand-last-match "\\1") revs)
	    point (match-end)))
    (nreverse revs)))


;; Entry points

;;;###autoload
(defun cvs-update (directory)
  "Switch to the `*cvs*' buffer, displaying a summary of all CVS working
files under the directory called DIRECTORY. See the `cvs-summary-mode'
function for a list of the commands available for manipulating these files.

When called interactively, DIRECTORY is prompted for."
  (interactive "DWorking directory:")
  (unless (file-directory-p directory)
    (error "%S is not a directory" directory))
  (setq directory (directory-file-name (expand-file-name directory)))
  (let
      ((buffer (open-buffer "*cvs*"))
       (inhibit-read-only t))
    (goto-buffer buffer)
    (if (and (eq major-mode 'cvs-summary-mode)
	     (file-name= default-directory directory))
	;; The *cvs* buffer is already set up for this directory
	;; Just update it.
	(progn
	  (setq cvs-file-list 'invalid)
	  (summary-update))
      (clear-buffer)
      (kill-all-local-variables)
      (format buffer "[CVS] %s:\n\n" directory)
      (setq default-directory directory
	    cvs-file-list 'invalid)
      (summary-mode "CVS" cvs-summary-functions cvs-keymap)
      (setq major-mode 'cvs-summary-mode))))

(defun cvs-update-no-prompt ()
  "Run `cvs-update' *without* prompting for a directory."
  (interactive)
  (let
      ((buffer (get-buffer "*cvs*")))
    (if buffer
	(cvs-update (with-buffer buffer default-directory))
      (call-command 'cvs-update))))

;;;###autoload
(defun cvs-update-parent ()
  "Run `cvs-update' in the parent of the `default-directory'."
  (interactive)
  (cvs-update ".."))

;;;###autoload
(defun cvs-update-pwd ()
  "Run `cvs-update' in the `default-directory'."
  (interactive)
  (cvs-update "."))


;; CVS summary mechanics

(defun cvs-update-if-summary ()
  "If the current buffer is the `*cvs*' summary buffer, call `cvs-update'."
  (when (eq major-mode 'cvs-summary-mode)
    (setq cvs-file-list 'invalid)
    (summary-update)))

(defun cvs-summary-list ()
  (if (eq cvs-file-list 'invalid)
      (cvs-update-file-list)
    cvs-file-list))

(defun cvs-summary-print (item)
  (let
      ((pending (summary-get-pending-ops item)))
    (format (current-buffer) "%c%c %12s -- %s"
	    (if (get-file-buffer (cvs-file-get-fullname item)) ?B ? )
	    (if (memq 'mark pending) ?* ? )
	    (symbol-name (cvs-file-get-status item))
	    (cvs-file-get-fullname item))))

(defun cvs-summary-select (item)
  (if (file-directory-p (cvs-file-get-fullname item))
      (cvs-update (cvs-file-get-fullname item))
    (find-file (cvs-file-get-fullname item))))

(defun cvs-summary-select-other-view ()
  (interactive)
  (let
      ((item (summary-current-item)))
    (goto-other-view)
    (cvs-summary-select item)))

(defun cvs-find-file ()
  "Open the file under the cursor in the `*cvs*' summary."
  (interactive)
  (find-file (cvs-file-get-fullname (summary-current-item))))


;; CVS commands

;; All may be called from either the *cvs* summary buffer, or from the
;; buffer of a file to operate on

(defun cvs-command-get-files ()
  "Return a list of CVS file structures corresponding to the files to be
operated on by the current CVS mode command."
  (if (eq major-mode 'cvs-summary-mode)
      ;; In the summary buffer, either all marked files, or if none
      ;; are marked, the file under the cursor
      (or (filter #'(lambda (x)
		      (memq 'mark (summary-get-pending-ops x))) cvs-file-list)
	  (list (summary-current-item)))
    ;; In a normal buffer. Try to find a CVS file structure for it
    (filter #'(lambda (x)
		(file-name= (cvs-file-get-fullname x) (buffer-file-name)))
	    cvs-file-list)))

(defun cvs-command-get-filenames ()
  "Return a list of file names corresponding to the files to be operated on
by the current CVS mode command."
  (mapcar #'(lambda (x)
	      (cvs-file-get-fullname x)) (cvs-command-get-files)))

(defun cvs-log ()
  "Displays the CVS logs of all selected files."
  (interactive)
  (cvs-command nil "log" (cvs-command-get-filenames))
  (cvs-show-output-buffer))

(defun cvs-status ()
  "Displays the CVS status of all selected files."
  (interactive)
  (cvs-command nil "status" (cvs-command-get-filenames))
  (cvs-show-output-buffer))

(defun cvs-add ()
  "Prompts for a log message, then adds each selected file to CVS using this
message. Note that this doesn't add the files to the central repository, use
cvs-commit after calling this command for that."
  (interactive)
  (cvs-callback-with-message
   "Adding files"
   `(lambda (m)
      (cvs-add-callback ',(cvs-command-get-files) m))))

(defun cvs-add-callback (files message)
  ;; Not possible to just call add. Instead it's necessary to iterate
  ;; through each directory that files are added in
  (let
      ((dir-files (cvs-get-filenames-by-dir files)))
    (clear-buffer cvs-output-buffer)
    (mapc #'(lambda (cell)
	      (cvs-command nil "add" (list* "-m" message (cdr cell))
			   nil nil (car cell) t)) dir-files)
    (cvs-show-output-buffer)
    (cvs-update-if-summary)))

(defun cvs-remove ()
  "Delete all selected files and remove them under from CVS control. This
doesn't change the central repository, a subsequent call to cvs-commit will
do that."
  (interactive)
  (let
      ((files (cvs-command-get-filenames)))
    (map-y-or-n-p "Really delete file `%s'?" files 'delete-file)
    ;; Remove any files that the user answered negatively to
    (setq files (delete-if 'file-exists-p files))
    (when files
      (cvs-command nil "remove" (cvs-command-get-filenames))
      (cvs-show-output-buffer)
      (cvs-update-if-summary))))

(defun cvs-commit ()
  "Commit all selected CVS files, after prompting for the log message to
commit them under."
  (interactive)
  (cvs-callback-with-message
   "Committing files"
   `(lambda (m)
      (cvs-commit-callback ',(cvs-command-get-filenames) m))))

(defun cvs-commit-callback (filenames message)
  (save-some-buffers)
  (cvs-command nil "commit" (list* "-m" message filenames))
  ;; Revert all loaded files (in case of keyword substitutions, etc.)
  (mapc #'(lambda (f)
	    (let
		((b (get-file-buffer f)))
	      (when b
		(revert-buffer b)))) filenames)
  (cvs-show-output-buffer)
  (cvs-update-if-summary))

(defun cvs-revert ()
  "Any CVS files whose status is `updated' or `conflict', and who are cached
locally in an editor buffer, are reverted to their on-disk versions."
  (interactive)
  (mapc #'(lambda (f)
	    (when (memq (cvs-file-get-status f) '(updated conflict))
	      (let
		  ((b (get-file-buffer (cvs-file-get-filename f))))
		(when b
		  (revert-buffer b))))) cvs-file-list))

(defun cvs-ignore ()
  "Add all currently selected CVS files to the list of files that should
be ignored by CVS. This is done by appending their names to the `.cvsignore'
files in the corresponding working directories."
  (interactive)
  (mapc #'(lambda (cell)
	    (when (find-file (expand-file-name ".cvsignore" (car cell)))
	      (goto (end-of-buffer))
	      (mapc #'(lambda (f)
			(unless (re-search-forward
				 (concat ?^ (quote-regexp f) ?$)
				 (start-of-buffer))
			  (unless (zerop (pos-col (cursor-pos)))
			    (insert "\n"))
			  (insert f)
			  (insert "\n")))
		    (cdr cell))
	      (save-file)
	      (bury-buffer)))
	(cvs-get-filenames-by-dir (cvs-command-get-files)))
  (cvs-update-if-summary))

(defun cvs-diff-cvs ()
  "Display all differences between the currently selected CVS files and their
corresponding revisions in the central repository."
  (interactive)
  (save-some-buffers)
  (cvs-command nil "diff" (cvs-command-get-filenames) t)
  (cvs-show-output-buffer))

(defun cvs-diff-backup ())

(defun cvs-undo-modification ())

(defun cvs-next-conflict-marker ()
  "Find the next CVS conflict marker in the current buffer."
  (interactive "@")
  (or (re-search-forward "^<<<<<<< ")
      (re-search-forward "^<<<<<<< " (start-of-buffer))))
