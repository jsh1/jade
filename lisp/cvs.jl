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

(defvar cvs-diff-command "diff -c '%s' '%s'"
  "Format string defining shell command to run to compare a backup working
file and the current version. Used by the `cvs-diff-backup' command. Two
strings can be substituted (using `%s'), the original file, and the merged
version containing the conflict markers.")

(defvar cvs-option-alist '(("diff" . ("-c"))
			   ("status" . ("-v")))
  "Alist of (COMMAND-NAME . OPTION-LIST) defining extra command options to
give to CVS commands.")

(defvar cvs-cvsroot nil
  "When non-nil the directory to use as the root of the CVS repository.")


;; Variables

(defvar cvs-buffer (open-buffer "*cvs*")
  "Buffer used to display CVS summaries.")

(defvar cvs-output-buffer (make-buffer "*cvs-output*")
  "Buffer used for output from CVS commands.")

(defvar cvs-update-char-map '((?U . updated)
			      (?A . added)
			      (?R . removed)
			      (?M . modified)
			      (?C . conflict)
			      (?? . unknown))
  "Alist mapping `cvs update' status characters to cvs-mode status tags.")
  
(defvar cvs-file-list nil
  "The list of known CVS file structures.")

(defvar cvs-update-pending nil
  "Previous output from `cvs update' that didn't end in a newline character
and hence hasn't been processed yet; or nil.")

(defvar cvs-update-in-progress nil
  "Non-nil when a `cvs update' process is running asynchronously.")

(defvar cvs-update-file-list nil
  "Used by the `cvs update' filter to build the new list.")

(defvar cvs-keymap (copy-sequence summary-keymap))
(bind-keys cvs-keymap
  "a" 'cvs-add
  "A" 'cvs-change-log-other-view
  "b" 'cvs-diff-backup
  "c" 'cvs-commit
  "C" 'cvs-commit-directory
  "d" 'cvs-diff-cvs
  "f" 'cvs-find-file
  "g" 'cvs-update-no-prompt
  "G" 'cvs-update
  "i" 'cvs-ignore
  "l" 'cvs-log
  "o" 'cvs-summary-select-other-view
  "p" 'cvs-update-pwd
  "P" 'cvs-update-parent
  "r" 'cvs-remove
  "R" 'cvs-revert
  "s" 'cvs-status
  "t" 'cvs-tag
  "T" 'cvs-tag-directory
  "%" 'cvs-summary-clean
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
				(list . (lambda ()
					  cvs-file-list))
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

;; Extra cvs-command parameters
(defvar cvs-command-ignore-errors nil)
(defvar cvs-command-output-stream nil)
(defvar cvs-command-directory nil)
(defvar cvs-command-dont-clear-output nil)
(defvar cvs-command-async nil)


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
	  (setq cvs-update-file-list (cons (cvs-make-file-struct
					    (directory-file-name
					     (file-name-directory name))
					    (file-name-nondirectory name)
					    name
					    (cdr (assq (aref out point)
						       cvs-update-char-map)))
					   cvs-update-file-list)))
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

(defun cvs-error-if-updating ()
  (and cvs-update-in-progress
       (error "CVS update in progress")))

(defun cvs-update-file-list ()
  "Rebuild the `cvs-file-list' by calling `cvs update' and parsing its output."
  (cvs-error-if-updating)
  (save-some-buffers)
  (setq cvs-update-file-list nil)
  (let
      ((cvs-command-ignore-errors t)
       (cvs-command-output-stream 'cvs-update-filter)
       (cvs-command-dont-clear-output t)
       (cvs-command-async
	#'(lambda ()
	    (setq cvs-update-pending nil
		  cvs-file-list (nreverse cvs-update-file-list)
		  cvs-update-in-progress nil)
	    (with-buffer cvs-buffer
	      (summary-update)
	      (remove-minor-mode 'cvs-update "Updating"))
	    (unless (cvs-buffer-p)
	      (with-view (other-view)
		(goto-buffer cvs-buffer)
		(shrink-view-if-larger-than-buffer)))
	    (add-buffer cvs-buffer)
	    (message "cvs update finished"))))
    (setq cvs-update-in-progress (cvs-command '() "update" '()))
    (when cvs-update-in-progress
      (with-buffer cvs-buffer
	(add-minor-mode 'cvs-update "Updating")))))

(defun cvs-command (cvs-opts command command-opts)
  "Execute a CVS command, something like `cvs CVS-OPTS COMMAND COMMAND-OPTS'.
If cvs-command-async is nil, the command is executed synchronously, i.e.
this function won't return until the command has completed. If
cvs-command-async is non-nil, the command will be executed asynchronously;
if cvs-command-async is a function, it will be called when the process
terminates.

Unless cvs-command-ignore-errors is non-nil, an error will be signalled if the
command returns anything less than total success.

All output (both stdout and stderr) from the command will be directed to
cvs-command-output-stream of the `*cvs-output*' buffer if this is undefined.
The command will be run in the directory cvs-command-directory, or the
`default-directory' of the `*cvs*' buffer, if nil.

Finally, unless the cvs-command-dont-clear-output parameter is non-nil, the
`*cvs-output*' buffer will be cleared before the command is invoked."
  (unless cvs-command-dont-clear-output
    (clear-buffer cvs-output-buffer))
  (let
      ((arg-list (append cvs-opts (and cvs-cvsroot (list "-d" cvs-cvsroot))
			 (list command)
			 (cdr (assoc command cvs-option-alist)) command-opts))
       (process (make-process (or cvs-command-output-stream cvs-output-buffer)
			      (and (functionp cvs-command-async)
				   cvs-command-async)
			      (or cvs-command-directory
				  (with-buffer cvs-buffer
				    default-directory)))))
    (message (format nil "%sing CVS: %s..."
		     (if cvs-command-async "Start" "Call") arg-list) t)
    (unless (or (if cvs-command-async
		    (apply 'start-process process cvs-program arg-list)
		  (zerop (apply 'call-process process nil
				cvs-program arg-list)))
		cvs-command-ignore-errors)
      (error "whilst running cvs"))))

(defun cvs-kill-processes (&optional force)
  "Interrupt all active CVS processes (after confirmation). If FORCE is non-nil
don't ask for confirmation and kill instead of interrupting."
  (interactive)
  (let
      ((processes (filter #'(lambda (p)
			      (string= (process-prog p) cvs-program))
			  (active-processes))))
    (if processes
	(if force
	    (mapc 'kill-process processes)
	  (map-y-or-n-p #'(lambda (p)
			    (format nil "Really interrupt `cvs %s'?"
				    (process-args p)))
			processes 'interrupt-process))
      (message "[No CVS processes active]"))))

(defmacro cvs-buffer-p ()
  "Return t if the current buffer is the CVS summary buffer."
  '(eq major-mode 'cvs-summary-mode))

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
    (text-mode)
    (setq cvs-callback-function function
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
      ((inhibit-read-only t))
    (with-buffer cvs-buffer
      (unless (and (cvs-buffer-p)
		   (file-name= default-directory directory))
	;; Initialise the buffer for this directory
	(clear-buffer)
	(kill-all-local-variables)
	(format cvs-buffer "[CVS] %s:\n\n" directory)
	(setq default-directory directory
	      cvs-file-list nil)
	(summary-mode "CVS" cvs-summary-functions cvs-keymap)
	(setq major-mode 'cvs-summary-mode))
      ;; Now build the list of interesting files
      (cvs-update-file-list))))

(defun cvs-update-no-prompt ()
  "Run `cvs-update' *without* prompting for a directory."
  (interactive)
  (cvs-update (with-buffer cvs-buffer default-directory)))

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

(defun cvs-summary-mode ()
  "CVS summary mode:

Major mode for manipulating a CVS repository. Use the `cvs-update' command
to call `cvs update' on a specified directory, displaying in the standard
summary interface all files that have been modified in some way in this
working directory.

Most of the standard `summary-mode' commands are available (with the notable
exception of the `delete' operations). Extra commands that are specific
to CVS mode include:

  `a'			Add all selected files to the local copy
			 of the repository
  `b'			For selected files whose status is `conflict'
			 show the differences between the previous
			 working copy and the new merged working copy
  `c'			Prompt for a log message, then commit all
			 selected files to the repository
  `C'			Run commit recursively in the current directory,
			 specifying a prefix-argument causes the directory
			 to be prompted for.
  `d'			Display the differences between all selected files
			 and their relations in the repository
  `f'			Open the current item in the current view
  `g'			Call `cvs update' again, rebuilding the display
  `G'			Prompt for a new working directory to call
			 `cvs update' on
  `i'			Add all selected files to the relevant .cvsignore
			 files so that CVS disregards them
  `l'			Display the logs of all selected files
  `o'			Open the current item in the other view
  `p'			Call `cvs-update' on the current directory
  `P'			Call `cvs-update' on the parent of the
			 current directory
  `r'			Delete all selected files then remove them from
			 the local copy of the repository
  `R'			Revert all locally buffered files that have
			 changed since they were loaded (via the update)
  `s'			Display the `cvs status' information of all
			 selected files
  `t'			Add a symbolic tag (prompted for) to all
			 selected files
  `T'			Prompt for the name of a working directory, then a
			 tag, then tag all files under the directory
  `%'			Remove all uninteresting items from the summary
  `~'			Undo all modifications made to the selected
			 files since they were checked out
  ``'			Search the current file for the next CVS
			 conflict marker

Where the commands work on the currently `selected' files, this means
one of two things:

   1. All files in the summary that are marked by an asterisk `*' (use
      the summary-mode `m' command to mark files, `u' or `U' to unmark
      them); of if no files are marked

   2. The current summary item. If a prefix argument is given, then ARG
      files starting at the current item are selected.

Note that all commands described above are available in all buffers by
prefixing them with the `Ctrl-x c' key sequence. For example, type
`Ctrl-x c s' to display the CVS status of the current buffer.")


;; CVS summary mechanics

(defun cvs-update-if-summary ()
  "If the current buffer is the `*cvs*' summary buffer, call `cvs-update'."
  (when (cvs-buffer-p)
    (cvs-update-file-list)))

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
      ((file (expand-file-name (cvs-file-get-fullname
				(summary-current-item)))))
    (goto-other-view)
    (find-file file)))

(defun cvs-find-file ()
  "Open the file under the cursor in the `*cvs*' summary."
  (interactive)
  (find-file (cvs-file-get-fullname (summary-current-item))))

(defun cvs-summary-clean ()
  "Remove all uninteresting files from the CVS summary. This includes
anything whose status is `unchanged' or `updated'."
  (interactive)
  (cvs-error-if-updating)
  (setq cvs-file-list (delete-if #'(lambda (f)
				     (memq (cvs-file-get-status f)
					   '(unchanged updated)))
				 cvs-file-list))
  (when (cvs-buffer-p)
    (summary-update)))


;; CVS commands

;; All may be called from either the *cvs* summary buffer, or from the
;; buffer of a file to operate on

(defun cvs-command-get-files ()
  "Return a list of CVS file structures corresponding to the files to be
operated on by the current CVS mode command."
  (cvs-error-if-updating)
  (if (cvs-buffer-p)
      (summary-command-items)
    ;; In a normal buffer. Try to find a CVS file structure for it
    (or (filter #'(lambda (x)
		    (file-name= (cvs-file-get-fullname x) (buffer-file-name)))
		cvs-file-list)
	;; This file isn't in the cvs-file-list, so make our own structure
	(list (cvs-make-file-struct
	       (or (local-file-name (directory-file-name default-directory))
		   (error "Can only run CVS on local files"))
	       (file-name-nondirectory (buffer-file-name))
	       (local-file-name (buffer-file-name)) 'unchanged)))))

(defun cvs-command-get-filenames ()
  "Return a list of file names corresponding to the files to be operated on
by the current CVS mode command."
  (mapcar #'(lambda (x)
	      (cvs-file-get-fullname x)) (cvs-command-get-files)))

(defun cvs-log ()
  "Displays the CVS logs of all selected files."
  (interactive)
  (let
      ((cvs-command-async #'(lambda ()
			      (cvs-show-output-buffer))))
    (cvs-command nil "log" (cvs-command-get-filenames))))

(defun cvs-status ()
  "Displays the CVS status of all selected files."
  (interactive)
  (let
      ((cvs-command-async #'(lambda ()
			      (cvs-show-output-buffer))))
    (cvs-command nil "status" (cvs-command-get-filenames))))

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
	      (let
		  ((cvs-command-directory (car cell))
		   (cvs-command-dont-clear-output t))
		(cvs-command nil "add" (list* "-m" message (cdr cell)))))
	  dir-files)
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

(defun cvs-commit-directory (directory)
  "Commit all CVS files under the current working directory that need to be.
If a prefix argument is given, the directory to commit in is prompted for."
  (interactive
   (list (if current-prefix-arg
	     (prompt-for-directory "Directory to commit in:")
	   ".")))
  (cvs-callback-with-message
   "Committing files"
   `(lambda (m)
      (cvs-commit-callback '(,directory) m))))

(defun cvs-commit-callback (filenames message)
  (save-some-buffers)
  (let
      ((cvs-command-async `(lambda ()
			     ;; Need backquote since FILENAMES won't
			     ;; still be bound when this evaluates
			     (cvs-revert-filenames ',filenames)
			     (cvs-show-output-buffer)
			     (cvs-update-if-summary))))
    (cvs-command nil "commit" (list* "-m" message filenames))))

(defun cvs-revert-filenames (filenames)
  "Revert any buffers that edit a file named in the list FILENAMES. As a
special case, if a directory is named in FILENAMES, any buffers editing
files under that directory are also reverted."
  (mapc #'(lambda (f)
	    (if (file-directory-p f)
		;; Try to revert _anything_ under directory F
		(let
		    ((canon-f (canonical-file-name
			       (file-name-as-directory f))))
		  (mapc #'(lambda (b)
			    (when (and (not (string= (buffer-file-name b) ""))
				       (string-head-eq (canonical-file-name
							(buffer-file-name))
						       canon-f))
			      (revert-buffer b)))
			buffer-list))
	      ;; A normal file
	      (let
		  ((b (get-file-buffer f)))
		(when b
		  (revert-buffer b)))))
	filenames))
  
(defun cvs-revert ()
  "Any CVS files whose status is `updated' or `conflict', and who are cached
locally in an editor buffer, are reverted to their on-disk versions."
  (interactive)
  (cvs-error-if-updating)
  (mapc #'(lambda (f)
	    (when (memq (cvs-file-get-status f) '(updated conflict))
	      (let
		  ((b (get-file-buffer (cvs-file-get-fullname f))))
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
  (let
      ((cvs-command-ignore-errors t)
       (cvs-command-async #'(lambda ()
			      (cvs-show-output-buffer))))
    (cvs-command nil "diff" (cvs-command-get-filenames))))

(defun cvs-diff-backup ()
  "Display the differences between the currently selected CVS file and its
backup file (created by a merge with conflicts.)"
  (interactive)
  (save-some-buffers)
  (let
      ((working-file (cvs-command-get-filenames))
       working-revision backup-file)
    (unless (eq (cdr working-file) nil)
      (message "[Ignoring all but the first file!]" t)
      ;; Give them time to read the message..
      (sleep-for 1))
    (setq working-file (car working-file))
    ;; I wanted to use cvs-get-working-revisions to find the revision
    ;; number appended to the backup file; but it gets the merged rev.
    ;; So do it the rude way..
    (let*
	((head (concat ".#" (file-name-nondirectory working-file) "."))
	 (possibilities (filter #'(lambda (f)
				    (string-head-eq f head))
				(directory-files
				 (file-name-directory working-file)))))
      (unless possibilities
	(error "Can't find backup file"))
      (setq backup-file (concat (file-name-directory working-file)
				(car possibilities))))
    ;; Need a diff interface
    (shell-command (format nil cvs-diff-command backup-file working-file))))

(defun cvs-undo-modification ()
  "Discard any local changes made to the currently selected CVS files. This
works by deleting the local copy, before updating it from the repository."
  (interactive)
  (let
      ((files (cvs-command-get-filenames)))
    (map-y-or-n-p "Really lose changes to `%s'?" files 'delete-file)
    ;; Remove any files that the user answered negatively to
    (setq files (delete-if 'file-exists-p files))
    (if (cvs-buffer-p)
	(cvs-update-no-prompt)
      (cvs-command nil "update" files))
    (cvs-revert-filenames files)))

(defun cvs-tag (tag-name)
  "Tag all selected CVS files with the string TAG-NAME."
  (interactive "sTag:")
  (let
      ((cvs-command-async #'(lambda ()
			      (cvs-show-output-buffer))))
    (cvs-command nil "tag" (cons tag-name (cvs-command-get-filenames)))))

(defun cvs-tag-directory (directory tag-name)
  "Tag all CVS controlled files under DIRECTORY with the string TAG-NAME."
  (interactive "DDirectory:\nsTag:")
  (let
      ((cvs-command-async #'(lambda ()
			      (cvs-show-output-buffer))))
    (unless (setq directory (local-file-name directory))
      (error "Can only work on local directories"))
    (cvs-command nil "tag" (list tag-name directory))))

(defun cvs-next-conflict-marker ()
  "Find the next CVS conflict marker in the current buffer."
  (interactive "@")
  (or (re-search-forward "^<<<<<<< ")
      (re-search-forward "^<<<<<<< " (start-of-buffer))))
