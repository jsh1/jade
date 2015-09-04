;;;; git.jl -- git interface for Jade
;;;  Copyright (C) 1998-2011 John Harper <jsh@unfactored.org>

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
(provide 'git)


;; Configuration

(defvar git-program "git"
  "The name of the program used to execute git commands.")

(defvar git-diff-format-string "diff -u '%s' '%s'"
  "Format string defining shell command to run to compare a backup working
file and the current version. Used by the `git-diff-backup' command. Two
strings can be substituted (using `%s'), the original file, and the merged
version containing the conflict markers.")

(defvar git-short-log-options '("--pretty=tformat:%h%x09%s")
  "Command-line options for git-short-log command.")

(defvar git-option-alist nil
  "Alist of (COMMAND-NAME . OPTION-LIST) defining extra command options to
give to git commands.")

(defvar git-print-long-status t
  "When non-nil, print whole-word status descriptions in git summary buffers,
instead of the standard `git status --porcelain' single characters.")


;; Variables

(defvar git-status-char-map '((#\  . unmodified)
			      (#\M . modified)
			      (#\A . added)
			      (#\D . deleted)
			      (#\C . copied)
			      (#\U . updated)
			      (#\? . unknown))
  "Alist mapping `git status --porcelain' status characters to git-mode
status tags.")

(defvar git-default-directory
  "Root working directory in which git is working.")

(defvar git-file-list nil
  "The list of known git file structures.")

(defvar git-update-pending nil
  "Previous output from `git status' that didn't end in a newline character
and hence hasn't been processed yet; or nil.")
(defvar git-update-pending-stderr nil)

(defvar git-update-in-progress nil
  "Non-nil when a `git status' process is running asynchronously.")

(setq minor-mode-alist (cons '(git-update-in-progress " git-update")
			     minor-mode-alist))

(defvar git-update-files nil
  "Used by the `git status' filter to build the new list.")

(defvar git-after-update-hook nil
  "Hook called after completing a git-update command.")

(defvar git-keymap
  (bind-keys (make-sparse-keymap summary-keymap)
    "a" 'git-add
    "A" 'git-add-change-log-entries
    "b" 'git-diff-backup
    "c" 'git-commit
    "d" 'git-diff-index
    "D" 'git-diff-cached
    "f" 'git-find-file
    "g" 'git-update-no-prompt
    "G" 'git-update
    "i" 'git-ignore
    "l" 'git-log
    "L" 'git-short-log
    "s" 'git-file-log
    "S" 'git-short-file-log
    "o" 'git-find-file-other-view
    "p" 'git-update-pwd
    "P" 'git-update-parent
    "r" 'git-remove
    "R" 'git-reset-index
    "t" 'git-tag
    "%" 'git-summary-clean
    "`" 'git-next-conflict-marker))

;;;###autoload (autoload 'git-keymap "git")
;;;###autoload (bind-keys ctrl-x-keymap "g" 'git-keymap)

(defvar git-callback-ctrl-c-keymap
  (bind-keys (make-sparse-keymap)
    "Ctrl-c" 'git-callback-finished))

(defvar git-cursor-column 32
  "The glyph-column at which the cursor is positioned in the git summary.")

(defvar git-callback-function nil
  "The function called as (FUNCTION MESSAGE) when a git callback buffer has
been completed.")
(make-variable-buffer-local 'git-callback-function)

;; Extra git-command parameters
(defvar git-command-ignore-errors nil)
(defvar git-command-output-stream nil)
(defvar git-command-error-stream nil)
(defvar git-command-directory nil)
(defvar git-command-dont-clear-output nil)
(defvar git-command-async nil)


;; git support functions

;; Each file we deal with has a structure as the following
;; [DIRECTORY FILENAME FULLNAME INDEX-STATUS WORK-STATUS ..what else?]
;; where STATUS is one of the following:
;;   unmodified, modified, added, deleted, copied, updated, unknown
;; If FILENAME is nil, then the item refers to a directory

(defmacro git-make-file-struct (dir file fullname index-status work-status)
  `(vector ,dir ,file ,fullname ,index-status ,work-status))

(defmacro git-file-get-dirname (f)
  `(aref ,f 0))

(defmacro git-file-get-filename (f)
  `(aref ,f 1))

(defmacro git-file-get-fullname (f)
  `(aref ,f 2))

(defmacro git-file-get-index-status (f)
  `(aref ,f 3))

(defmacro git-file-set-index-status (f status)
  `(aset ,f 3 ,status))

(defmacro git-file-get-work-status (f)
  `(aref ,f 4))

(defmacro git-file-set-work-status (f status)
  `(aset ,f 4 ,status))

(defun git-get-filenames-by-dir (files)
  "From the list of git file structures FILES, return a list of the files in
each level of the directory hierarchy, i.e. ((DIR FILENAMES...) ...), such
that each of the FILENAMES contains no directory specifiers."
  (let* ((dir-files (cons (list (git-file-get-dirname (car files))
				(git-file-get-filename (car files))) nil)))
    (setq files (cdr files))
    (while files
      (if (string=? (git-file-get-dirname (car files)) (car (car dir-files)))
	  (setcdr (car dir-files) (cons (git-file-get-filename (car files))
					(cdr (car dir-files))))
	(setq dir-files (cons (list (git-file-get-dirname (car files))
				    (git-file-get-filename (car files)))
			      dir-files)))
      (setq files (cdr files)))
    ;; Now dir-files is a list of ((DIR . FILES...) ...)
    dir-files))

;; Function receiving output from the `git status' command
(defun git-update-filter (o)
  (let ((out (concat git-update-pending o))
	(point 0)
	(not-done t))
    (setq git-update-pending nil)
    (while not-done
      (cond
       ((string-looking-at ".. (.* -> )?([^\n]+)\n" out point)
	;; Found a status line
	(let ((name (expand-last-match "\\2")))
	  (setq git-update-files (cons (git-make-file-struct
					(directory-file-name
					 (file-name-directory name))
					(file-name-nondirectory name)
					name
					(cdr (assq (aref out point)
						   git-status-char-map))
					(cdr (assq (aref out (1+ point))
						   git-status-char-map)))
				       git-update-files)))
	(setq point (match-end)))
       ((string-match "\n" out point)
	;; Something else. Display it
	(message (substring out point (match-start)) t)
	(setq point (match-end)))
       (t
	;; An unfinished line
	(setq not-done nil)
	(unless (= point (length out))
	  (setq git-update-pending (substring out point))))))))

(defun git-update-stderr-filter (o)
  (let ((out (concat git-update-pending-stderr o))
	(point 0))
    (setq git-update-pending-stderr nil)
    (while (string-looking-at "([^\n]+)\n" out point)
      (message (expand-last-match "\\1") t)
      (setq point (match-end)))
    (setq git-update-pending-stderr (substring out point))))

;; Function called after `git update' has completed
(defun git-update-finished (hook)
  (let ((buffer (open-buffer "*git*"))
	(inhibit-read-only t))
    (setq git-update-pending nil
	  git-update-pending-stderr nil
	  git-file-list (nreverse git-update-files)
	  git-update-in-progress nil)
    (with-buffer buffer
      (if (and (git-buffer-p)
	       (file-name= *default-directory* git-default-directory))
	  ;; Already initialised as a summary buffer
	  (summary-update)
	(clear-buffer)
	(format (current-buffer) "[GIT] %s:\n\n   %12s %12s\n"
		git-default-directory "index" "tree")
	(setq *default-directory* git-default-directory)
	(summary-mode "git" git-summary-functions git-keymap)
	(setq major-mode 'git-summary-mode)))
    (unless (git-buffer-p)
      (with-view (other-view)
	(goto-buffer buffer)
	(shrink-view-if-larger-than-buffer)))
    (add-buffer buffer)
    (message "git update finished")
    (call-hook hook)))

(defun git-error-if-updating ()
  (and git-update-in-progress
       (process-in-use? git-update-in-progress)
       (error "git update in progress")))

(defun git-update-file-list ()
  "Rebuild the `git-file-list' by calling `git status' and parsing its output."
  (git-error-if-updating)
  (save-some-buffers)
  (setq git-update-files nil)
  (let ((git-command-ignore-errors t)
	(git-command-output-stream git-update-filter)
	(git-command-error-stream git-update-stderr-filter)
	(git-command-dont-clear-output t)
	(git-command-async
	 ;; Need to construct a call using the _current_ value of
	 ;; git-after-update-hook (in case it's bound dynamically)
	 (let ((hook git-after-update-hook))
	   (lambda () (git-update-finished hook)))))
    (setq git-update-in-progress (git-command '() "status" '("--porcelain")))))

;; Return the buffer used for output from git commands. If CLEAR is
;; t delete all of its current contents
(defun git-output-buffer (#!optional clear)
  (let ((buffer (open-buffer "*git-output*")))
    (when clear
      (clear-buffer buffer))
    buffer))

(defun git-command (git-opts command command-opts)
  "Execute a git command, something like `git git-OPTS COMMAND COMMAND-OPTS'.
If git-command-async is nil, the command is executed synchronously, i.e.
this function won't return until the command has completed. If
git-command-async is non-nil, the command will be executed asynchronously;
if git-command-async is a function, it will be called when the process
terminates.

Unless git-command-ignore-errors is non-nil, an error will be signalled if the
command returns anything less than total success.

All output (both stdout and stderr) from the command will be directed to
git-command-output-stream of the `*git-output*' buffer if this is undefined.
The command will be run in the directory git-command-directory, or the
`git-default-directory', if nil.

Finally, unless the git-command-dont-clear-output parameter is non-nil, the
`*git-output*' buffer will be cleared before the command is invoked."
  (let ((output (git-output-buffer (not git-command-dont-clear-output))))
    (let ((arg-list (append git-opts
			    (list command)
			    (cdr (assoc command git-option-alist))
			    command-opts))
	  (process (make-process (or git-command-output-stream output)
				 (and (function? git-command-async)
				      git-command-async)
				 (or git-command-directory
				     git-default-directory))))
      (when git-command-error-stream
	(set-process-error-stream process git-command-error-stream))
      (message (format nil "%sing git: %s..."
		       (if git-command-async "Start" "Call") arg-list) t)
      (unless (or (if git-command-async
		      (apply start-process process git-program arg-list)
		    (zero? (apply call-process process nil
				  git-program arg-list)))
		  git-command-ignore-errors)
	(error "whilst running git")))))

(defun git-kill-processes (#!optional force)
  "Interrupt all active git processes (after confirmation). If FORCE is non-nil
don't ask for confirmation and kill instead of interrupting."
  (interactive)
  (let ((processes (filter (lambda (p)
			     (string=? (process-prog p) git-program))
			   (active-processes))))
    (if processes
	(if force
	    (mapc kill-process processes)
	  (map-y-or-n-p (lambda (p)
			  (format nil "Really interrupt `git %s'?"
				  (process-args p)))
			processes interrupt-process))
      (message "[No git processes active]"))))

(defmacro git-buffer-p ()
  "Return t if the current buffer is the git summary buffer."
  '(eq? major-mode 'git-summary-mode))

(defun git-show-output-buffer (#!optional activate)
  "Ensure that the `*git-output*' buffer is visible in the current window,
probably in the other view. If ACTIVATE is non-nil, the view displaying the
buffer will be activated."
  (let ((buffer (git-output-buffer)))
    (unless (with-buffer buffer
	      (equal? (start-of-buffer) (end-of-buffer)))
      (let ((view (or (get-buffer-view buffer) (other-view)))
	    (original-buffer (current-buffer)))
	(with-view view
	  (goto-buffer buffer)
	  (goto (start-of-buffer))
	  (unless (with-buffer original-buffer (git-buffer-p))
	    (shrink-view-if-larger-than-buffer)))
	(when activate
	  (set-current-view view))))))

(defun git-callback-with-message (title function #!optional setup-thunk)
  "Arrange for FUNCTION to be called with its sole argument a piece of text
entered in a new buffer, under the heading TITLE."
  (let ((buffer (open-buffer (concat "*git:" title #\*) t)))
    (goto-buffer buffer)
    (text-mode)
    (setq git-callback-function function
	  local-ctrl-c-keymap git-callback-ctrl-c-keymap)
    (when setup-thunk
      (setup-thunk))))

(defun git-callback-finished ()
  "Signal that the message in current buffer is complete, and therefore
the git command waiting for it can be invoked."
  (interactive)
  (goto (end-of-buffer))
  (when (and (> (buffer-length) 1) (= (pos-col (cursor-pos)) 0))
    (goto (forward-char -1)))
  (let ((function git-callback-function)
	(msg (copy-area (start-of-buffer) (cursor-pos))))
    (kill-buffer (current-buffer))
    (function msg)))


;; Entry points

;;;###autoload
(defun git-update (directory)
  "Switch to the `*git*' buffer, displaying a summary of all git working
files under the directory called DIRECTORY. See the `git-summary-mode'
function for a list of the commands available for manipulating these files.

When called interactively, DIRECTORY is prompted for."
  (interactive "DWorking directory:")
  (unless (file-directory? directory)
    (error "%S is not a directory" directory))
  (setq directory (directory-file-name (expand-file-name directory)))
  (setq git-default-directory directory
	git-file-list nil)
  ;; Now build the list of interesting files
  (git-update-file-list))

(defun git-update-no-prompt ()
  "Run `git-update' *without* prompting for a directory."
  (interactive)
  (git-update git-default-directory))

;;;###autoload
(defun git-update-parent ()
  "Run `git-update' in the parent of the `*default-directory*'."
  (interactive)
  (git-update ".."))

;;;###autoload
(defun git-update-pwd ()
  "Run `git-update' in the `*default-directory*'."
  (interactive)
  (git-update "."))

(defun git-summary-mode ()
  "git summary mode:

Major mode for manipulating a git repository. Use the `git-update' command
to call `git update' on a specified directory, displaying in the standard
summary interface all files that have been modified in some way in this
working directory.

Most of the standard `summary-mode' commands are available (with the notable
exception of the `delete' operations). Extra commands that are specific
to git mode include:

\\{git-keymap}

Most of the commands work on the currently `selected' files, this means
one of two things:

   1. All files in the summary that are marked by an asterisk `*' (use
      the summary-mode `m' command to mark files, `u' or `U' to unmark
      them); of if no files are marked,

   2. The current summary item. If a prefix argument is given, then ARG
      files starting at the current item are selected.

Note that all commands described above are available in all buffers by
prefixing them with the `Ctrl-x c' key sequence. For example, type
`Ctrl-x c s' to display the git status of the current buffer.")


;; git summary mechanics

(defun git-update-if-summary ()
  "If the current buffer is the `*git*' summary buffer, call `git-update'."
  (when (git-buffer-p)
    (git-update-file-list)))

(defun git-summary-print (item)
  (let ((pending (summary-get-pending-ops item)))
    (format (current-buffer) "%c%c %12s %12s -- %s"
	    (if (get-file-buffer (git-file-get-fullname item)) #\B #\ )
	    (if (memq 'mark pending) #\* #\ )
	    (if git-print-long-status
		(symbol-name (git-file-get-index-status item))
	      (make-string 1 (car (rassq (git-file-get-index-status item)
					 git-status-char-map))))
	    (if git-print-long-status
		(symbol-name (git-file-get-work-status item))
	      (make-string 1 (car (rassq (git-file-get-work-status item)
					 git-status-char-map))))
	    (git-file-get-fullname item))))

(defun git-summary-select (item)
  (if (file-directory? (git-file-get-fullname item))
      (git-update (git-file-get-fullname item))
    (find-file (git-file-get-fullname item))))

(defun git-find-file-other-view ()
  (interactive)
  (git-find-file t))

(defun git-find-file (#!optional in-other)
  "Open the selected files in the `*git*' summary."
  (interactive)
  (let ((files (git-command-get-filenames))
	(root git-default-directory))
    (when in-other
      (goto-other-view))
    (mapc (lambda (f)
	    (find-file (expand-file-name f root))) files)))

(defun git-summary-clean ()
  "Remove all uninteresting files from the git summary. This includes
anything whose status is `unchanged' or `updated'."
  (interactive)
  (git-error-if-updating)
  (setq git-file-list (delete-if (lambda (f)
				   (and (memq (git-file-get-index-status f)
					      '(unchanged updated))
					(memq (git-file-get-work-status f)
					      '(unchanged updated))))
				 git-file-list))
  (when (git-buffer-p)
    (summary-update)))

(defvar git-summary-functions
  (list (cons 'print git-summary-print)
	(cons 'after-move  (lambda ()
			     (goto-glyph
			      (pos git-cursor-column nil))))
	(cons 'list (lambda () git-file-list))
	(cons 'after-marking (lambda () (summary-next-item 1)))
	(cons 'select git-summary-select)
	(cons 'on-quit bury-buffer))
  "Alist of summary-mode functions for git.")


;; git commands

;; All may be called from either the *git* summary buffer, or from the
;; buffer of a file to operate on

(defun git-command-get-files ()
  "Return a list of git file structures corresponding to the files to be
operated on by the current git mode command."
  (git-error-if-updating)
  (if (git-buffer-p)
      (summary-command-items)
    ;; In a normal buffer. Try to find a git file structure for it
    (or (filter (lambda (x)
		  (file-name= (git-file-get-fullname x) (buffer-file-name)))
		git-file-list)
	;; This file isn't in the git-file-list, so make our own structure
	(list (git-make-file-struct
	       (or (local-file-name (directory-file-name *default-directory*))
		   (error "Can only run git on local files"))
	       (file-name-nondirectory (buffer-file-name))
	       (local-file-name (buffer-file-name)) 'unchanged 'unchanged)))))

(defun git-command-get-filenames ()
  "Return a list of file names corresponding to the files to be operated on
by the current git mode command."
  (mapcar (lambda (x)
	    (git-file-get-fullname x)) (git-command-get-files)))

;;;###autoload
(defun git-log (#!optional count)
  "Displays the git logs of all files."
  (interactive (list (and current-prefix-arg
			  (prefix-numeric-argument current-prefix-arg))))
  (let ((git-command-async (lambda ()
			     (git-show-output-buffer))))
    (git-command nil "log" (and count (list (format nil "-%d" count))))))

;;;###autoload
(defun git-short-log (#!optional count)
  "Displays the git logs of all selected files."
  (interactive (list (and current-prefix-arg
			  (prefix-numeric-argument current-prefix-arg))))
  (let ((git-command-async (lambda ()
			     (git-show-output-buffer))))
    (git-command nil "log" (append git-short-log-options
				   (and count
					(list (format nil "-%d" count)))))))

;;;###autoload
(defun git-file-log (#!optional count)
  "Displays the git logs of all files."
  (interactive (list (and current-prefix-arg
			  (prefix-numeric-argument current-prefix-arg))))
  (let ((git-command-async (lambda ()
			     (git-show-output-buffer))))
    (git-command nil "log" (append (and count (list (format nil "-%d" count)))
				   (cons "--" (git-command-get-filenames))))))

;;;###autoload
(defun git-short-file-log (#!optional count)
  "Displays the git logs of all selected files."
  (interactive (list (and current-prefix-arg
			  (prefix-numeric-argument current-prefix-arg))))
  (let ((git-command-async (lambda ()
			     (git-show-output-buffer))))
    (git-command nil "log" (append git-short-log-options
				   (and count (list (format nil "-%d" count)))
				   (cons "--" (git-command-get-filenames))))))

;;;###autoload
(defun git-add ()
  "Adds each selected file to the local git index. Note that this doesn't
add the files to the repository, use git-commit after calling this
command for that."
  (interactive)
  (git-command nil "add" (cons "--" (git-command-get-filenames)))
  (git-update-if-summary))

;;;###autoload
(defun git-remove (#!optional force)
  "Delete all selected files and remove them under from the local git index.
This doesn't change the repository, a subsequent call to git-commit will
do that."
  (interactive "P")
  (let ((files (git-command-get-filenames)))
    (map-y-or-n-p "Really delete file `%s'?" files delete-file)
    ;; Remove any files that the user answered negatively to
    (setq files (delete-if file-exists? files))
    (when files
      (git-command nil "rm" (if force
				(list* "-f" "--" files)
			      (cons "--" files)))
      (git-show-output-buffer)
      (git-update-if-summary))))

;;;###autoload
(defun git-commit (#!optional amend)
  "Commit the current index, after prompting for the log message to
commit them under."
  (interactive "P")
  (when amend
    (git-command nil "log" '("-1" "--pretty=%B")))
  (git-callback-with-message
   (if (not amend) "Committing files" "Amending files")
   (lambda (msg)
     (save-some-buffers)
     (let ((git-command-async (lambda ()
				(git-show-output-buffer)
				(git-update-if-summary))))
       (git-command nil "commit" (append (if amend '("--amend"))
					 (list "-m" msg)))))
   (lambda ()
     (when amend
       (insert (with-buffer (git-output-buffer)
		 (copy-area (start-of-buffer) (end-of-buffer))))))))

(defun git-revert-filenames (filenames)
  "Revert any buffers that edit a file named in the list FILENAMES. As a
special case, if a directory is named in FILENAMES, any buffers editing
files under that directory are also reverted."
  (mapc (lambda (f)
	  (if (file-directory? f)
	      ;; Try to revert _anything_ under directory F
	      (let ((canon-f (canonical-file-name
			      (file-name-as-directory f))))
		(mapc (lambda (b)
			(when (and (not (string=? (buffer-file-name b) ""))
				   (string-prefix? (canonical-file-name
						    (buffer-file-name))
						   canon-f))
			  (revert-buffer b)))
		      (buffer-list)))
	    ;; A normal file
	    (let ((b (get-file-buffer f)))
	      (when b
		(revert-buffer b)))))
	filenames))
  
(defun git-revert ()
  "Any git files whose status is `updated' or `conflict', and who are cached
locally in an editor buffer, are reverted to their on-disk versions."
  (interactive)
  (git-error-if-updating)
  (mapc (lambda (f)
	  (when (memq (git-file-get-work-status f) '(updated conflict))
	    (let ((b (get-file-buffer (git-file-get-fullname f))))
	      (when b
		(revert-buffer b))))) git-file-list))

(defun git-reset-index (#!optional all-files)
  "Reset the index copy of the currently selected files (all files if a
prefix arg is given)."
  (interactive "P")
  (git-error-if-updating)
  (git-command nil "reset" (cons "-q"
				 (if all-files
				     '()
				   (cons "--" (git-command-get-filenames)))))
  (git-update-if-summary))

;;;###autoload
(defun git-ignore ()
  "Add all currently selected git files to the list of files that should
be ignored by git. This is done by appending their names to the `.gitignore'
files in the corresponding working directories."
  (interactive)
  (mapc (lambda (cell)
	  (when (find-file (expand-file-name ".gitignore" (car cell)))
	    (goto (end-of-buffer))
	    (mapc (lambda (f)
		    (unless (re-search-forward
			     (concat #\^ (quote-regexp f) #\$)
			     (start-of-buffer))
		      (unless (zero? (pos-col (cursor-pos)))
			(insert "\n"))
		      (insert f)
		      (insert "\n")))
		  (cdr cell))
	    (save-file)
	    (bury-buffer)))
	(git-get-filenames-by-dir (git-command-get-files)))
  (git-update-if-summary))

;;;###autoload
(defun git-add-change-log-entries ()
  "Add a change log entry for each selected file. Files are grouped by
directory, the ChangeLog file associated with files in each directory
will be prompted for."
  (interactive)
  (let ((files (mapcar (lambda (cell)
			 ;; Expand the directory names so they're
			 ;; valid outside the *git* buffer
			 (cons (expand-file-name (car cell))
			       (cdr cell)))
		       (git-get-filenames-by-dir (git-command-get-files)))))
    (goto-other-view)
    (mapc (lambda (cell)
	    (add-change-log-entry
	     (prompt-for-file
	      (format nil "Log file for directory `%s':" (car cell))
	      nil (or (find-change-log-file (car cell))
		      (expand-file-name "ChangeLog" (car cell))))
	     (mapcar (lambda (f)
		       (expand-file-name f (car cell))) (cdr cell))))
	  files)))

;;;###autoload
(defun git-diff-index (#!optional all-files)
  "Compare the selected files (all files if prefix argument is set) against
the current index."
  (interactive "P")
  (save-some-buffers)
  (let ((git-command-ignore-errors t)
	(git-command-async (lambda ()
			     (git-show-output-buffer))))
    (git-command nil "diff" (if all-files
				'()
			      (git-command-get-filenames)))))

;;;###autoload
(defun git-diff-cached (#!optional all-files)
  "Compare the selected files (all files if prefix argument is set) in
the index against the repository version."
  (interactive "P")
  (save-some-buffers)
  (let ((git-command-ignore-errors t)
	(git-command-async (lambda ()
			     (git-show-output-buffer))))
    (git-command nil "diff" (cons "--cached"
				  (if all-files
				      '()
				    (git-command-get-filenames))))))

(defun git-diff-revisions (rev1 rev2)
  "Compare revisions REV1 and REV2 (or the working copy) of all selected git
files."
  (interactive
   (let* ((first (prompt-for-string "Older revision:"))
	  (second (prompt-for-string
		   (concat "Newer revision: (older: " first ")"))))
     (list first second)))
  (save-some-buffers)
  (let ((git-command-ignore-errors t)
	(git-command-async (lambda ()
			     (git-show-output-buffer))))
    (git-command nil "diff" (nconc (and rev1 (not (string=? "" rev1))
					(list (concat "-r" rev1)))
				   (and rev2 (not (string=? "" rev2))
					(list (concat "-r" rev2)))
				   (git-command-get-filenames)))))

;;;###autoload
(defun git-diff-backup ()
  "Display the differences between the currently selected git file and its
backup file (created by a merge with conflicts.)"
  (interactive)
  (save-some-buffers)
  (let ((working-file (git-command-get-filenames))
	back-file)
    (unless (eq? (cdr working-file) nil)
      (message "[Ignoring all but the first file!]" t)
      ;; Give them time to read the message..
      (sleep-for 1))
    (setq working-file (car working-file))
    ;; I wanted to use git-get-working-revisions to find the revision
    ;; number appended to the backup file; but it gets the merged rev.
    ;; So do it the rude way..
    (let* ((head (concat ".#" (file-name-nondirectory working-file) "."))
	   (possibilities (filter (lambda (f)
				    (string-prefix? f head))
				  (directory-files
				   (file-name-directory working-file)))))
      (unless possibilities
	(error "Can't find backup file"))
      (setq back-file (concat (file-name-directory working-file)
			      (car possibilities))))
    ;; Need a diff interface
    (shell-command (format nil git-diff-format-string
			   back-file working-file))))

;;;###autoload
(defun git-tag (tag-name)
  "Tag all selected git files with the string TAG-NAME."
  (interactive "sTag:")
  (git-command nil "tag" (list* tag-name "--" (git-command-get-filenames))))

;;;###autoload
(defun git-next-conflict-marker ()
  "Find the next git conflict marker in the current buffer."
  (interactive "@")
  (or (re-search-forward "^<<<<<<< ")
      (re-search-forward "^<<<<<<< " (start-of-buffer))))
