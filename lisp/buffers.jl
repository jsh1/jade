;;;; buffers.jl -- High-level buffer/file handling
;;;  Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
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

;; Variables

(defvar auto-save-p t
  "When t files are auto-save'd regularly.")

(defvar default-auto-save-interval 120
  "The number of seconds between each auto-save.")

(defvar make-backup-files t
  "When non-nil backups of files are made when they are saved.")

(defvar backup-by-copying nil
  "When non-nil all file backups are made by copying the file, not by
renaming it.")

(defvar else-backup-by-copying t
  "Non-nil means make file backups by copying the file if it's not a good
idea to rename it. If `backup-by-copying' is non-nil this variable has no
effect.")

(defvar default-buffer (current-buffer)
  "The `*jade*' buffer.")

(set! *standard-output* default-buffer)
(set! *standard-input* default-buffer)

(defvar buffer-file-modtime (cons 0 0)
  "Holds the modification time of the file this buffer was loaded from")
(make-variable-buffer-local 'buffer-file-modtime)
(put 'buffer-file-modtime 'permanent-local t)

(defvar kill-buffer-hook nil
  "Buffer-local hook called when the current buffer is killed.")
(make-variable-buffer-local 'kill-buffer-hook)

(defvar enable-local-variables t
  "Tells how to process local variable lists. t means process them
silently, nil means ignore them, anything else means to query each
variable being set.")

(defvar enable-local-eval 'maybe
  "Tells how to process the `eval' local variable. Same options as
with `enable-local-variables'.")

(defvar local-variable-lines 20
  "This variable defines how many of the bottom-most lines in a file are
searched for a `Local Variables:' section.")

;; Initialise the first window's buffer-list
(set-buffer-list (list (current-buffer)))

;; Ensure that the buffer's *default-directory* is preserved
(put '*default-directory* 'permanent-local t)


;; Buffer manipulation

(defmacro with-buffer (buffer #!rest forms)
  "Temporarily switches to buffer, then executes the FORMS in it before
returning to the original buffer."
  (list 'call-with-object buffer (list* 'lambda nil forms)))

(defun open-buffer (name #!optional always-create)
  "If no buffer called NAME exists, creates one and adds it to the main
buffer-list. Always returns the buffer. If ALWAYS-CREATE is non-nil never
return an existing buffer, always create a new one. If a new buffer has
to be created, the value of its *default-directory* variable will be
inherited from the current buffer."
  (let
      ((buf (if always-create nil (get-buffer name)))
       (old-def-dir *default-directory*))
    (unless buf
      (set! buf (make-buffer name))
      (when buf
	(with-buffer buf
	  (set! *default-directory* old-def-dir))))
    (when buf
      (add-buffer buf))
    buf))

(defun goto-buffer (buffer #!optional view)
  "Switch the current buffer to BUFFER which can either be a buffer-object
or a string naming an existing buffer. The selected buffer is moved to
the head of the buffer list. If BUFFER is a string and it doesn't name
an existing buffer a new one will be created with that name. When VIEW
is non-nil it defines the view to display the buffer in."
  (interactive "BSwitch to buffer:")
  (unless view
    (set! view (current-view)))
  (with-view view
    (when (string? buffer)
      (set! buffer (open-buffer buffer)))
    (unless (bufferp buffer)
      (signal 'bad-arg (list buffer 1)))
    (set-buffer-list (cons buffer (delq! buffer (buffer-list))))
    (set-current-buffer buffer)))

(defun kill-buffer (buffer)
  "Remove BUFFER from the `buffer-list' variable of all views. If BUFFER
is currently selected in a view, the next buffer in the view's `buffer-list'
is selected.

If BUFFER contains unsaved modifications to a file, confirmation by the
user is required before anything is done.

When called interactively, BUFFER is prompted for."
  (interactive "bBuffer to kill:")
  (cond
   ((bufferp buffer))
   ((string? buffer)
    (set! buffer (get-buffer buffer))))
  (when (and buffer (check-changes buffer))
    (with-buffer buffer
      (call-hook 'kill-buffer-hook (list buffer)))
    (mapc (lambda (w)
	    (mapc (lambda (v)
		    (with-view v
		      (set-buffer-list (delq! buffer (buffer-list)))
		      (when (eq? (current-buffer) buffer)
			(set-current-buffer (or (car (buffer-list))
						default-buffer)))))
		  (window-view-list w)))
	  (window-list))))

(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun add-buffer (buffer)
  "Make sure that BUFFER is in the `buffer-list' of all open windows. It gets
put at the end of the list if it's not already in a member."
  (mapc (lambda (w)
	  (mapc (lambda (v)
		  (unless (minibuffer-view-p v)
		    (with-view v
		      (unless (memq buffer (buffer-list))
			(set-buffer-list (append (buffer-list)
						 (cons buffer nil)))))))
		(window-view-list w)))
	(window-list)))

(defun bury-buffer (#!optional buffer all-views)
  "Puts BUFFER (or the currently displayed buffer) at the end of the current
window's buffer-list then switch to the buffer at the head of the list.
If ALL-VIEWS is non-nil this is done in all views (the same buffer will be
buried in each view though)."
  (interactive "\nP")
  (unless buffer
    (set! buffer (current-buffer)))
  (let
      ((lst (if all-views
		 (window-list)
	       (cons (current-window) nil))))
    (mapc (lambda (w)
	    (mapc (lambda (v)
		    (unless (minibuffer-view-p v)
		      (with-view v
			(set-buffer-list (append! (delq! buffer (buffer-list))
						(cons buffer nil)))
			(set-current-buffer (car (buffer-list))))))
		  (if all-views
		      (window-view-list w)
		    (list (current-view)))))
	  lst)))

(defun switch-to-buffer ()
  "Prompt the user for the name of a buffer, then display it."
  (interactive)
  (let*
      ((default (or (list-ref (buffer-list) 1) (current-buffer)))
       (buffer (prompt-for-buffer (concat "Switch to buffer (default: "
					  (buffer-name default)
					  "):")
				  nil
				  default)))
    (goto-buffer buffer)))

(defun rename-buffer (new-name)
  "Set the name of the current buffer to a uniqified instance of the string
NEW-NAME (i.e. NEW-NAME possibly with a `<N>' suffix)."
  (interactive "sNew name of buffer:")
  (set-buffer-name nil (make-buffer-name new-name)))

(defun insert-buffer (buffer #!optional p)
  "Insert the contents of BUFFER before POS (or the cursor is POS is nil)."
  (interactive "bBuffer to insert:")
  (insert (copy-area (start-of-buffer buffer)
		     (end-of-buffer buffer) buffer) p))


;; Storing files in buffers

(defun find-file (name #!optional dont-activate)
  "Find a buffer containing the file called NAME, and (unless DONT-ACTIVATE
is t) install it as the current buffer in the current view.

If no buffer containing the file NAME exists one will be created. Then
read-file-hook will be called (with arguments NAME BUFFER). If this hook
returns nil the file will be loaded into the buffer verbatim.

Once the file is in memory, through the hook or otherwise, the init-mode
function is called to initialise the correct editing mode for the file.

find-file always returns the buffer holding file NAME, or nil if no
such buffer could be made."
  (interactive "FFind file:")
  (set! name (expand-file-name name))
  (when (file-directory? name)
    (set! name (directory-file-name name)))
  (let
      ((buf (get-file-buffer name)))
    (if buf
	;; Buffer already exists; check that it's up to date
	(with-buffer buf
	  (when (and (file-exists? name)
		     (> (file-modtime name) buffer-file-modtime)
		     (yes-or-no-p "File on disk has changed; revert buffer?"))
	    ;; it's not, so reread it
	    (revert-buffer)))
      ;; No buffer exists
      (set! buf (call-hook 'find-file-hook (list name) 'or))
      (unless buf
	;; find-file-hook didn't; do keep going
	(let ((b-name (file-name-nondirectory name)))
	  (when (string=? b-name "")
	    (set! b-name (file-name-nondirectory (directory-file-name name))))
	  (set! buf (open-buffer (file-name-nondirectory b-name) t)))
	(with-buffer buf
	  (read-file-into-buffer name))))
    (unless dont-activate
      (goto-buffer buf))
    buf))

(defun read-file-into-buffer (file-name)
  "Reads the file FILE-NAME into the current buffer, overwriting anything
else in the buffer. Everything will be set up as required. Before calling
normal-mode, the hook after-read-file-hook is dispatched."
  (interactive "fFile to read into buffer:")
  (let ((buf (current-buffer)))
    (clear-buffer)
    (set! file-name (expand-file-name file-name))
    (unless (call-hook 'read-file-hook (list file-name buf) 'or)
      (set! *default-directory* (file-name-directory file-name))
      (set-buffer-file-name buf file-name)
      (if (file-exists? file-name)
	  (progn
	    (read-file-contents file-name)
	    (set! buffer-file-modtime (file-modtime file-name)))
	(message "New file")))
    (hack-local-variables)
    (set-buffer-modified buf nil)
    (when auto-save-p
      (auto-save-interval default-auto-save-interval))
    (last-save-time (current-time))
    (set-buffer-undo-list nil)
    (when (auto-save-file-newer-p file-name)
      (message "Warning: Auto-saved file is newer")
      (beep))
    (set-buffer-read-only buf (and (file-exists? file-name)
				   (not (file-writable? file-name))))
    (call-hook 'after-read-file-hook (list buf))
    (normal-mode)))

;; Scans the end of a file for any local-variable definitions
(defun hack-local-variables ()
  (when enable-local-variables
    ;; extract variables from -*- MODE; VAR: VALUE; ... -*- string
    (let ((text (and (looking-at
		      ".*-\\*-\\s*(.+?)\\s*-\\*-" (start-of-buffer))
		     (expand-last-match "\\1"))))
      (when (and text (> (string-length text) 0))
	(let ((pieces (string-split "\\s*;\\s*" text)))
	  (when pieces
	    (mapc (lambda (x)
		    (when (string-looking-at "(\\S+)\\s*:\\s*(\\S+)" x)
		      (let ((var (intern (expand-last-match "\\1")))
			    (value (expand-last-match "\\2")))
			(when (or (eq? enable-local-variables t)
				  (y-or-n-p (format nil "Set %s to %s?"
						    var value)))
			  (make-local-variable var)
			  (let ((value (read-from-string value)))
			    ;; since we're not evaluating, inline
			    ;; things that eval to ().
			    (variable-set! var (case value
				       ((nil #f) nil)
				       (t value))))))))
		  pieces)))))
    (let
	((p (pos 0 (- (buffer-length) local-variable-lines))))
      (when (< (pos-line p) 0)
	(set! p (start-of-buffer)))
      (when (re-search-forward "^(.*)Local Variables:(.*)$" p nil t)
	(let
	    ((re (concat #\^ (quote-regexp (copy-area (match-start 1)
						     (match-end 1)))
			 "([^:\n]+):[\t ]*(.*)"
			 (quote-regexp (copy-area (match-start 2)
						  (match-end 2))) #\$))
	     name value finished)
	  (set! p (match-end))
	  (while (and (not finished) (re-search-forward re p))
	    (set! p (match-end))
	    (set! name (copy-area (match-start 1) (match-end 1)))
	    (set! value (copy-area (match-start 2) (match-end 2)))
	    (cond
	     ((and (equal? name "End") (equal? value ""))
	      (set! finished t))
	     ((equal? name "mode")
	      (when (or (eq? enable-local-variables t)
			(y-or-n-p (format nil "Use major mode %s?" value)))
		(set! mode-name value)))
	     ((equal? name "eval")
	      (when (and enable-local-eval
			 (or (eq? enable-local-eval t)
			     (y-or-n-p (format nil "Eval `%s'?" value))))
		(eval (read-from-string value))))
	     (t
	      (when (or (eq? enable-local-variables t)
			(y-or-n-p (format nil "Set %s to %s?" name value)))
		(set! name (intern name))
		(make-local-variable name)
		(variable-set! name (read-from-string value)))))))))))

(defun find-file-read-only (name #!optional dont-activate)
  "Similar to `find-file' except that the buffer is edited in read-only mode."
  (interactive "FFind file read-only:")
  (let
      ((buffer (find-file name dont-activate)))
    (when buffer
      (set-buffer-read-only buffer t))
    buffer))

(defun find-alternate-file (name)
  "If NAME is unspecified one will be prompted for. The current buffer is
killed and one editing NAME is found."
  (interactive "FFind alternate file:")
  (kill-buffer (current-buffer))
  (find-file name))

(defun make-backup-file-name (file-name)
  "Returns the name of the file used to store the backup of FILE-NAME."
  (concat file-name #\~))

(defun backup-file (name)
  "If necessary make a backup of NAME. The file called NAME may or
may not exist after this function returns."
  (when (and make-backup-files (file-regular? name))
    (let
	((backup-name (make-backup-file-name name)))
      (if backup-by-copying
	  (copy-file name backup-name)
	(if (and (file-owner? name)
		 (= (file-nlinks name) 1))
	    (progn
	      (when (file-exists? backup-name)
		(delete-file backup-name))
	      (rename-file name backup-name))
	  (when else-backup-by-copying
	    (copy-file name backup-name)))))))

(defun write-file (buffer #!optional name)
  "Writes the contents of BUFFER to the file NAME, or to the one
that it is associated with."
  (or (string? name)
      (set! name (buffer-file-name buffer))
      (error "No file is associated with buffer: %s" buffer))
  (unless (call-hook 'write-file-hook (list name buffer) 'or)
    (let
	((modes (when (file-exists? name) (file-modes name))))
      (with-buffer buffer
	(backup-file name))
      (when (with-buffer buffer
	      (write-buffer-contents name))
	(when modes
	  (set-file-modes name modes))
	t))))

(defun save-file (#!optional buffer force)
  "Saves the buffer BUFFER, or the current buffer, to the file that it is
associated with, then sets the number of modifications made to this file
to zero. If no changes have been made to the buffer, it won't be saved
(unless the optional FORCE parameter is non-nil; this is taken from the
prefix-argument when the function is called interactively)."
  (interactive "\nP")
  (unless (bufferp buffer)
    (set! buffer (current-buffer)))
  (with-buffer buffer
    (if (and (not force) (not (buffer-modified-p)))
	(message "No changes need to be saved!")
      (let
	  ((name (buffer-file-name)))
	(unless name
	  (error "Buffer has no file associated with it: %s" buffer))
	(when (and (file-exists? name)
		   (> (file-modtime name) buffer-file-modtime)
		   (not (yes-or-no-p "File on disk has changed since it was loaded, save anyway")))
	  (error "Save aborted"))
	(when (write-file buffer)
	  (set-buffer-modified buffer nil)
	  (last-save-changes (buffer-changes))
	  (last-user-save-changes (buffer-changes))
	  (last-save-time (current-time))
	  (set! buffer-file-modtime (file-modtime name))
	  (delete-auto-save-file)
	  (message (concat "Wrote file `" name #\') t))))))

(defun save-file-as (name #!optional buffer)
  "Saves the buffer BUFFER, or the current one, to the file NAME,
resetting the name of the buffer and the file that it is associated with
to reflect NAME. Also sets the modification count to zero."
  (interactive "FWrite file:")
  (unless (bufferp buffer)
    (set! buffer (current-buffer)))
  (let
      ((tem (get-file-buffer (expand-file-name name))))
    (when (and tem (not (eq? tem buffer)))
      ;; Doing the save would overwrite this buffer
      (or (check-changes tem)
	  (error "Saving file would overwrite buffer %s" tem))
      (when (y-or-n-p (format nil "Delete duplicate buffer %s?" tem))
	(kill-buffer tem))))
  (with-buffer buffer
    (set-buffer-file-name buffer (expand-file-name name))
    (set-buffer-name buffer (file-name-nondirectory name))
    (when (write-file buffer)
      (set-buffer-modified buffer nil)
      (last-save-changes (buffer-changes))
      (last-user-save-changes (buffer-changes))
      (last-save-time (current-time))
      (set! buffer-file-modtime (file-modtime name))
      (set! *default-directory* (file-name-directory name))
      (delete-auto-save-file)
      (message (concat "Wrote file `" name #\') t))))

(defun insert-file (name #!optional buffer)
  "Inserts the file NAME into the buffer BUFFER (or the current one) before
the cursor position."
  (interactive "FInsert file:")
  (unless (bufferp buffer)
    (set! buffer (current-buffer)))
  (with-buffer buffer
    (unless (call-hook 'insert-file-hook (list name) 'or)
      (insert-file-contents name))))

(defun check-changes (#!optional buffer)
  "Returns t if it is ok to lose the current contents of BUFFER, or the
current buffer. If unsaved changes have been made to it the user is asked
whether they mind losing them."
  (or (not (buffer-modified-p buffer))
      (null? (buffer-file-name buffer))
      (yes-or-no-p (format nil "OK to lose change(s) to buffer `%s'"
			   (file-name-nondirectory (buffer-name buffer))))))

(defun revert-buffer (#!optional buffer force)
  "Restores the contents of BUFFER (or current buffer) to the contents of the
file it was loaded from. Unless FORCE is t, unsaved modifications will only
be lost after confirmation from the user."
  (interactive)
  (unless buffer
    (set! buffer (current-buffer)))
  (unless (buffer-file-name buffer)
    (error "No file is associated with buffer: %s" buffer))
  (when (or force (check-changes buffer))
    (with-buffer buffer
      (delete-auto-save-file)
      (let*
	  ((old-pos (cursor-pos))
	   (context-pre (max 0 (1- (pos-line old-pos))))
	   (context-post (min (1- (buffer-length)) (1+ (pos-line old-pos))))
	   (context (copy-area (pos 0 context-pre)
			       (end-of-line (pos 0 context-post))))
	   (window-line (pos-line (char-to-display-pos old-pos))))
	(clear-buffer)
	(when major-mode-kill
	  (major-mode-kill))
	(kill-all-local-variables)
	(read-file-into-buffer (buffer-file-name buffer))
	;; Try to restore the cursor to it's original position
	(goto (min old-pos (end-of-buffer)))
	(let*
	    ((match-pre (max 0 (1- (pos-line (cursor-pos))))))
	  (unless (buffer-compare-string context match-pre)
	    ;; First, try to search for this string.
	    (when (or (search-forward context match-pre)
		      (search-backward context match-pre))
	      (goto (pos (pos-col old-pos)
			 (1+ (pos-line (match-start)))))))
	  ;; Assuming we found the text, now try to recenter the view as
	  ;; it was originally
	  (center-display nil window-line))))))

(defun revert-buffers ()
  "Asks whether to revert each buffer associated with a file."
  (interactive)
  (let ((buffers (filter buffer-file-name (buffer-list))))
    (if buffers
	(map-y-or-n-p (lambda (x)
			(format nil "Revert buffer %s" (buffer-name x)))
		      buffers revert-buffer)
      (message "[No buffers to revert]"))))

(defun save-some-buffers ()
  "Asks whether or not to save any modified buffers, returns t if no modified
buffers exist on exit."
  (interactive)
  (let
      ((unsaved-buffers (filter (lambda (b)
				  (and (buffer-modified-p b)
				       (buffer-file-name b)))
				(buffer-list))))
    (if unsaved-buffers
	(map-y-or-n-p (lambda (x)
			(format nil "Save file %s" (buffer-file-name x)))
		      unsaved-buffers
		      save-file)
      (message "[No modified buffers]"))))

(defun save-some-files (filenames)
  "Asks whether or not to save buffers associated with the list of filenames.
Returns t if none of the files were left modified."
  (interactive)
  (let ((unsaved-buffers
	 (delete-if! (lambda (b)
		      (not (and b (buffer-modified-p b))))
		    (mapcar get-file-buffer filenames))))
    (if unsaved-buffers
	(map-y-or-n-p (lambda (x)
			(format nil "Save file %s" (buffer-file-name x)))
		      unsaved-buffers
		      save-file)
      (message "[No buffers need saving]"))))

(defun maybe-save-buffer (#!optional buffer)
  "If BUFFER has been modified, ask whether or not to save it. Returns t if
the buffer is (now) in sync with the copy on disk."
  (or (not (buffer-modified-p buffer))
      (and (buffer-file-name buffer)
	   (y-or-n-p (concat "Save buffer " (buffer-name buffer)))
	   (save-file buffer))))


;; Auto saving

(defun make-auto-save-name (name)
  "Returns a string naming the file used to hold the auto-save'd file for
file NAME."
  (concat (file-name-directory name) #\# (file-name-nondirectory name) #\#))

(defun auto-save-function (buffer)
  "Automatically called when BUFFER is due to be automatically saved.
This function calls the hook `auto-save-hook', if this returns nil it then
saves it to the file specified by `make-auto-save-name' appiled to the
name of the file stored in BUFFER."
  (message (concat "Auto-saving `" (buffer-name buffer) "'...") t)
  (with-buffer buffer
    (if (or (call-hook 'auto-save-hook (list buffer) 'or)
	    (write-buffer-contents (make-auto-save-name (buffer-file-name))))
	(message (concat "Auto-saving `" (buffer-name buffer) "'...done"))
      (error "Can't auto-save" buffer)
      nil)))

(defun delete-auto-save-file (#!optional buffer)
  "Deletes the file used to store the auto-save'd copy of the file stored in
BUFFER, if such a file exists."
  (interactive)
  (let
      ((a-name (make-auto-save-name (buffer-file-name buffer))))
    (when (file-exists? a-name)
      (delete-file a-name))))

(defun auto-save-file-newer-p (name)
  "Returns t if there exists an automatically saved copy of file NAME which
is newer than NAME."
  (let
      ((t1 (file-modtime (make-auto-save-name name)))
       (t2 (file-modtime name)))
    (and t1 t2 (> t1 t2))))

(defun auto-save-mode (#!optional disable)
  "When this mode is enabled files are autosaved regularly if they've been
modified."
  (interactive "P")
  (if (or (/= (auto-save-interval) 0)
	  disable)
      (progn
	(auto-save-interval 0)
	(message "Auto-save is now disabled in this buffer."))
    (auto-save-interval default-auto-save-interval)
    (message "Auto-save is now enabled for this buffer.")))

(defun recover-file (#!optional buffer)
  "Loads the auto-saved copy of the file stored in BUFFER into BUFFER
overwriting its current contents (if any changes are to be lost the user
will have to agree to this)."
  (interactive)
  (let
      ((recover-name (make-auto-save-name (buffer-file-name buffer))))
    (unless buffer
      (set! buffer (current-buffer)))
    (when (and (file-exists? recover-name) (check-changes buffer))
      (with-buffer buffer
	(read-file-contents recover-name)
	(set-buffer-modified buffer t)
	(last-save-time (current-time))
	(message (concat "Using " recover-name " as "
			 (buffer-file-name buffer)))))
    buffer))


;; Misc

(defun save-and-quit (#!optional no-query)
  "Exit the editor. Unless NO-QUERY is non-nil, ask the user whether or
not to save any buffers with outstanding modifications. When NO-QUERY is
numeric it's used as the exit status of the editor process."
  (interactive "P")
  (when (or no-query
	    (save-some-buffers)
	    (yes-or-no-p "Unsaved buffers exist; quit anyway?"))
    ;; we want *before-exit-hook* to run while the top-level recursive
    ;; edit is still present (otherwise things stop working..)
    (condition-case nil
	(call-hook '*before-exit-hook*)
      (error))
    (set! *before-exit-hook* nil)
    (throw 'quit (if (number? no-query) no-query 0))))

(defun buffer-read-only-p (#!optional buffer)
  (condition-case nil
      (buffer-variable-ref 'read-only (extent-root buffer) nil)
    (error)))

(defun set-buffer-read-only (buffer status)
  (with-buffer (or buffer (current-buffer))
    (set! read-only status)))
