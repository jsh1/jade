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

(defvar standard-output default-buffer
  "Stream that `prin?' writes its output to by default")

(defvar standard-input default-buffer
  "Stream that `read' takes it's input from by default")

(defvar buffer-file-modtime (cons 0 0)
  "Holds the modification time of the file this buffer was loaded from")
(make-variable-buffer-local 'buffer-file-modtime)

(defvar mildly-special-buffer nil
  "When a buffer's `special' attribute is set kill-buffer will only kill
it totally if this variable is non-nil.")
(make-variable-buffer-local 'mildly-special-buffer)

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

(defvar before-exit-hook nil
  "Hook called immediately prior to the editor finishing execution.")

;; Initialise the first window's buffer-list
(setq buffer-list (list (current-buffer)))


;; Buffer manipulation

(defun open-buffer (name &optional always-create)
  "If no buffer called NAME exists, creates one and adds it to the main
buffer-list. Always returns the buffer. If ALWAYS-CREATE is non-nil never
return an existing buffer, always create a new one."
  (let
      ((buf (if always-create nil (get-buffer name))))
    (unless buf
      (when (setq buf (make-buffer name))
	(add-buffer buf)))
    buf))

(defun goto-buffer (buffer &optional view)
  "Switch the current buffer to BUFFER which can either be a buffer-object
or a string naming an existing buffer. The selected buffer is moved to
the head of the buffer list. If BUFFER is a string and it doesn't name
an existing buffer a new one will be created with that name. When VIEW
is non-nil it defines the view to display the buffer in."
  (interactive "BSwitch to buffer")
  (unless view
    (setq view (current-view)))
  (with-view view
    (when (stringp buffer)
      (setq buffer (open-buffer buffer)))
    (unless (bufferp buffer)
      (signal 'bad-arg (list buffer 1)))
    (setq buffer-list (cons buffer (delq buffer buffer-list)))
    (set-current-buffer buffer)))

(defun kill-buffer (buffer)
  "Destroys BUFFER (can be an actual buffer or name of a buffer), first
checks whether or not we're allowed to with the function `check-changes'.
  If it can be deleted, all windows displaying this buffer are switched
to the buffer at the head of the buffer-list, and BUFFER is removed
from the buffer-list (if it was in it)."
  (interactive "bBuffer to kill:")
  (cond
   ((bufferp buffer))
   ((stringp buffer)
    (setq buffer (get-buffer buffer))))
  (when (and buffer (check-changes buffer))
    (eval-hook 'kill-buffer-hook buffer)
    (unless (and (buffer-special-p buffer)
		 (null (with-buffer buffer mildly-special-buffer)))
      (kill-mode buffer)
      (destroy-buffer buffer))
    (remove-buffer buffer)
    t))

(defun bury-buffer (&optional buffer all-windows)
  "Puts BUFFER (or the currently displayed buffer) at the end of the current
window's buffer-list then switch to the buffer at the head of the list.
If ALL-WINDOWS is non-nil this is done in all windows (the same buffer
will be buried in each window though)."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (let
      ((list (if all-windows
		 window-list
	       (cons (current-window) nil))))
    (while list
      (with-window (car list)
	(let
	    ((old-list (copy-sequence buffer-list)))
	  (setq buffer-list (nconc (delq buffer buffer-list)
				   (cons buffer nil)))
	  (set-current-buffer (car buffer-list))
	  ;; It seems that buffer-list sometimes?
	  (when (/= (length buffer-list) (length old-list))
	    (error "buffer-list changed length!"))))
      (setq list (cdr list)))))

(defun switch-to-buffer ()
  "Prompt the user for the name of a buffer, then display it."
  (interactive)
  (let*
      ((default (or (nth 1 buffer-list) (current-buffer)))
       (buffer (prompt-for-buffer (concat "Switch to buffer (default: "
					  (buffer-name default)
					  "):")
				  nil
				  default)))
    (goto-buffer buffer)))

(defun rotate-buffers-forward ()
  "Moves the buffer at the head of the buffer-list to be last in the list, the
new head of the buffer-list is displayed in the current window."
  (interactive)
  (let
      ((head (car buffer-list))
	(end (nthcdr (1- (length buffer-list)) buffer-list)))
    (rplacd end (cons head nil))
    (setq buffer-list (cdr buffer-list))
    (set-current-buffer (car buffer-list))))

;(defun rotate-buffers-backward (&aux end)
;  "(rotate-buffers-backward)
;Moves the buffer at the end of the buffer-list to be first in the list, the
;new head of the buffer-list is displayed in the current window."
;  (setq
;    end (nthcdr (- 2 (length buffer-list)) buffer-list)
;    buffer-list (cons (last buffer-list) buffer-list))
;  (rplacd end nil)
;  (set-current-buffer (car buffer-list)))


;; Storing files in buffers

(defun open-file (name)
  "If no buffer containing file NAME exits try to create one.
After creating a new buffer (named after the file's (not path) name)
it first call the hook `read-file-hook' with arguments `(buffer-file-name
buffer)'.
If this hook returns nil (ie, no members of the hook decided to read the
file into memory) the file is read into the buffer verbatim.\n
Once the file is in memory, through the hook or otherwise, this function
then tries to initialise the correct editing mode for the file.\n
`open-file' always returns the buffer holding the file, or nil if it
doesn't exist."
  (let
      ((buf (get-file-buffer name)))
    (unless buf
      (when (setq buf (make-buffer (file-name-nondirectory name)))
	(add-buffer buf buffer-list)
	(with-buffer buf
	  (read-file-into-buffer name))))
    buf))

(defun read-file-into-buffer (file-name)
  "Reads the file FILE-NAME into the current buffer, overwriting anything
else in the buffer. Everything will be set up as required."
  (interactive "fFile to read into buffer:")
  (let ((buf (current-buffer)))
    (clear-buffer)
    (unless (eval-hook 'read-file-hook file-name buf)
      (set-buffer-file-name buf file-name)
      (if (file-exists-p file-name)
	  (progn
	    (read-buffer file-name)
	    (setq buffer-file-modtime (file-modtime file-name)))
	(message "New file")))
    (fix-local-variables)
    (set-buffer-modified buf nil)
    (when auto-save-p
      (setq auto-save-interval default-auto-save-interval))
    (setq last-save-time (current-time)
	  buffer-undo-list nil)
    (when (auto-save-file-newer-p file-name)
      (message "Warning: Auto-saved file is newer")
      (beep))
    (set-buffer-read-only buf (and (file-exists-p file-name)
				   (not (file-writable-p file-name))))
    (eval-hook 'open-file-hook buf)
    (init-mode buf)))

;; Scans the end of a file for any local-variable definitions
(defun fix-local-variables ()
  (unless enable-local-variables
    (return))
  (let
      ((pos (pos 0 (- (buffer-length) local-variable-lines))))
    (when (< (pos-line pos) 0)
      (setq pos (start-of-buffer)))
    (when (re-search-forward "^(.*)Local Variables:(.*)$" pos nil t)
      (let
	  ((re (concat ?^
		       (quote-regexp (copy-area (match-start 1) (match-end 1)))
		       "([^:\n]+):[\t ]*(.*)"
		       (quote-regexp (copy-area (match-start 2) (match-end 2)))
		       ?$))
	   name value)
	(setq pos (match-end))
	(while (re-search-forward re pos)
	  (setq pos (match-end)
		name (copy-area (match-start 1) (match-end 1))
		value (copy-area (match-start 2) (match-end 2)))
	  (cond
	   ((and (equal name "End") (equal value ""))
	    (return))
	   ((equal name "mode")
	    (when (or (eq enable-local-variables t)
		      (y-or-n-p (format nil "Use major mode %s?" value)))
	      (setq mode-name value)))
	   ((equal name "eval")
	    (when (and enable-local-eval
		       (or (eq enable-local-eval t)
			   (y-or-n-p (format nil "Eval `%s'?" value))))
	      (eval (read-from-string value))))
	   (t
	    (when (or (eq enable-local-variables t)
		      (y-or-n-p (format nil "Set %s to %s?" name value)))
	      (setq name (intern name))
	      (make-local-variable name)
	      (set name (read-from-string value))))))))))

(defun find-file (name)
  "Sets the current buffer to that containing the file NAME, if NAME
is unspecified it will be prompted for. If the file is not already in memory
`open-file' will be used to load it."
  (interactive "FFind file: ")
  (goto-buffer (open-file name)))

(defun find-file-read-only (name)
  "Similar to `find-file' except that the buffer is edited in read-only mode."
  (interactive "FFind file read-only:")
  (let
      ((buf (open-file name)))
    (when buf
      (set-buffer-read-only buf t)
      (goto-buffer buf))))

(defun find-alternate-file (name)
  "If NAME is unspecified one will be prompted for. The current buffer is
killed and one editing NAME is found."
  (interactive "FFind alternate file:")
  (kill-buffer (current-buffer))
  (goto-buffer (open-file name)))

(defun backup-file (file-name)
  "If necessary make a backup of FILE-NAME. The file called FILE-NAME may or
may not exist after this function returns."
  (when (and make-backup-files (file-regular-p name))
    (let
	((backup-name (concat name ?~)))
      (if backup-by-copying
	  (copy-file name backup-name)
	(if (and (file-owner-p name)
		 (= (file-nlinks name) 1))
	    (progn
	      (when (file-exists-p backup-name)
		(delete-file backup-name))
	      (rename-file name backup-name))
	  (when else-backup-by-copying
	    (copy-file name backup-name)))))))

(defun write-file (buffer &optional name)
  "Writes the contents of BUFFER to the file NAME, or to the one
that it is associated with."
  (unless (stringp name)
    (setq name (buffer-file-name buffer)))
  (unless (eval-hook 'write-file-hook name buffer)
    (let
	((modes (when (file-exists-p name) (file-modes name))))
      (backup-file name)
      (when (write-buffer name buffer)
	(when modes
	  (set-file-modes name modes))
	t))))

(defun save-file (&optional buffer &aux name)
  "Saves the buffer BUFFER, or the current buffer, to the file that it is
associated with, then sets the number of modifications made to this file
to zero. If no changes have been made to the buffer, it won't be saved."
  (interactive)
  (unless (bufferp buffer)
    (setq buffer (current-buffer)))
  (with-buffer buffer
    (if (not (buffer-modified-p))
	(message "No changes need to be saved!")
      (let
	  ((name (buffer-file-name)))
	(when (and
	       (time-later-p (file-modtime name) buffer-file-modtime)
	       (not (yes-or-no-p "File on disk has changed since it was loaded, save anyway")))
	  (return nil))
	(when (write-file buffer)
	  (set-buffer-modified buffer nil)
	  (setq last-save-time (current-time)
		last-save-changes (buffer-changes)
		last-user-save-changes (buffer-changes)
		buffer-file-modtime (file-modtime name))
	  (delete-auto-save-file)
	  (message (concat "Wrote file `" name ?\') t))))))

(defun save-file-as (name &optional buffer)
  "Saves the buffer BUFFER, or the current one, to the file NAME,
resetting the name of the buffer and the file that it is associated with
to reflect NAME. Also sets the modification count to zero."
  (interactive "FWrite file:")
  (unless (bufferp buffer)
    (setq buffer (current-buffer)))
  (with-buffer buffer
    (set-buffer-file-name buffer name)
    (set-buffer-name buffer (file-name-nondirectory name))
    (when (write-file buffer)
      (set-buffer-modified buffer nil)
      (setq last-save-time (current-time)
	    last-save-changes (buffer-changes)
	    last-user-save-changes (buffer-changes)
	    buffer-file-modtime (file-modtime name))
      (delete-auto-save-file)
      (format t "Saved file `%s'." name))))

(defun insert-file (name &optional buffer)
  "Inserts the file NAME into the buffer BUFFER (or the current one) before
the cursor position."
  (interactive "FInsert file:")
  (unless (bufferp buffer)
    (setq buffer (current-buffer)))
  (with-buffer buffer
    (unless (eval-hook 'insert-file-hook name)
      (insert (read-file name)))))

(defun check-changes (&optional buffer)
  "Returns t if it is ok to kill BUFFER, or the current buffer. If unsaved
changes have been made to it the user is asked whether they mind losing
them."
  (or (not (buffer-modified-p buffer))
      (yes-or-no-p (format nil "OK to lose change(s) to buffer `%s'"
			   (file-name-nondirectory (buffer-name buffer))))))

(defun revert-buffer (&optional buffer)
  "Restores the contents of BUFFER (or current buffer) to the contents of the
file it was loaded from."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (when (check-changes buffer)
    (with-buffer buffer
      (delete-auto-save-file)
      (read-file-into-buffer (buffer-file-name buffer)))))

(defun save-some-buffers ()
  "Asks whether or not to save any modified buffers, returns t if no modified
buffers exist on exit."
  (interactive)
  (let
      ((bufs buffer-list)
       buf
       (unsaved-files-p nil))
    (while (consp bufs)
      (setq buf (car bufs))
      (when (and (buffer-modified-p buf) (not (buffer-special-p buf)))
	(if (y-or-n-p (concat "Save buffer " (buffer-name buf)))
	    (unless (save-file buf)
	      (setq unsaved-files-p t))
	  (setq unsaved-files-p t)))
      (setq bufs (cdr bufs)))
    (not unsaved-files-p)))

(defun maybe-save-buffer (buffer)
  "If BUFFER has been modified, ask whether or not to save it."
  (when (and (buffer-modified-p buffer) (not (buffer-special-p buffer)))
    (if (y-or-n-p (concat "Save buffer " (buffer-name buffer)))
	(unless (save-file buffer)
	  (setq unsaved-files-p t))
      (setq unsaved-files-p t))))


;; Auto saving

(defun make-auto-save-name (name)
  "Returns a string naming the file used to hold the auto-save'd file for
file NAME."
  (concat (file-name-directory name) ?# (file-name-nondirectory name) ?#))

(defun auto-save-function (buffer)
  "Automatically called when BUFFER is due to be automatically saved.
This function calls the hook `auto-save-hook', if this returns nil it then
saves it to the file specified by `make-auto-save-name' appiled to the
name of the file stored in BUFFER."
  (format t "Auto-saving `%s'..." (buffer-name buffer))
  (refresh-all)
  (flush-output)
  (with-buffer buffer
    (if (or (eval-hook 'auto-save-hook buffer)
	    (write-buffer (make-auto-save-name (buffer-file-name))))
	(format t "done.")
      (error "Can't auto-save" buffer)
      nil)))

(defun delete-auto-save-file (&optional buffer)
  "Deletes the file used to store the auto-save'd copy of the file stored in
BUFFER, if such a file exists."
  (interactive)
  (let
      ((a-name (make-auto-save-name (buffer-file-name buffer))))
    (when (file-exists-p a-name)
      (delete-file a-name))))

(defun auto-save-file-newer-p (name)
  "Returns t if there exists an automatically saved copy of file NAME which
is newer than NAME."
  (let
      ((recover-name (make-auto-save-name name)))
    (time-later-p (file-modtime recover-name) (file-modtime name))))

(defun auto-save-mode (&optional disable)
  "When this mode is enabled files are autosaved regularly if they've been
modified."
  (interactive "P")
  (if (or (/= auto-save-interval 0)
	  disable)
      (progn
	(setq auto-save-interval 0)
	(message "Auto-save is now disabled in this buffer."))
    (setq auto-save-interval default-auto-save-interval)
    (message "Auto-save is now enabled for this buffer.")))

(defun recover-file (&optional buffer)
  "Loads the auto-saved copy of the file stored in BUFFER into BUFFER
overwriting its current contents (if any changes are to be lost the user
will have to agree to this)."
  (interactive)
  (let
      ((recover-name (make-auto-save-name (buffer-file-name buffer))))
    (unless buffer
      (setq buffer (current-buffer)))
    (when (and (file-exists-p recover-name) (check-changes buffer))
      (with-buffer buffer
	(read-buffer recover-name)
	(set-buffer-modified buffer t)
	(setq last-save-time (current-time))
	(message (concat "Using " recover-name " as "
			 (buffer-file-name buffer)))))
    buffer))


;; Marks

(defun goto-mark (mark)
  "Switches (if necessary) to the buffer containing MARK at the position
of the mark. If the file containing MARK is not in memory then we
attempt to load it with `open-file'."
  (when (markp mark)
    (let
	((file (mark-file mark))
	 (pos (mark-pos mark)))
      (when (stringp file)
	(setq file (open-file file)))
      (set-auto-mark)
      (goto-buffer file)
      (goto pos))))

(defun set-auto-mark ()
  "Sets the mark `auto-mark' to the current position (buffer & cursor-pos)."
  (interactive)
  (set-mark auto-mark (cursor-pos) (current-buffer))
  (message "Set auto-mark."))

(defun swap-cursor-and-auto-mark ()
  "Sets the `auto-mark' to the current position and then sets the current
position (buffer and cursor-pos) to the old value of `auto-mark'."
  (interactive)
  (let
      ((a-m-file (mark-file auto-mark))
       (a-m-pos (mark-pos auto-mark)))
    (set-auto-mark)
    (when (stringp a-m-file)
      (setq a-m-file (open-file a-m-file)))
    (goto-buffer a-m-file)
    (goto a-m-pos)))


;; Misc

(defun file-newer-than-file-p (file1 file2)
  "Returns t when FILE1 was modified more recently than FILE2."
  (time-later-p (file-modtime file1) (file-modtime file2)))

(defun save-and-quit (&optional no-query)
  "Exit the editor. Unless NO-QUERY is non-nil, ask the user whether or
not to save any buffers with outstanding modifications. When NO-QUERY is
numeric it's used as the exit status of the editor process.
Immediately prior to exiting, calls `before-exit-hook'."
  (interactive "P")
  (when (or no-query
	    (save-some-buffers)
	    (yes-or-no-p "Unsaved buffers exist; quit anyway?"))
    (eval-hook 'before-exit-hook)
    (throw 'quit (if (numberp no-query) no-query 0))))
