;;;; rcs.jl -- RCS interface for Jade
;;;  Copyright (C) 1997 John Harper <john@dcs.warwick.ac.uk>
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

;;; TODO:
;;;  - command to convert log entries to Changelog entries
;;;  - change into generic version control, with backends for RCS, CVS,
;;;    SCCS, PRCS, ...

(require 'ring)
(require 'rcs-hooks)
(provide 'rcs)


;; User options

(defvar rcs-initial-comment nil
  "When non-nil ask for a string describing the file being registered,
when initialising RCS files.")

(defvar rcs-make-backup-files nil
  "When nil RCS controlled files will never have backups created when they
are saved.")

(defvar rcs-only-lock-seen-files t
  "When this variable is t, doing `Ctrl-u Ctrl-x Ctrl-q' on an unlocked
buffer, for a revision different to the currently displayed revision, will
cause the new revision _not_ to be locked, only checked out. A successive
`Ctrl-x Ctrl-q' will lock it.")

(defvar rcs-display-log-args '()
  "A list of extra arguments to be passed to the RCS rlog command.")


;; Program variables

(defvar rcs-output-buffer (make-buffer "*rcs*")
  "Buffer used for output from RCS commands.")
(set-buffer-special rcs-output-buffer t)

(defvar rcs-process (make-process rcs-output-buffer)
  "Process object used to run RCS commands.")

(defvar rcs-descr-ring (make-ring 16)
  "Ring buffer containing RCS history entries.")

(defvar rcs-callback-ctrl-c-keymap (make-keylist)
  "Keymap for Ctrl-C when entering text for a callback.")
(defvar rcs-callback-keymap (make-keylist)
  "Keymap for callback buffer.")
(make-variable-buffer-local 'rcs-current-info-string)

(defvar rcs-controlled-buffer nil
  "Local variable that is t when the buffer is controlled by RCS.")
(make-variable-buffer-local 'rcs-controlled-buffer)

(defvar rcs-revision nil
  "Local variable storing the revision number of buffer's controlled by RCS,
as a string. May be nil if revision is unknown.")
(make-variable-buffer-local 'rcs-revision)

(unless (boundp 'rcs-initialised)
  ;; Only do this stuff once
  (bind-keys rcs-callback-ctrl-c-keymap
    "Ctrl-c" 'rcs-call-callback)
  (bind-keys rcs-callback-keymap
    "Meta-p" 'rcs-down-history
    "Meta-n" 'rcs-up-history)
  (setq rcs-initialised t))

(defvar rcs-callback-args nil
  "Arguments stored for when `Ctrl-c Ctrl-c' is typed in the callback buffer.
A list, (FILE CALLBACK-BUFFER COMMAND OPTIONS TEXT-PREFIX REREAD).")
(make-variable-buffer-local 'rcs-callback-args)

(defvar rcs-history-level nil
  "Depth of last inserted history item")
(make-variable-buffer-local 'rcs-history-level)


;; Internal functions

;; Run the RCS shell command COMMAND on FILE-NAME, using the list of
;; options OPTIONS. If REREAD-BUFFER-P is t the current buffer will
;; be reinitialised from FILE after the command completes.
(defun rcs-command (command file-name options &optional
		    reread-buffer-p ignore-errors output-stream)
  (format t "Running RCS command: %s %s %s..." command file-name options)
  (clear-buffer rcs-output-buffer)
  (let
      ((arg-list (append options (list file-name))))
    (set-process-output-stream rcs-process
			       (or output-stream rcs-output-buffer))
    (set-process-error-stream rcs-process rcs-output-buffer)
    (unless (or (zerop (apply 'call-process rcs-process nil command arg-list))
		 ignore-errors)
      (signal 'file-error (list "Can't run RCS command"))))
  (format t "done")
  (when reread-buffer-p
    (let
	((old-pos (cursor-pos)))
      (read-file-into-buffer file-name)
      (goto old-pos))))

;; Switch to the buffer containing all RCS output
(defun rcs-goto-buffer ()
  (with-view (other-view)
    (goto-buffer rcs-output-buffer)
    (goto (start-of-buffer))
    (shrink-view-if-larger-than-buffer)))

;; Arrange for (rcs-command COMMAND FILE OPTIONS REREAD-P) to be called
;; later including a piece of text entered by the user. It will be added
;; to OPTIONS preceded by TEXT-PREFIX. TITLE gives part of the title of
;; the buffer used to enter the message.
(defun rcs-callback-with-description (title file command options
				      text-prefix reread-p)
  (let*
      ((buf-name (concat "*rcs: " title " " (file-name-nondirectory file) "*"))
       (buffer (get-buffer buf-name)))
    (if buffer
	(goto-buffer buffer)
      (setq buffer (open-buffer buf-name))
      (set-buffer-special buffer t)
      (goto-buffer buffer)
      (setq mildly-special-buffer t
	    keymap-path (cons rcs-callback-keymap keymap-path)))
    (text-mode)
    (setq rcs-callback-args (list file buffer command options
				  text-prefix reread-p)
	  ctrl-c-keymap rcs-callback-ctrl-c-keymap
	  rcs-history-level nil)))

;; Called when Ctrl-C Ctrl-C is typed in a callback buffer.
(defun rcs-call-callback ()
  (interactive)
  (goto (end-of-buffer))
  (when (= (pos-col (cursor-pos)) 0)
    (goto (forward-char -1)))
  (let
      ((text (copy-area (start-of-buffer) (cursor-pos))))
    (let
	((file (nth 0 rcs-callback-args))
	 (command (nth 2 rcs-callback-args))
	 (options (if (string-match "^[ \t\n]*$" text)
		      (nth 3 rcs-callback-args)
		    (cons (concat (nth 4 rcs-callback-args) text)
			  (nth 3 rcs-callback-args))))
	 (reread-p (nth 5 rcs-callback-args))
	 (callback-buffer (current-buffer)))
      (add-to-ring rcs-descr-ring text)
      (goto-buffer (get-file-buffer file))
      (rcs-command command file options reread-p)
      (kill-buffer callback-buffer))))

(defun rcs-down-history (&optional count)
  "Replace the buffer contents with the COUNT'th previous RCS change
description entered. COUNT may be negative."
  (interactive "p")
  (let
      ((level (+ (or rcs-history-level 0) (or count 1))))
    (if (or (<= level 0) (>= level (ring-size rcs-descr-ring)))
	(error "Invalid history item" level)
      (clear-buffer)
      (insert (get-from-ring rcs-descr-ring level))
      (setq rcs-history-level level))))

(defun rcs-up-history (&optional count)
  "Replace the buffer contents with the COUNT'th next RCS change
description entered. COUNT may be negative."
  (interactive "p")
  (rcs-down-history (- (unless count) 1)))
    
;; Returns t if the current buffer is locked under RCS
(defun rcs-buffer-locked-p ()
  ;; For now just check its read-only status
  (not (buffer-read-only-p)))

;; Initialises rcs-revision to a string defining the current revision
;; number of the current buffer. Returns this string, or nil
(defun rcs-get-version ()
  ;; First look for a Header, Id, or Revision keyword
  (let
      ((revision-pos (re-search-forward
		      "\\$((Header|Id): .*,v |Revision: )([0-9.]+) "
		      (start-of-buffer) buffer nil)))
    (if revision-pos
	(setq rcs-revision (copy-area (match-start 3) (match-end 3)))
      ;; Could run rlog -h FILE or something and look through
      ;; the output.. but how to deal with branches..
      (setq rcs-revision nil))))

;; Called from the find-file-hook in rcs-hooks.jl
;;;###autoload
(defun rcs-init-file (buffer)
  (with-buffer buffer
    ;; This file uses RCS. If the file doesn't actually exist, try to
    ;; check it out.
    (when (or (not (file-exists-p (buffer-file-name)))
	      (zerop (file-size (buffer-file-name))))
      ;; Attempt to check out the current revision read-only,
      ;; ignoring errors
      (rcs-command "co" (buffer-file-name) '("-r") t t))
    ;; Try to find its revision number and it's locked status, and put
    ;; them into the minor mode name.
    (let
	((info (concat "RCS"
		       (if (rcs-buffer-locked-p) ?: ?-)
		       (or (rcs-get-version) "?"))))
      (when (minor-mode-installed-p 'rcs-mode)
	(remove-minor-mode 'rcs-mode rcs-current-info-string))
      (add-minor-mode 'rcs-mode info)
      (setq rcs-current-info-string info
	    toggle-read-only-function 'rcs-toggle-read-only
	    rcs-controlled-buffer t)
      (unless rcs-make-backup-files
	;; Ensure no backup files are made for this buffer
	(make-local-variable 'make-backup-files)
	(setq make-backup-files nil)))))

;; Signals an error if the current buffer is not under RCS control
(defun rcs-verify-buffer ()
  (unless rcs-controlled-buffer
    (error "Buffer not under RCS control" (current-buffer))))


;; User interface

;;;###autoload
(defun rcs-register-buffer (&optional revision)
  "Register the file in the current buffer with RCS. If the variable
rcs-initial-comment is non-nil a description of the file will be prompted
for. REVISION is optionally the initial revision number to give the file.

When called interactively a non-nil prefix-argument causes the initial
revision to be prompted for."
  (interactive (list (and current-prefix-arg
			  (prompt-for-string "Initial revision:"))))
  (maybe-save-buffer (current-buffer))
  (if rcs-initial-comment
      (rcs-callback-with-description "description of"
				     (buffer-file-name)
				     "ci" (list (concat "-u" (or revision "")))
				     "-t-" t)
    (rcs-command "ci" (buffer-file-name)
		 (list (concat "-u" (or revision "")) "-t-") t)))

(defun rcs-check-in-buffer (&optional revision)
  "Checks in the current buffer, assuming that it's currently locked.
If REVISION is nil the next revision number will be used; otherwise
REVISION should be a string naming the revision to check the file in as.

If REVISION is nil and the current-prefix-arg non-nil a revision will
be prompted for."
  (interactive)
  ;; Can't use the interactive spec since this is called from
  ;; toggle-buffer-read-only, via rcs-toggle-read-only
  (when (and (not revision) current-prefix-arg)
    (setq revision (or (prompt-for-string
			(apply 'concat "Check in as revision:"
			       (and rcs-revision
				    (list " (currently " rcs-revision ")"))))
		       (error "No revision specified"))))
  (rcs-verify-buffer)
  (maybe-save-buffer (current-buffer))
  (rcs-callback-with-description "log for" (buffer-file-name)
				 "ci" (list (concat "-u" (or revision "")))
				 "-m" t))

(defun rcs-check-out-buffer (&optional revision)
  "Checks out the current buffer, locking it for modification. If REVISION
is nil the current revision will be checked out; otherwise REVISION should be
a string naming the revision to check out.

If REVISION is nil and the current-prefix-arg non-nil a revision will be
prompted for."
  (interactive)
  ;; Can't use the interactive spec since this is called from
  ;; toggle-buffer-read-only, via rcs-toggle-read-only
  (unless revision
    (setq revision (if current-prefix-arg
		       (or (prompt-for-string
			    (apply 'concat "Revision to lock:"
				   (and rcs-revision
					(list " (currently "
					      rcs-revision ")"))))
			   (error "No revision specified"))
		     rcs-revision)))
  (rcs-verify-buffer)
  (when (or (not (buffer-modified-p))
	    (y-or-n-p (concat "Buffer " (buffer-name) " modified; continue")))
    (rcs-command "co" (buffer-file-name)
		 (list (concat (if (and rcs-only-lock-seen-files
					rcs-revision
					revision
					(not (string= rcs-revision revision)))
				   "-r" "-l")
			       (or revision ""))) t)))

(defun rcs-view-revision (&optional revision)
  "Display an old revision of the current buffer. REVISION is a string
naming the revision, or nil, in which case it will be prompted for."
  (interactive)
  (unless revision
    (setq revision (prompt-for-string "Revision to view:")))
  (when revision
    (let*
	((buffer-name (concat (buffer-name) ?~ revision ?~))
	 (new-buffer (open-buffer buffer-name)))
      (when (check-changes new-buffer)
	(clear-buffer new-buffer)
	(set-buffer-read-only new-buffer nil)
	(rcs-command "co" (buffer-file-name)
		     (list (concat "-p" revision)) nil nil new-buffer)
	(set-buffer-read-only new-buffer t)
	(set-buffer-modified new-buffer nil)
	(goto-buffer new-buffer)
	(goto (start-of-buffer))))))

(defun rcs-revert-buffer ()
  "Discards any changes made since locking the current buffer."
  (interactive)
  (rcs-verify-buffer)
  (when (rcs-buffer-locked-p)
    (unless (yes-or-no-p (concat "Sure you want to return "
				 (buffer-name) " to its previous version?"))
      (error "RCS revert cancelled"))
    (maybe-save-buffer (current-buffer))
    (rcs-command "co" (buffer-file-name) '("-f" "-u") t)))

;;;###autoload
(defun rcs-display-log (file-name)
  "Displays the RCS log of FILE-NAME."
  (interactive (list (buffer-file-name)))
  (rcs-command "rlog" file-name rcs-display-log-args nil)
  (rcs-goto-buffer))

(defun rcs-compare-revisions (rev1 rev2)
  "Displays the differences between two revisions of the current buffer.

The two revisions are defined by REV1 and REV2, if both REV1 and REV2 are
strings, REV1 specifies the older revision, REV2 the newer. Else if REV1 is
a string but REV2 is nil, REV1 is compared with the current working version
of the buffer. When both REV1 and REV2 are nil, the head of the current
branch is compared with the working version.

When called interactively, if a prefix argument is given REV1 and REV2 will
be prompted for. Otherwise compare the currently selected revision of the
file with the working copy."
  (interactive
   (progn
     (rcs-verify-buffer)
     (if (not current-prefix-arg)
	 (list rcs-revision nil)
       (let
	   ((first (prompt-for-string (apply 'concat "Older revision:"
					     (when rcs-revision
					       (list " (working on "
						     rcs-revision ")"))))))
	 (list first (prompt-for-string
		      (concat "Newer revision: (older: " first ")")))))))
  (maybe-save-buffer (current-buffer))
  (rcs-command "rcsdiff" (buffer-file-name)
	       (append (when (and rev1 (not (string= rev1 "")))
			 (if (and rev2 (not (string= rev2 "")))
			     (list (concat "-r" rev1) (concat "-r" rev2))
			   (list (concat "-r" rev1))))
		       '("-c")) nil t)
  (rcs-goto-buffer))

;; Called by toggle-buffer-read-only for the current buffer
(defun rcs-toggle-read-only ()
  (if (buffer-read-only-p)
      ;; Need to check out and lock the current buffer
      (rcs-check-out-buffer)
    ;; Need to check in the current buffer
    (rcs-check-in-buffer)))
