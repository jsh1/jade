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
;;;   - options to work on specific revisions
;;;   - commands for working with branches
;;;   - command to convert change descriptions to Changelog entries

(require 'ring)
(provide 'rcs)

(defvar rcs-initial-comment nil
  "When non-nil ask for a string describing the file being registered,
when initialising RCS files.")


;; Initialisation

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

(unless (boundp 'rcs-initialised)
  ;; Only do this stuff once
  (bind-keys rcs-callback-ctrl-c-keymap
    "Ctrl-c" 'rcs-call-callback)
  (bind-keys rcs-callback-keymap
    "Meta-p" 'rcs-down-history
    "Meta-n" 'rcs-up-history)
  (setq rcs-initialised t))


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
  (goto-buffer rcs-output-buffer)
  (goto (start-of-buffer)))

;; (FILE CALLBACK-BUFFER COMMAND OPTIONS TEXT-PREFIX REREAD)
(make-variable-buffer-local 'rcs-callback-args)

;; Depth of last inserted history item
(make-variable-buffer-local 'rcs-history-level)

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
      ((level (if rcs-history-level
		  (+ rcs-history-level (unless count 1))
		1)))
    (if (or (<= level 0) (>= level (ring-size rcs-descr-ring)))
	(error 'RCS "Invalid history item" level)
      (clear-buffer)
      (insert (get-from-ring rcs-descr-ring level))
      (setq rcs-history-level level))))

(defun rcs-up-history (&optional count)
  "Replace the buffer contents with the COUNT'th next RCS change
description entered. COUNT may be negative."
  (interactive "p")
  (rcs-down-history (- (unless count) 1)))
    
;; Returns t if BUFFER is locked under RCS
(defun rcs-buffer-locked-p (buffer)
  ;; For now just check its read-only status
  (not (buffer-read-only-p buffer)))

;; Returns a string defining the current revision number of BUFFER
(defun rcs-version (buffer)
  ;; First look for a Header, Id, or Revision keyword
  (let
      ((revision-pos (re-search-forward
		      "\\$((Header|Id): .*,v |Revision: )([0-9.]+) "
		      (start-of-buffer) buffer nil)))
    (if revision-pos
	;; Found the id
	(copy-area (match-start 3) (match-end 3) buffer)
      ;; Can't find the id
      "?.?")))

;; Called from the open-file-hook in rcs-hooks.jl
(defun rcs-init-file (buffer)
  ;; This file uses RCS. Try to find its revision number and it's
  ;; locked status, and put them into the minor mode name.
  (let
      ((info (concat "RCS" (if (buffer-read-only-p buffer) "-" ":")
		     (rcs-version buffer))))
    (with-buffer buffer
      (when (minor-mode-installed 'rcs-mode)
	(remove-minor-mode 'rcs-mode rcs-current-info-string))
      (add-minor-mode 'rcs-mode info t)
      (setq rcs-current-info-string info
	    toggle-read-only-function 'rcs-toggle-read-only
	    rcs-controlled-buffer t)
      ;; Ensure no backup files are made for this buffer
      (make-local-variable 'make-backup-files)
      (setq make-backup-files nil))))

;; Signals an error if BUFFER is not under RCS control
(defun rcs-verify-buffer (buffer)
  (unless (with-buffer buffer rcs-controlled-buffer)
    (error "Buffer not under RCS control" buffer)))


;; User interface

;;;###autoload
(defun rcs-register-buffer (&optional buffer revision)
  "Register the file in BUFFER with RCS. If the variable rcs-initial-comment
is non-nil a description of the file will be prompted for. REVISION is
optionally the initial revision number to give the file. When called
interactively a non-nil prefix-argument causes the initial revision to
be prompted for."
  (interactive (list nil (and current-prefix-arg
			      (prompt-for-string "Initial revision:"))))
  (unless buffer (setq buffer (current-buffer)))
  (maybe-save-buffer buffer)
  (if rcs-initial-comment
      (rcs-callback-with-description "description of"
				     (buffer-file-name buffer)
				     "ci" (list (concat "-u" (or revision "")))
				     "-t-" t)
    (rcs-command "ci" (buffer-file-name buffer)
		 (list (concat "-u" (or revision "")) "-t-") t)))

(defun rcs-check-in-buffer (&optional buffer revision)
  "Checks in BUFFER, assuming that it's currently locked. If REVISION is
nil the next revision number will be used; otherwise REVISION should be
a string naming the revision to check the file in as. If REVISION is nil
and the current-prefix-arg non-nil a revision will be prompted for."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  ;; Can't use the interactive spec since this is called from
  ;; toggle-buffer-read-only, via rcs-toggle-read-only
  (when (and (not revision) current-prefix-arg)
    (setq revision (prompt-for-string "Revision to lock:")))
  (rcs-verify-buffer buffer)
  (maybe-save-buffer buffer)
  (rcs-callback-with-description "changes to"
				 (buffer-file-name buffer)
				 "ci" (list (concat "-u" (or revision "")))
				 "-m" t))

(defun rcs-lock-buffer (&optional buffer revision)
  "Checks out BUFFER, locking it for modification. If REVISION is nil
the current revision will be checked out; otherwise REVISION should be
a string naming the revision to check out. If REVISION is nil and
the current-prefix-arg non-nil a revision will be prompted for."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  ;; Can't use the interactive spec since this is called from
  ;; toggle-buffer-read-only, via rcs-toggle-read-only
  (when (and (not revision) current-prefix-arg)
    (setq revision (prompt-for-string "Revision to lock:")))
  (rcs-verify-buffer buffer)
  (when (or (not (buffer-modified-p buffer))
	    (y-or-n-p (concat "Buffer " (buffer-name buffer)
			      " modified; continue")))
    (rcs-command "co" (buffer-file-name buffer)
		 (list (concat "-l" (or revision ""))) t)))

(defun rcs-view-revision (&optional buffer revision)
  "Display an old revision of BUFFER. REVISION is a string naming the
revision, or nil, in which case it will be prompted for."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (unless revision
    (setq revision (prompt-for-string "Revision to view:")))
  (when revision
    (let*
	((buffer-name (concat (buffer-name buffer) ?~ revision ?~))
	 (new-buffer (open-buffer buffer-name)))
      (when (check-changes new-buffer)
	(clear-buffer new-buffer)
	(set-buffer-read-only new-buffer nil)
	(rcs-command "co" (buffer-file-name buffer)
		     (list (concat "-p" revision)) nil nil new-buffer)
	(set-buffer-read-only new-buffer t)
	(set-buffer-modified new-buffer nil)
	(goto-buffer new-buffer)
	(goto (start-of-buffer))))))

(defun rcs-revert-buffer (&optional buffer)
  "Discards any changes made since locking BUFFER."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (rcs-verify-buffer buffer)
  (when (rcs-buffer-locked-p buffer)
    (unless (yes-or-no-p (concat "Sure you want to return "
				 (buffer-name buffer)
				 " to its previous version?"))
      (error "RCS revert cancelled"))
    (maybe-save-buffer buffer)
    (rcs-command "co" (buffer-file-name buffer) '("-f" "-u") t)))

;;;###autoload
(defun rcs-display-log (file-name)
  "Displays the RCS log of FILE-NAME."
  (interactive (list (buffer-file-name)))
  (rcs-command "rlog" file-name '() nil)
  (rcs-goto-buffer))

;;;###autoload
(defun rcs-display-diffs (file-name)
  "Displays the differences between the last two versions of FILE-NAME."
  (interactive (list (buffer-file-name)))
  (let
      ((buffer (get-file-buffer file-name)))
    (when buffer
      (maybe-save-buffer buffer)))
  (rcs-command "rcsdiff" file-name '("-c") nil t)
  (rcs-goto-buffer))

;; Called by toggle-buffer-read-only for the current buffer
(defun rcs-toggle-read-only ()
  (if (buffer-read-only-p)
      ;; Need to check out and lock the current buffer
      (rcs-lock-buffer)
    ;; Need to check in the current buffer
    (rcs-check-in-buffer)))
