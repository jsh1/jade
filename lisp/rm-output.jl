;;;; rm-output.jl -- Saving messages
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

(require 'read-mail)

;;;; Configuration

(defvar rm-delete-after-output t
  "When t messages are marked for deletion after being saved to other files.")


;;;; Code

;; Last folder written to
(defvar rm-last-output-folder nil)

;; Append MSG (a message structure), ending at position END, to the mailbox
;; DEST, either a buffer or a file-stream
(defun rm-output-message (msg dest)
  (let*
      ((end (rm-message-end msg))
       (text (copy-area (mark-pos (rm-get-msg-field msg rm-msg-mark)) end
			(mark-file (rm-get-msg-field msg rm-msg-mark)))))
    (cond
     ((bufferp dest)
      ;; DEST is a buffer. Append to that.
      (with-buffer dest
	(when (eq major-mode 'read-mail-mode)
	  (error "Can't append to buffers in read-mail-mode!"))
	(save-restriction
	  (unrestrict-buffer)
	  (save-excursion
	    (goto (end-of-buffer))
	    (unless (zerop (1- (buffer-length)))
	      (insert "\n"))
	    (let
		((ins-start (cursor-pos))
		 ;; Don't override read-only in normal buffers
		 (inhibit-read-only (eq major-mode 'read-mail-mode)))
	      (insert text))))))
     ((filep dest)
      ;; DEST is a file. Append to it
      (unless (zerop (file-size (file-binding dest)))
	;; The file isn't empty, so ensure there's a blank
	;; line separating messages
	(write dest ?\n))
      (write dest text)))
    (rm-set-flag msg 'filed)
    (when rm-delete-after-output
      (rm-set-flag msg 'deleted))))

;;;###autoload
(defun rm-output (count dest)
  "Output COUNT messages, starting at the current message, to the mail
folder DEST. If DEST is currently loaded into a buffer, append there;
otherwise write straight to the folder's file."
  (interactive (list (prefix-numeric-argument current-prefix-arg)
		     (prompt-for-folder "Destination folder:"
					rm-last-output-folder)))
  (let*
      ((folder (rm-current-folder))
       (msg-list (cons (rm-get-folder-field folder rm-folder-current-msg)
		       (rm-get-folder-field folder rm-folder-after-list))))
    (unless (car msg-list)
      (error "No current message"))
    (save-restriction
      (unrestrict-buffer)
      (let
	  ((real-dest (or (get-file-buffer dest)
			  (open-file dest 'append))))
	(unwind-protect
	    (while (and (> count 0) msg-list)
	      (rm-output-message (car msg-list) real-dest)
	      (setq count (1- count)
		    msg-list (cdr msg-list)))
	  (when (filep real-dest)
	    (close-file real-dest)))))
    (when (rm-get-folder-field folder rm-folder-summary)
      (rm-with-summary folder
	(summary-update)))
    (rm-fix-status-info folder)))
