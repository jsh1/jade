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

;; Append MSG (a message structure), ending at position END, to the folder
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
	(save-restriction
	  (unrestrict-buffer)
	  (save-excursion
	    (goto (end-of-buffer))
	    (unless (zerop (buffer-length))
	      (insert "\n"))
	    (let
		((ins-start (cursor-pos))
		 ;; Don't override read-only in normal buffers
		 (inhibit-read-only (eq major-mode 'read-mail-mode)))
	      (insert text)
	      (when (eq major-mode 'read-mail-mode)
		;; Need to update mail reader's data structures :-}
		(let
		    ((new-msg (rm-build-message-struct ins-start)))
		  (when new-msg
		    (rm-set-msg-field new-msg rm-msg-flags
				      (rm-get-msg-field msg rm-msg-flags))
		    (if rm-current-msg
			(setq rm-after-msg-list (nconc rm-after-msg-list
						       (list new-msg))
			      rm-message-count (1+ rm-message-count)
			      rm-cached-msg-list 'invalid)
		      ;; The new message becomes the current one
		      (setq rm-current-msg new-msg
			    rm-message-count 1
			    rm-current-msg-index 0
			    rm-cached-msg-list 'invalid)
		      (rm-display-current-message))))))))))
     ((filep dest)
      ;; DEST is a file. Append to it; also append an extra newline
      ;; character to ensure the "\n\n" requirement at the end of the
      ;; file is met.
      (write dest ?\n)
      (write dest text)))
    (rm-set-flag msg 'filed)
    (when rm-delete-after-output
      (rm-with-summary
       (summary-mark-delete msg)))))

;;;###autoload
(defun rm-output (count dest)
  "Output COUNT messages, starting at the current message, to the mail
folder DEST. If DEST is currently loaded into a buffer, append there;
otherwise write straight to the folder's file."
  (interactive (list (prefix-numeric-argument current-prefix-arg)
		     (prompt-for-folder "Destination folder:"
					rm-last-output-folder)))
  (unless rm-current-msg
    (error "No current message"))
  (let
      ((msg-list (cons rm-current-msg rm-after-msg-list)))
    (save-restriction
      (unrestrict-buffer)
      (let
	  ((real-dest (or (get-file-buffer dest)
			  (open-file dest "a"))))
	(unwind-protect
	    (while (and (> count 0) msg-list)
	      (rm-output-message (car msg-list) real-dest)
	      (setq count (1- count)
		    msg-list (cdr msg-list)))
	  (cond
	   ((filep real-dest)
	    (close-file real-dest))
	   ((bufferp real-dest)
	    (with-buffer real-dest
	      (when (eq major-mode 'read-mail-mode)
		(rm-with-summary
		 (summary-update))))))))))
  (rm-with-summary
   (summary-update))
  (rm-fix-status-info))
