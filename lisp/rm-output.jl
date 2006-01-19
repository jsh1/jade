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

(defvar rm-auto-archive-alist nil
  "List of (RULE . MAILBOX-NAME) used to associate messages with mailboxes
while auto-archiving.")


;;;; Code

;; Last folder written to
(defvar rm-last-output-folder nil)

;; Append MSG (a message structure), ending at position END, to the mailbox
;; DEST, either a buffer or a file-stream
(defun rm-output-message (msg dest)
  (with-buffer (mark-file (rm-get-msg-field msg rm-msg-mark))
    (save-restriction)
    (unrestrict-buffer)
    (let
	((text (copy-area (mark-pos (rm-get-msg-field msg rm-msg-mark))
			  (rm-message-end msg))))
      (cond
       ((bufferp dest)
	;; DEST is a buffer. Append to that.
	(with-buffer dest
	  (when (eq major-mode 'read-mail-mode)
	    ;; XXX re-enable this
	    (error "Can't append to buffers in read-mail-mode!"))
	  (save-restriction
	    (unrestrict-buffer)
	    (save-excursion
	      (goto (end-of-buffer))
	      (unless (zerop (1- (buffer-length)))
		(insert "\n"))
	      (let
		  ;; Don't override read-only in normal buffers
		  ((inhibit-read-only (eq major-mode 'read-mail-mode)))
		(insert text))))))
       ((filep dest)
	;; DEST is a file. Append to it. The flush is for when checking the
	;; size of the file
	(flush-file dest)
	(unless (zerop (file-size (file-binding dest)))
	  ;; The file isn't empty, so ensure there's a blank
	  ;; line separating messages
	  (write dest ?\n))
	(write dest text)))
      (rm-message-put msg 'filed t)
      (when rm-delete-after-output
	(rm-message-put msg 'deleted t)))))

;;;###autoload
(defun rm-output (dest)
  "Output the currently selected messages to the mail folder DEST. If DEST is
currently loaded into a buffer, append there; otherwise write straight to the
folder's file."
  (interactive (list (prompt-for-folder "Destination folder:"
					rm-last-output-folder)))
  (let*
      ((folder (rm-current-folder))
       (messages (rm-command-items folder)))
    (unless messages
      (error "No selected messages"))
    (let
	((real-dest (or (get-file-buffer dest)
			(open-file dest 'append))))
      (unwind-protect
	  (mapc #'(lambda (m)
		    (rm-output-message m real-dest)) messages)
	(when (filep real-dest)
	  (close-file real-dest))))
    (rm-redisplay-folder folder)))

;;;###autoload
(defun rm-auto-archive-folder (folder)
  "Auto-archive any messages in FOLDER matching a rule in the variable
`rm-auto-archive-alist'."
  (interactive (list (rm-current-folder) current-prefix-arg))
  (rm-map-messages
   #'(lambda (m)
       (catch 'saved
	 (mapc #'(lambda (cell)
		   (when (rm-apply-rule (car cell) m)
		     (let
			 ((dest (or (get-file-buffer (cdr cell))
				    (open-file (cdr cell) 'append))))
		       (unwind-protect
			   (rm-output-message m dest)
			 (when (filep dest)
			   (close-file dest))))
		     (throw 'saved t)))
	       rm-auto-archive-alist)))
   folder)
  (rm-redisplay-folder folder))

;;;###autoload
(defun rm-archive-folder (folder rule mailbox)
  "Archive all messages in FOLDER matching RULE to the mailbox called MAILBOX."
  (interactive (list (rm-current-folder)
		     (rm-prompt-for-rule "Rule to archive by:")
		     (prompt-for-folder "Archive to mailbox:")))
  (let
      ((dest (or (get-file-buffer mailbox)
		 (open-file mailbox 'append))))
    (unwind-protect
	(rm-map-messages #'(lambda (m)
			     (when (rm-apply-rule rule m)
			       (rm-output-message m dest)))
			 folder)
      (when (filep dest)
	(close-file dest))))
  (rm-redisplay-folder folder))
