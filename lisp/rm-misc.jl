;;;; rm-misc.jl -- Replying to and forwarding mail
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


;; Replying to messages

;; The message structure of the message being replied to.
(make-variable-buffer-local 'rm-reply-message)

;; This matches most forms of "Re: SUBJECT" strings, leaving SUBJECT
;; starting at the end of the first substring
(defvar rm-Re-regexp "^[\t ]*(Re(\\([0-9]+\\)|\\^[0-9]+)?:[\t ]*)*")

;;;###autoload
(defun rm-reply (&optional yankp followup-p)
  "Reply to the mail message currently being displayed."
  (interactive "P")
  (or rm-current-msg (error "No current message"))
  (let
      ((message rm-current-msg)
       (subject (rm-get-msg-field rm-current-msg rm-msg-subject))
       to cc msg-id)
    (save-restriction
      ;; Need to look at *all* headers
      (restrict-buffer (mark-pos (rm-get-msg-field rm-current-msg rm-msg-mark))
		       rm-current-msg-body)
      (setq to (mail-get-header "From" t)
	    cc (if followup-p
		   (nconc (mail-get-header "To" t)
			  (mail-get-header "CC" t))
		 '())
	    msg-id (mail-get-header "Message-Id")))
    (when (regexp-match rm-Re-regexp subject t)
      (setq subject (concat mail-reply-prefix
			    (substring subject (match-end)))))
    (mail-setup to subject msg-id cc nil
		(list (cons 'rm-reply-callback
			    (list (current-buffer) message))))
    (setq rm-reply-message message)
    (when yankp
      (mail-yank-original))))

(defun rm-reply-callback (buffer message)
  (rm-set-flag message 'replied)
  (with-buffer buffer
    (rm-with-summary
     (summary-update-item message))))

;;;###autoload
(defun rm-followup (&optional yankp)
  "Follow-up to the current mail message. This differs from replying to a
message in that all recipients of the original wil receive the reply."
  (interactive "P")
  (rm-reply yankp t))

;;;###autoload
(defun mail-yank-original ()
  "Insert the body of the message currently being replied to."
  (interactive)
  (let
      ((msg rm-reply-message)
       (yank-begin (cursor-pos))
       start end)
    (insert (with-buffer (mark-file (rm-get-msg-field msg rm-msg-mark))
	      (save-restriction
		(unrestrict-buffer)
		;; Insert everything but the initial ^From_ line
		(setq start (next-line 1 (copy-pos
					  (mark-pos
					   (rm-get-msg-field msg
							     rm-msg-mark))))
		      end start)
		(while (and end (setq end (find-next-regexp mail-message-start
							    end))
			    (not (rm-message-start-p end)))
		  (setq end (next-line 1 end)))
		(copy-area start (or end (buffer-end))))))
    (eval-hook 'mail-yank-hooks yank-begin (cursor-pos) rm-reply-message)))

(defun rm-default-yank-function (start end &optional msg)
  (when msg
    (restrict-buffer start end)
    (let
	((body-start (and (find-next-regexp "^\n" start) (match-end)))
	 body-end)
      (when body-start
	(delete-area start body-start)
	(format (cons (current-buffer) (buffer-start))
		"%s writes:\n" (or (rm-get-msg-field msg rm-msg-from-name)
				   (rm-get-msg-field msg rm-msg-from-addr)))
	(setq start (next-line 1 (buffer-start))))
      (setq body-end (find-prev-regexp "^.*[^\t\n ].*$" (buffer-end)))
      (while (<= start (or body-end (buffer-end)))
	(insert mail-yank-prefix start)
	(setq start (next-line 1 start)))
      (delete-area start (buffer-end)))
    (unrestrict-buffer)
    t))

(add-hook 'mail-yank-hooks 'rm-default-yank-function t)


;; Message forwarding

;;;###autoload
(defun rm-forward (&optional to all-headers-p)
  "Forward the current message. Optional arg TO specifies who to send
it to. When ALL-HEADERS-P is non-nil non-visible headers will be included."
  (interactive "\nP")
  (or rm-current-msg (error "No current message"))
  (unless to
    (setq to ""))
  (let
      ((subject (rm-get-msg-field rm-current-msg rm-msg-subject))
       (message rm-current-msg))
    (mail-setup to subject nil nil nil
		(cons #'(lambda (msg)
			  (rm-set-flag msg 'forwarded))
		      message))
    (insert "----- Begin Forwarded Message -----\n\n
----- End Forwarded Message -----\n")
    (goto-prev-line 2)
    (when all-headers-p
      ;; Quote ^From_
      (insert ">"))
    (restrict-buffer (cursor-pos) (line-end))
    (insert (with-buffer (mark-file (rm-get-msg-field message rm-msg-mark))
	      (save-restriction
		(unrestrict-buffer)
		(let*
		    ((start (if all-headers-p
				(mark-pos (rm-get-msg-field message
							    rm-msg-mark))
			      rm-current-msg-visible-start)))
		  (copy-area start rm-current-msg-end)))))
    ;; Quote "^-" as "- -" as specified by RFC-934
    (let
	((pos (buffer-start)))
      (while (find-next-regexp "^-" pos)
	(insert "- " (match-start))
	(setq pos (line-end (match-start)))))
    (unrestrict-buffer)
    (if (string= to "")
	(goto-char (line-end (buffer-start)))
      (goto-next-line 2))))


;; Message bursting

;;;###autoload
(defun rm-burst-message ()
  "Burst the currently displayed message, inserting the messages at the end
of the folder. The message being exploded should conform to the RFC-934
standard for encapsulating messages. Some effort will be made to avoid
inserting rubbish. The original message will be marked as deleted."
  (interactive)
  (let
      ((inhibit-read-only t)
       (input-start rm-current-msg-body)
       ;; Work backwards from the last message in the digest
       (input-pos rm-current-msg-end)
       (last-pos nil)
       (original-msg rm-current-msg)
       (msgs nil)
       (count 0)
       output-pos new-msg)
    (unrestrict-buffer)
    (let
	((rm-move-after-deleting nil))
      (rm-mark-message-deletion))
    ;; Find the last message in the folder
    (while rm-after-msg-list
      (rm-move-forwards))
    (restrict-buffer input-start input-pos)
    (unless (setq input-pos (find-prev-regexp "^-[^ ]" input-pos))
      (error "Not an RFC-934 digest!"))
    (setq last-pos input-pos
	  input-pos (prev-line 1 (copy-pos input-pos)))
    (while (setq input-pos (find-prev-regexp "^-[^ ]" input-pos))
      (unrestrict-buffer)
      (let
	  ((start (next-line 1 (copy-pos input-pos))))
	(when (looking-at "([\t ]*\n)+" start)
	  (setq start (match-end)))
	;; Try to ignore junk!?
	(if (not (or (looking-at "^>?From " start)
		     (looking-at mail-header-name start)))
	    (message "Ignoring nonsense message!")
	  ;; Enforce the "\n\n" rule between messages
	  (rm-enforce-msg-separator)
	  (setq output-pos (cursor-pos))
	  (insert (copy-area start last-pos))
	  ;; Unmangle "^- " lines
	  (let
	      ((pos (cursor-pos)))
	    (restrict-buffer output-pos pos)
	    (while (setq pos (find-prev-regexp "^- " pos))
	      (delete-area (match-end) (match-start))
	      (setq pos (prev-line 1 pos)))
	    (unrestrict-buffer))
	  ;; Unmangle quoted ^From_
	  (when (looking-at "^>From " output-pos)
	    (delete-area output-pos (next-char 1 (copy-pos output-pos))))
	  (unless (looking-at mail-message-start output-pos)
	    ;; No ^From_ line, kludge one ourselves
	    (insert (concat "From jade " (current-time-string) ?\n)
		    output-pos))
	  (when (setq new-msg (rm-build-message-struct output-pos))
	    (setq count (1+ count)
		  msgs (cons new-msg msgs))
	    (rm-set-flag new-msg 'unread)))
	(setq last-pos input-pos
	      input-pos (prev-line 1 (copy-pos input-pos)))
	(restrict-buffer input-start last-pos)))
    (unrestrict-buffer)
    (when rm-current-msg
      (setq rm-before-msg-list (cons rm-current-msg
				     rm-before-msg-list)
	    rm-current-msg-index (1+ rm-current-msg-index)))
    (setq rm-current-msg (car msgs)
	  rm-after-msg-list (cdr msgs)
	  rm-message-count (+ rm-message-count count)
	  rm-cached-msg-list 'invalid)
    (rm-with-summary
     (summary-update))
    (rm-display-current-message)))
