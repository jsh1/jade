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

;; Suppress annoying compiler warnings
(eval-when-compile (require 'send-mail))
(eval-when-compile (require 'rm-summary))

(defvar rm-citation-format "%F writes:\n"
  "Format string defining how quoted messages are preceded. All standard
message formatting characters are available.")


;; Replying to messages

;; The message structure of the message being replied to.
(defvar rm-reply-message nil)
(make-variable-buffer-local 'rm-reply-message)

;;;###autoload
(defun rm-reply (#!optional yankp followup-p ignore-replyto)
  "Reply to the mail message currently being displayed."
  (interactive "\n\nP")
  (let*
      ((folder (rm-current-folder))
       (msg (or (rm-get-folder-field folder rm-folder-current-msg)
		(error "No current message")))
       (subject (rm-get-subject msg))
       (to (or (and (not ignore-replyto)
		    (let
			((replyto (rm-get-msg-header msg "Reply-To" t)))
		      (when (stringp mail-ignored-reply-tos)
			(setq replyto (delete-if
				       (lambda (m)
					 (string-match
					  mail-ignored-reply-tos m)) replyto)))
		      (mapcar mail-parse-address replyto)))
	       (rm-get-from msg)
	       (rm-get-sender msg)))
       (cc (if followup-p
	       ;; Only include addresses not in the TO
	       (filter (lambda (addr)
			 (catch 'foo
			   (mapc (lambda (addr2)
				   (and (mail-compare-addresses addr addr2)
					(throw 'foo nil)))
				 ;; also include the user's address
				 (cons user-mail-address to))
			   t)) (rm-get-recipients msg))))
       (msg-id (rm-get-msg-header msg "Message-Id"))
       (references (append (rm-get-msg-header msg "References" t t)
			   (and msg-id (list msg-id)))))
    (when subject
      (setq subject (concat mail-reply-prefix
			    (mail-get-actual-subject subject))))
    (let
	((fun (lambda (cell)
		(mail-format-address (car cell) (cdr cell)))))
      (mail-setup (mapcar fun to) subject msg-id (mapcar fun cc) references
		  (list (cons (lambda (f m)
				(declare (unused f))
				(rm-message-put m 'replied t))
			      (list folder msg)))))
    (if to
	(send-mail-go-text)
      (send-mail-go-to))
    (setq rm-reply-message msg)
    (when yankp
      (mail-yank-original))
    (set-buffer-modified (current-buffer) nil)))

;;;###autoload
(defun rm-followup (#!optional yankp ignore-replyto)
  "Follow-up to the current mail message. This differs from replying to a
message in that all recipients of the original wil receive the reply."
  (interactive "\nP")
  (rm-reply yankp t ignore-replyto))

;;;###autoload
(defun mail-yank-original ()
  "Insert the body of the message currently being replied to."
  (interactive)
  (let
      ((msg rm-reply-message)
       (yank-begin (cursor-pos)))
    ;; XXX: when yanking MIME encoded messages should we yank the
    ;; XXX: decoded version or the encoded version? Perhaps have
    ;; XXX: it as an option...
    (insert (with-buffer (mark-file (rm-get-msg-field msg rm-msg-mark))
	      (save-restriction
		(unrestrict-buffer)
		;; Insert everything but the initial ^From_ line
		(copy-area (forward-line 1 (mark-pos (rm-get-msg-field
						      msg rm-msg-mark)))
			   (rm-message-end msg)))))
    (call-hook 'mail-yank-hooks
	       (list yank-begin (cursor-pos) rm-reply-message) 'or)))

(defun rm-default-yank-function (start end #!optional msg)
  (when msg
    (restrict-buffer start end)
    (let
	((body-start (and (re-search-forward "^\n" start) (match-end)))
	 body-end)
      (when body-start
	(delete-area start body-start)
	(insert (rm-format rm-citation-format msg) (start-of-buffer))
	(setq start (forward-line 1 (start-of-buffer))))
      (setq body-end (re-search-backward "^.*[^\t\n ].*$" (end-of-buffer)))
      (while (<= start (or body-end (end-of-buffer)))
	(insert mail-yank-prefix start)
	(setq start (forward-line 1 start)))
      (delete-area start (end-of-buffer)))
    (unrestrict-buffer)
    t))

(add-hook 'mail-yank-hooks rm-default-yank-function t)


;; Message forwarding

;;;###autoload
(defun rm-forward (#!optional to)
  "Forward the current message using RFC-934 message encapsulation. Optional
arg TO specifies who to send it to."
  (interactive "\nP")
  (unless to
    (setq to ""))
  (let*
      ((folder (rm-current-folder))
       (msg (or (rm-get-folder-field folder rm-folder-current-msg)
		(error "No current message")))
       (subject (rm-get-subject msg))
       start tem)
    (mail-setup to subject nil nil nil
		(list (cons (lambda (f m)
			      (rm-message-put m 'forwarded t)
			      (when (rm-get-folder-field
				     f rm-folder-summary)
				(rm-with-summary f
				  (summary-update-item m))))
			    (list folder msg))))
    (insert "----- begin forwarded message -----\n")
    (setq start (cursor-pos))
    (goto (insert (with-buffer (mark-file (rm-get-msg-field msg
							    rm-msg-mark))
		    (let*
			((start (restriction-start))
			 (end (restriction-end)))
		      (save-restriction
			(unrestrict-buffer)
			(copy-area start end))))))
    ;; Quote "^-" as "- -" as specified by RFC-934
    (setq tem start)
    (while (re-search-forward "^-" tem)
      (insert "- " (match-start))
      (setq tem (end-of-line (match-start))))
    ;; Delete trailing blank lines
    (goto (start-of-line))
    (while (looking-at "^[\t ]*$")
      (delete-area (forward-char -1 (match-start)) (match-end)))
    (insert "\n----- end forwarded message -----\n")
    (if (null to)
	(send-mail-go-to)
      (send-mail-go-text))
    (set-buffer-modified nil nil)))


;; Message bursting

(defvar rm-rfc1153-preamble-sep (concat ?^ (make-string 70 ?-) ?$))
(defvar rm-rfc1153-message-sep (concat ?^ (make-string 30 ?-) ?$))
(defvar rm-rfc1153-stuffed-re (concat "^ (" (make-string 30 ?-) ")$"))

(defvar rm-rfc934-preamble-sep "^-[^ ]")
(defvar rm-rfc934-message-sep rm-rfc934-preamble-sep)
(defvar rm-rfc934-stuffed-re "^- (.*)$")

(defun rm-really-burst-message (preamble-sep message-sep stuffed-re)
  (let*
      ((folder (rm-current-folder))
       (msg (or (rm-get-folder-field folder rm-folder-current-msg)
		    (error "No current message")))
       (inhibit-read-only t)
       (input-pos (rm-message-body msg))
       (input-end (rm-message-end msg))
       (last-pos nil)
       (msgs nil)
       (count 0)
       output-pos
       new-msg)
    (unrestrict-buffer)
    (let
	((rm-move-after-deleting nil))
      (rm-mark-message-deletion))
    ;; Find the last message in the folder
    (while (rm-get-folder-field folder rm-folder-after-list)
      (rm-move-forwards folder))

    ;; Find the start of the first message
    (restrict-buffer input-pos input-end)
    (unless (setq input-pos (re-search-forward preamble-sep input-pos))
      (error "Can't find digest preamble!"))
    (setq input-pos (forward-line 1 input-pos)
	  last-pos input-pos)
    (while (setq input-pos (re-search-forward message-sep input-pos))
      (unrestrict-buffer)
      (let
	  ((start last-pos))
	(when (looking-at "([\t ]*\n)+" start)
	  (setq start (match-end)))
	;; Ignore null message
	(unless (looking-at message-sep start)
	  ;; Try to ignore junk!?
	  (if (not (or (looking-at "^>?From " start)
		       (looking-at mail-header-name start)))
	      (message "Ignoring nonsense message!")
	    ;; Enforce the "\n\n" rule between messages
	    (goto (end-of-buffer))
	    (unless (zerop (1- (buffer-length)))
	      (insert "\n"))
	    (setq output-pos (cursor-pos))
	    (insert (copy-area start input-pos))
	    ;; Unmangle stuffed lines
	    (let
		((p (cursor-pos)))
	      (restrict-buffer output-pos p)
	      (while (setq p (re-search-backward stuffed-re p))
		(replace-last-match "\\1")
		(setq p (forward-line -1 p)))
	      (unrestrict-buffer))
	    ;; Unmangle quoted ^From_
	    (when (looking-at "^>From " output-pos)
	      (delete-area output-pos (forward-char 1 output-pos)))
	    (unless (looking-at mail-message-start output-pos)
	      ;; No ^From_ line, kludge one ourselves
	      (insert (concat "From jade " (current-time-string) ?\n)
		      output-pos))
	    (when (setq new-msg (rm-parse-message output-pos))
	      (setq count (1+ count)
		    msgs (cons new-msg msgs))
	      (rm-message-put new-msg 'unread t))))
	(setq input-pos (forward-line 1 input-pos)
	      last-pos input-pos)
	(restrict-buffer input-pos input-end)))
    (rm-add-messages folder msgs)))
  
;;;###autoload
(defun rm-burst-message ()
  "Burst the currently displayed message, inserting the messages at the end
of the folder. Prompts for the digest type, RFC-934 or RFC-1153."
  (interactive)
  (when rm-buffer-read-only
    (error "Read-only mailbox"))
  (let
      ((tem (re-search-forward rm-rfc934-preamble-sep (start-of-buffer))))
    (when tem
      (if (looking-at rm-rfc1153-preamble-sep tem)
	  (setq tem 'rfc1153)
	(setq tem 'rfc934)))
    ;; Now tem has the guessed default type or nil
    (setq tem (intern (prompt-from-list '("rfc934" "rfc1153")
					"Type of digest (`rfc934' or `rfc1153'):"
					(and tem (symbol-name tem)))))
    (cond
     ((eq tem 'rfc934)
      (rm-really-burst-message rm-rfc934-preamble-sep
			       rm-rfc934-message-sep
			       rm-rfc934-stuffed-re))
     ((eq tem 'rfc1153)
      (rm-really-burst-message rm-rfc1153-preamble-sep
			       rm-rfc1153-message-sep
			       rm-rfc1153-stuffed-re))
     (t
      (error "Unknown digest type" tem)))))
