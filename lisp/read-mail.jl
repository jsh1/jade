;;;; read-mail.jl -- Simple mail reading mode
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

(require 'summary)
(provide 'read-mail)

;; Configurable parameters

(defvar mail-visible-headers
  "^((Resent-|)(From|Sender|Reply-To|To|Cc|Bcc|Date|Sender)|Subject|Keywords|Comments)[ \t]*:"
  "Regular expression matching the message headers that should be displayed
when showing a mail message.")

(defvar mail-folder-dir (expand-file-name "~/Mail")
  "The directory in which mail folders are stored by default.")

(defvar mail-two-digit-year-prefix (substring (current-time-string) 20 22)
  "A two-digit string that will be prepended to year specifications that
only have two, lower order, digits. This is picked up automatically from
the current year, i.e. 1997 -> \"19\", 2001 -> \"20\".")

(defvar mail-summary-lines 16
  "The number of lines that the summary view of a mail folder contains.")

(defvar mail-message-start "^From "
  "The regular expression separating each message. Actually it's interpreted
as a single blank line or the start of the buffer, followed by this regexp.")

;; This is defined as 1 or more characters followed by a colon. The
;; characters may not include SPC, any control characters, or ASCII DEL.
;; Unfortunately the regexp library doesn't allow NUL bytes in regexps so
;; they slip through..
;; This is also defined in send-mail.jl
(defvar mail-header-name "^([^\001- \^?]+)[ \t]*:"
  "A regexp matching a header field name. The name is left in the match
buffer as the first substring.")

;; Variables

;; The message list is kept in three parts. The messages before the
;; current message, in reverse order (rm-before-msg-list), the current
;; message itself (rm-current-msg), and the messages after the current
;; message (rm-after-msg-list).
(defvar rm-before-msg-list nil
  "The messages before the current message, in reverse order.")
(defvar rm-current-msg nil
  "The currently displayed message.")
(defvar rm-after-msg-list nil
  "The messages after the current message, in normal order.")
(make-variable-buffer-local 'rm-before-msg-list)
(make-variable-buffer-local 'rm-current-msg)
(make-variable-buffer-local 'rm-after-msg-list)

(defvar rm-current-msg-index nil
  "The index in the folder of the current message.")
(make-variable-buffer-local 'rm-current-msg-index)

(defvar rm-current-msg-end nil
  "The position of the last line of the current message.")
(defvar rm-current-msg-visible-start nil
  "The position of the first visible line of the current message.")
(defvar rm-current-msg-body nil
  "The position of the first line of the message body in the current message.")
(make-variable-buffer-local 'rm-current-msg-end)
(make-variable-buffer-local 'rm-current-msg-visible-start)
(make-variable-buffer-local 'rm-current-msg-body)

(defvar rm-cached-msg-list 'invalid
  "A cached copy of the message list, used by the summary buffer. Invalid
when set to the symbol `invalid'.")
(make-variable-buffer-local 'rm-cached-msg-list)

(defvar rm-summary-buffer nil
  "The buffer displaying the summary of this folder. nil if no summary exists.")
(make-variable-buffer-local 'rm-summary-buffer)

(defvar rm-keymap (make-keylist)
  "Keymap for reading mail")
(bind-keys rm-keymap
  "n" 'rm-next-message
  "p" 'rm-previous-message
  "t" 'rm-toggle-all-headers
  "SPC" 'next-screen
  "BS" 'prev-screen
  "h" 'rm-summary)


;; User-visible interface

;;;###autoload
(defun read-mail (folder)
  "Read mail stored in the file FOLDER."
  (interactive "FMail folder to open:")
  (when (find-file-read-only folder)
    ;; The current buffer is now the folder. Set up the major mode
    (read-mail-mode)))

;;;###autoload
(defun read-mail-mode ()
  "Read-Mail Mode:\n
Major mode for viewing mail folders. Commands include:\n
  `n'			Display the next message.
  `p'			Display the previous message.
  `t'			Toggle between showing all headers and just
			 showing important headers in the current msg.
  `SPC'			Display the next page of the message.
  `BS'			Display the previous page of the message.
  `h'			Create/update the folder summary."
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "Read-Mail"
	major-mode 'read-mail-mode
	major-mode-kill 'read-mail-mode-kill
	mode-comment-fun 'c-insert-comment
	;;ctrl-c-keymap c-mode-ctrl-c-keymap
	keymap-path (cons 'rm-keymap keymap-path))
  (eval-hook 'read-mail-mode-hook)
  ;; Build the message list and display the current message
  (rm-build-message-lists)
  (rm-display-current-message))

(defun read-mail-mode-kill ()
  (setq mode-name nil
	major-mode nil
	major-mode-kill nil
	;;ctrl-c-keymap nil
	keymap-path (delq 'rm-keymap keymap-path)))


;; Internal message structure

;; [ START-MARK FROM-ADDR FROM-NAME SUBJECT DAY-NAME DAY MONTH YEAR TIME ZONE ]
(defconst rm-msg-mark 0)
(defconst rm-msg-from-addr 1)
(defconst rm-msg-from-name 2)
(defconst rm-msg-subject 3)
(defconst rm-msg-day-name 4)
(defconst rm-msg-day 5)
(defconst rm-msg-month 6)
(defconst rm-msg-year 7)
(defconst rm-msg-time 8)
(defconst rm-msg-zone 9)
(defconst rm-msg-struct-size 10)

(defmacro rm-set-msg-field (msg field value)
  (list 'aset msg field value))

(defmacro rm-get-msg-field (msg field)
  (list 'aref msg field))

(defmacro rm-make-msg ()
  '(make-vector rm-msg-struct-size))


;; Local functions

;; Twiddle the message lists so that the next message is current
(defmacro rm-move-forwards ()
  '(setq rm-before-msg-list (cons rm-current-msg rm-before-msg-list)
	 rm-current-msg (car rm-after-msg-list)
	 rm-after-msg-list (cdr rm-after-msg-list)
	 rm-current-msg-index (1+ rm-current-msg-index)))

;; Twiddle the message lists so that the previous message is current
(defmacro rm-move-backwards ()
  '(setq rm-after-msg-list (cons rm-current-msg rm-after-msg-list)
	 rm-current-msg (car rm-before-msg-list)
	 rm-before-msg-list (cdr rm-before-msg-list)
	 rm-current-msg-index (1- rm-current-msg-index)))

;; Create the lists of messages. The buffer should be unrestricted when
;; calling this. Returns the current message.
(defun rm-build-message-lists ()
  (let
      ;; If we scan the buffer from start to end we can create the
      ;; list in reverse order, just what we want.
      ((pos (buffer-start))
       (msgs nil)
       (count 0))
    (while (and pos (setq pos (find-next-regexp mail-message-start pos nil t)))
      (when (rm-message-start-p pos)
	(setq msgs (cons (rm-build-message-struct pos) msgs)
	      count (1+ count)))
      (next-line 1 pos))
    ;; Okay, the current message is the last in the buffer. There's
    ;; no messages after the current one, all the rest go before.
    (setq rm-current-msg (car msgs)
	  rm-before-msg-list (cdr msgs)
	  rm-after-msg-list '()
	  rm-current-msg-index (1- count)
	  rm-cached-msg-list 'invalid)
    rm-current-msg))

;; Parse one message and return a message structure. The buffer
;; should be unrestricted.
(defun rm-build-message-struct (start)
  (let
      ((end (find-next-regexp "^$" start))
       (msg (rm-make-msg))
       pos)
    (restrict-buffer start (unless end (buffer-end)))
    (rm-set-msg-field msg rm-msg-mark (make-mark start))
    (when (or (find-next-regexp "^From[ \t]*:[ \t]*" start nil t)
	      (find-next-regexp "^Sender[ \t]*:[ \t]*" start nil t))
      (setq pos (match-end))
      (let*
	  ((atom-re "[a-zA-Z0-9_*+!#$~%^&={}'|-]+")
	   (addr-re (concat atom-re "(\\." atom-re ")*@" atom-re "(\\." atom-re ")*"))
	   (angle-addr-re (concat "<(" addr-re ")>"))
	   (angle-name-re "[\t ]*\"?([^<\t\" \n\f]([\t ]*[^<\t\" \n\f])*)")
	   (paren-name-re "[\t ]*\\(\"?([^\n\"]+)\"?\\)"))
	(cond
	 ((looking-at addr-re pos)
	  ;; straightforward "foo@bar.baz" format..
	  (rm-set-msg-field msg rm-msg-from-addr
			    (copy-area (match-start) (match-end)))
	  (when (looking-at paren-name-re (match-end))
	    ;; ..with a "(Foo Bar)" comment following
	    (rm-set-msg-field msg rm-msg-from-name
			      (copy-area (match-start 1) (match-end 1)))))
	 ((regexp-match-line angle-addr-re pos)
	  ;; "..<foo@bar.baz>..." format
	  (rm-set-msg-field msg rm-msg-from-addr
			    (copy-area (match-start 1) (match-end 1)))
	  ;; Now look for a preceding name
	  (when (looking-at angle-name-re pos)
	    (rm-set-msg-field msg rm-msg-from-name
			      (copy-area (match-start 1) (match-end 1))))))))
    (when (find-next-regexp "^Subject[ \t]*:[\t ]*(.*)[\t ]*$" start nil t)
      (rm-set-msg-field msg rm-msg-subject
			(copy-area (match-start 1) (match-end 1))))
    (when (find-next-regexp "^Date[\t ]*:[\t ]*" start nil t)
      (setq pos (match-end))
      (when (looking-at "[\t ]*(Mon|Tue|Wed|Thu|Fri|Sat|Sun)[\t ]*,[\t ]*"
			pos nil t)
	(rm-set-msg-field msg rm-msg-day-name
			  (copy-area (match-start 1) (match-end 1)))
	(setq pos (match-end)))
      (when (looking-at "[\t ]*([0-9]+)[\t ]+([A-Za-z]+)[\t ]+([0-9]+)[\t ]+"
			pos)
	(rm-set-msg-field msg rm-msg-day
			  (copy-area (match-start 1) (match-end 1)))
	(rm-set-msg-field msg rm-msg-month
			  (copy-area (match-start 2) (match-end 2)))
	(rm-set-msg-field msg rm-msg-year
			  (if (= (- (pos-col (match-end 3))
				    (pos-col (match-start 3)))
				 2)
			      ;; 2-digit year; concat a two digit prefix
			      ;; onto the front
			      (concat mail-two-digit-year-prefix
				      (copy-area (match-start 3)
						 (match-end 3)))
			    (copy-area (match-start 3) (match-end 3))))
	(setq pos (match-end)))
      (when (looking-at "([0-9]+:[0-9]+(:([0-9]+)|))[\t ]*([A-Z]+|[+-][0-9]+)"
			pos)
	(rm-set-msg-field msg rm-msg-time
			  (copy-area (match-start 1) (match-end 1)))
	(rm-set-msg-field msg rm-msg-zone
			  (copy-area (match-start 4) (match-end 4)))))
    (unrestrict-buffer)
    msg))

;; Returns the position of the start of the last line in the current message.
;; Works no matter what the restriction is set to.
(defun rm-current-message-end ()
  (if rm-after-msg-list
      (prev-line 1 (copy-pos (mark-pos (rm-get-msg-field
					(car rm-after-msg-list) rm-msg-mark))))
    (buffer-end nil t)))

;; Display the current message
(defun rm-display-current-message ()
    (unrestrict-buffer)
    (when rm-current-msg
      (let
	  ((header-start (mark-pos (rm-get-msg-field rm-current-msg
						     rm-msg-mark))))
	(unless (regexp-match-line mail-message-start header-start nil t)
	  (error "Position isn't start of header: %s" header-start))
	(let
	    ((end-of-hdrs (find-next-regexp "^$" header-start))
	     (inhibit-read-only t))
	  (setq rm-current-msg-body end-of-hdrs)
	  ;; Just operate on the headers
	  (restrict-buffer header-start (prev-line 1 (copy-pos end-of-hdrs)))
	  ;; First of all, move all visible headers after non-visible ones
	  (setq rm-current-msg-visible-start (rm-coalesce-visible-headers))
	  (unrestrict-buffer)
	  (setq rm-current-msg-end (rm-current-message-end))
	  (goto-char end-of-hdrs)
	  (rm-restrict-to-message)
	  ;; Called when the current restriction is about to be
	  ;; displayed
	  (eval-hook 'read-mail-display-message-hook rm-current-msg))))
    ;; Fix the summary buffer if it exists
    (when rm-summary-buffer
      (let
	  ((sum-buf rm-summary-buffer))
	(with-view (other-view mail-summary-lines)
	  (goto-buffer sum-buf)
	  (rm-summary-update-current)))))

;; Make MSG the current message, rejigging the message lists as necessary
;; Doesn't fix the variables defining some of the positions in the current
;; message.
(defun rm-make-message-current (msg)
  (unless (eq msg rm-current-msg)
    (if (memq msg rm-after-msg-list)
	;; Move forwards
	(while (not (eq rm-current-msg msg))
	  (rm-move-forwards))
      ;; Move backwards
      (while (not (eq rm-current-msg msg))
	(rm-move-backwards)))))

;; Display an arbitrary MSG
(defun rm-display-message (msg)
  (rm-make-message-current msg)
  (rm-display-current-message))

;; Ensure that all headers matching mail-visible-headers are at the end
;; of the header section so that the non-visible headers can be left out
;; of the display restriction. The buffer should currently be restricted
;; to the header section of the message. Returns the position of the first
;; visible header.
(defun rm-coalesce-visible-headers ()
  (let*
      ((first-visible (find-next-regexp mail-visible-headers
					(buffer-start) nil t))
       (current-hdr first-visible)
       (next-hdr first-visible))
    (while current-hdr
      ;; Move over all visible headers from CURRENT-HDR until
      ;; reaching an invisible one
      (setq next-hdr current-hdr)
      (while (and next-hdr
		  (regexp-match-line mail-visible-headers next-hdr nil t))
	(setq next-hdr (rm-unfold-header next-hdr)))
      ;; Now we have a block of visible headers from CURRENT-HDR to
      ;; NEXT-HDR (but not including NEXT-HDR). Next find the following
      ;; visible header to delimit the block of invisible ones
      (setq current-hdr next-hdr)
      (while (and next-hdr
		  (not (regexp-match-line mail-visible-headers
					  next-hdr nil t)))
	(setq next-hdr (rm-unfold-header next-hdr)))
      ;; Now we have a block of invisible headers, CURRENT-HDR to NEXT-HDR
      ;; Move them to before the FIRST-VISIBLE-HDR, updating this to
      ;; point to the end of the insertion
      (unless next-hdr
	(setq next-hdr (buffer-end)))
      (when (and current-hdr
	       (> current-hdr first-visible)
	       (> next-hdr current-hdr))
	(setq first-visible (insert (cut-area current-hdr next-hdr)
				    first-visible))
	(if (not (zerop (pos-col first-visible)))
	    ;; Last insertion didn't add a trailing newline. This
	    ;; means that the end of the headers has been reached!
	    (setq first-visible (insert "\n" first-visible)
		  current-hdr nil)
	  (setq current-hdr next-hdr))))
    first-visible))

;; Sets up the buffer restriction to just show the current message
;; When SHOW-ALL-HDRS-P is non-nil the whole message is shown
(defun rm-restrict-to-message (&optional show-all-hdrs-p)
  (restrict-buffer (if show-all-hdrs-p
		       (mark-pos (rm-get-msg-field rm-current-msg rm-msg-mark))
		     rm-current-msg-visible-start)
		   rm-current-msg-end))

;; Returns t if POS is the start of a message. Munges the regexp history
(defun rm-message-start-p (pos)
  (and (regexp-match-line mail-message-start pos nil t)
       (or (equal pos (buffer-start))
	   (regexp-match-line "^$" (prev-line 1 (match-start))))))

;; Return the start of the header following the header starting at POS
;; This returns nil to show that the header goes to the end of the
;; buffer or restriction
(defun rm-unfold-header (pos)
  ;; Be non-destructive!
  (setq pos (copy-pos pos))
  (next-line 1 pos)
  ;; A header is a single line followed by zero or more lines whose
  ;; first character is a SPC or TAB character [RFC-822]
  (while (and (< pos (restriction-end))
	      (regexp-match-line "^[ \t]+" pos))
    (next-line 1 pos))
  (if (and pos (< pos (restriction-end)))
      pos
    nil))

;; Delete the current message. Unless GO-BACKWARDS-P is t the next
;; message is displayed (unless there is no next message). NO-REDISPLAY-P
;; controls whether or not to display the new current message.
(defun rm-delete-current-message (&optional go-backwards-p no-redisplay-p)
  (unless rm-current-msg
    (error "No message to delete"))
  ;; When this hook returns t the message isn't deleted.
  (unless (eval-hook 'read-mail-delete-message-hook rm-current-msg)
    (let
	((inhibit-read-only t)
	 ;; Don't use rm-curr-msg-end, it may not be initialised.
	 (end (rm-current-message-end)))
      (unrestrict-buffer)
      (unless (equal end (buffer-end))
	;; Now this points to the first character of the next message
	(next-line 1 end))
      (delete-area (mark-pos (rm-get-msg-field rm-current-msg rm-msg-mark))
		   end)
      (if (and rm-before-msg-list
	       (or go-backwards-p (null rm-after-msg-list)))
	  (setq rm-current-msg (car rm-before-msg-list)
		rm-before-msg-list (cdr rm-before-msg-list)
		rm-current-msg-index (1- rm-current-msg-index))
	;; Even if this list is empty things will still work out
	(setq rm-current-msg (car rm-after-msg-list)
	      rm-after-msg-list (cdr rm-after-msg-list)))
      (setq rm-cached-msg-list 'invalid)
      (unless no-redisplay-p
	(rm-display-current-message)))))

;; Delete all messages in the list DEL-MSGS as efficiently as possible
(defun rm-delete-messages (del-msgs)
  ;; Need to delete del-msgs in the most efficient order, to
  ;; minimise the amount of list thrashing. The current method
  ;; isn't that great.
  ;; Having said that, it's a lot better than what could happen if we
  ;; just deleted messages in the order thrown at us by summary-execute
  (let
      ((old-curr-msg rm-current-msg))
    ;; 1. Delete the current message as long as it's in the list
    (while (and del-msgs rm-current-msg (memq rm-current-msg del-msgs))
      (setq del-msgs (delq rm-current-msg del-msgs))
      (rm-delete-current-message nil t))
    ;; 2. Work forwards down the rm-after-msg-list looking for
    ;; messages
    (when (and del-msgs rm-after-msg-list)
      (rm-move-forwards)
      (while (and del-msgs rm-after-msg-list)
	(if (memq rm-current-msg del-msgs)
	    (progn
	      (setq del-msgs (delq rm-current-msg del-msgs))
	      (rm-delete-current-message nil t))
	  (rm-move-forwards))))
    ;; 3. Work backwards down the rm-before-list
    (when (and del-msgs rm-before-msg-list)
      (rm-move-backwards)
      (while (and del-msgs rm-before-msg-list)
	(if (memq rm-current-msg del-msgs)
	    (progn
	      (setq del-msgs (delq rm-current-msg del-msgs))
	      (rm-delete-current-message t t))
	  (rm-move-backwards))))
    ;; If possible, try to display the original current message. Otherwise
    ;; leave it how it is..
    (if (or (eq old-curr-msg rm-current-msg)
	      (memq old-curr-msg rm-after-msg-list)
	      (memq old-curr-msg rm-before-msg-list))
	(rm-display-message old-curr-msg)
      (rm-display-current-message))))


;; Commands

(defun rm-next-message ()
  "Display the next message in the current mail folder."
  (interactive)
  (unless rm-after-msg-list
    (error "No more messages"))
  (rm-move-forwards)
  (rm-display-current-message))

(defun rm-previous-message ()
  "Display the previous message in the current mail folder."
  (interactive)
  (unless rm-before-msg-list
    (error "No previous message"))
  (rm-move-backwards)
  (rm-display-current-message))

(defun rm-toggle-all-headers ()
  "Toggles between showing invisible headers and not showing them for the
current message."
  (interactive)
  (rm-restrict-to-message (equal (restriction-start)
				 rm-current-msg-visible-start))
  ;; When expanding the restriction ensure that the new text is visible
  ;; now. Otherwise it can look as though nothing happened.
  (when (equal (restriction-start)
	       (mark-pos (rm-get-msg-field rm-current-msg rm-msg-mark)))
    (goto-char (buffer-start))))


;; Summary interface

(defvar rm-summary-keymap (copy-sequence summary-keymap))
(bind-keys rm-summary-keymap
  "n" 'rm-summary-next
  "p" 'rm-summary-previous
  "SPC" 'summary-select-item)

(defvar rm-summary-functions '((select . rm-summary-select-item)
			       (list . rm-summary-list)
			       (print . rm-summary-print-item)
			       (delete . rm-summary-delete-item)
			       (execute-end . rm-summary-execute-end))
  "Function vector for summary-mode.")

(defvar rm-summary-mail-buffer nil
  "The buffer whose folder is being summarised.")
(make-variable-buffer-local 'rm-summary-mail-buffer)

(defvar rm-summary-current-marked nil
  "The message with a `->' mark next to it.")
(make-variable-buffer-local 'rm-summary-current-marked)

(defvar rm-summary-msgs-to-delete nil
  "List of messages to be deleted. Build up during the execute phase of
the summary buffer.")
(make-variable-buffer-local 'rm-summary-msgs-to-delete)

(defun rm-summary ()
  "Display a summary of all messages in a separate view."
  (interactive)
  (if rm-summary-buffer
      (progn
	(set-current-view (other-view mail-summary-lines))
	(goto-buffer rm-summary-buffer)
	(summary-update)
	(rm-summary-update-current))
    (setq rm-summary-buffer (make-buffer (concat "*summary of "
						 (buffer-name) ?*)))
    (let
	((mail-buf (current-buffer)))
      (set-current-view (other-view mail-summary-lines))
      (goto-buffer rm-summary-buffer)
      (setq rm-summary-mail-buffer mail-buf)
      (summary-mode "Mail-Summary" rm-summary-functions rm-summary-keymap)
      (setq major-mode 'rm-summary-mode)
      (rm-summary-update-current))))

(defun rm-summary-mode ()
  "Mail Summary Mode:\n
Major mode for displaying a summary of a mail folder.")

(defun rm-summary-list ()
  (with-buffer rm-summary-mail-buffer
    (if (eq rm-cached-msg-list 'invalid)
	(setq rm-cached-msg-list
	      (if rm-current-msg
		  (nconc (reverse rm-before-msg-list)
			 (cons rm-current-msg
			       (copy-sequence rm-after-msg-list)))
		'()))
      rm-cached-msg-list)))

(defun rm-summary-print-item (item)
  (let
      ((pending-ops (summary-get-pending-ops item)))
    (format (current-buffer) "%s %c  %s %s %s "
	    (if (eq item (with-buffer rm-summary-mail-buffer rm-current-msg))
		(progn
		  (setq rm-summary-current-marked item)
		  "->")
	      "  ")
	    (if (memq 'delete pending-ops) ?D ?\ )
	    (rm-get-msg-field item rm-msg-day)
	    (rm-get-msg-field item rm-msg-month)
	    (rm-get-msg-field item rm-msg-year))
    (indent-to 20)
    (insert (or (rm-get-msg-field item rm-msg-from-name)
		(rm-get-msg-field item rm-msg-from-addr)))
    (insert " ")
    (indent-to 40)
    (insert (or (rm-get-msg-field item rm-msg-subject) ""))))

(defun rm-summary-select-item (item)
  (let
      ((mail-buf rm-summary-mail-buffer))
    (with-view (other-view)
      (goto-buffer mail-buf)
      (rm-display-message item))))

(defun rm-summary-delete-item (item)
  (setq rm-summary-msgs-to-delete (cons item rm-summary-msgs-to-delete)))

(defun rm-summary-execute-end ()
  (let
      ((del-msgs rm-summary-msgs-to-delete)
       (mail-buf rm-summary-mail-buffer))
    (setq rm-summary-msgs-to-delete nil)
    (with-view (other-view)
      (goto-buffer mail-buf)
      (rm-delete-messages del-msgs))))
    
(defun rm-summary-update-current ()
  (if (with-buffer rm-summary-mail-buffer rm-current-msg)
      (progn
	(summary-update-item rm-summary-current-marked)
	(summary-update-item (with-buffer rm-summary-mail-buffer
			       rm-current-msg))
	(summary-goto-item (with-buffer rm-summary-mail-buffer
			     rm-current-msg-index)))
    ;; No messages, call update to clear everything
    (summary-update)))

(defun rm-summary-next ()
  (interactive)
  (let
      ((mail-buf rm-summary-mail-buffer))
    (with-view (other-view)
      (goto-buffer mail-buf)
      (rm-next-message))))

(defun rm-summary-previous ()
  (interactive)
  (let
      ((mail-buf rm-summary-mail-buffer))
    (with-view (other-view)
      (goto-buffer mail-buf)
      (rm-previous-message))))
