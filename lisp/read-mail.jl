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
(require 'maildefs)
(require 'mail-headers)
(provide 'read-mail)


;; Configuration

(defvar rm-auto-next-message t
  "When t the next message will automatically be displayed when trying to
page past the limits of the current message.")

(defvar rm-move-after-deleting t
  "When t move to the next message after deleting the current message.")

(defvar rm-summary-format "%a %d/%m%10i%25F%36i%s"
  "A string defining the format of lines in mail summary buffers. It is
copied verbatim except for formatting directives introduced by percent
signs (%). Each directive consists of a percent character, an optional
numeric argument, and a single character specifying what should be
inserted in place of the format directive. These characters include:

	a	A 3-character attribute string, showing the status of
		the message
	d	The numeric day of the month when the message was sent
	f	The address of the first sender
	F	The name of address of the first sender
	i	Indent to column ARG (i.e. %20i => indent to column 20)
	m	The numeric month of the message's date
	M	The abbreviated month name of the date
	s	The subject line
	t	The hour and minute at which the message was sent
	T	The hour, minute, and second of the date
	%	Insert a percent character
	r	The name of the first recipient of the message
	y	The numeric year of the sending date

The list of formatting options can be extended by the variable
`rm-summary-print-functions'.")

(defvar rm-summary-print-functions nil
  "An alist of (CHARACTER . FUNCTION) defining extra formatting directives
for the rm-summary-format-string. The function is called as:
(FUNCTION MESSAGE-STRUCTURE ARG); it should insert whatever is required
at the current cursor position (no newlines).")

(defvar rm-saved-cache-tags '(sender-list recipient-list date-vector)
  "List of cache tags whose values should be saved in the headers of each
message (to improve performance when the folder is next loaded).")


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

(defvar rm-message-count nil
  "The number of messages in the current folder.")
(make-variable-buffer-local 'rm-message-count)

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
  "The buffer displaying the summary of this folder.")
(make-variable-buffer-local 'rm-summary-buffer)

(defvar rm-keymap (make-keylist)
  "Keymap for reading mail")
(bind-keys rm-keymap
  "n" 'rm-next-undeleted-message
  "p" 'rm-previous-undeleted-message
  "N" 'rm-next-message
  "P" 'rm-previous-message
  "t" 'rm-toggle-all-headers
  "SPC" 'rm-next-page
  "BS" 'rm-previous-page
  "h" 'rm-summary
  "d" 'rm-mark-message-deletion
  "Ctrl-d" 'rm-mark-message-deletion
  "x" '(rm-with-summary (summary-execute))
  "#" '(rm-with-summary (summary-execute))
  "g" 'rm-get-mail
  "k" 'rm-kill-subject
  "q" 'rm-save-and-quit
  "u" '(rm-with-summary (summary-unmark-item
			 (with-buffer rm-summary-mail-buffer
			   rm-current-msg)))
  "v" 'read-mail-folder
  "r" 'rm-reply
  "R" '(rm-reply t)
  "f" 'rm-followup
  "F" '(rm-followup t)
  "z" 'rm-forward
  "*" 'rm-burst-message
  "s" 'rm-output
  "|" 'rm-pipe-message)
  
(defvar rm-last-folder mail-folder-dir
  "File name of the most recently opened folder. Used as a default value for
the next prompt.")


;; Entry points

;;;###autoload
(defun read-mail ()
  "Read mail."
  (interactive)
  (read-mail-folder mail-default-folder))

;;;###autoload
(defun read-mail-folder (folder)
  "Read mail stored in the file FOLDER."
  (interactive (list (prompt-for-folder "Mail folder to open:"
					rm-last-folder)))
  (when (and (boundp 'rm-summary-mail-buffer) rm-summary-mail-buffer)
    ;; In summary
    (let
	((mail-view (get-buffer-view rm-summary-mail-buffer)))
      (set-current-view mail-view)))
  (if (or (file-name-absolute-p folder)
	  (file-exists-p (expand-file-name folder)))
      (setq folder (expand-file-name folder))
    (setq folder (expand-file-name (file-name-concat mail-folder-dir folder))))
  (when (find-file-read-only folder)
    ;; The current buffer is now the folder. Set up the major mode
    (setq rm-last-folder folder)
    (read-mail-mode)))

(defun read-mail-mode ()
  "Read-Mail Mode:\n
Major mode for viewing mail folders. Commands include:\n
  `n'			Display the next undeleted message.
  `p'			Display the previous undeleted message.
  `N', 'P'		Display the next or previous message, including
			 those that have been marked for deletion.
  `t'			Toggle between showing all headers and just
			 showing important headers in the current msg.
  `SPC'			Display the next page of the message.
  `BS'			Display the previous page of the message.
  `h'			Create/update the folder summary.
  `d', `Ctrl-d'		Mark the current message to be deleted.
  `k'			Mark that all messages with the same subject as
			 the current message should be deleted.
  `u'			Unmark the current message.
  `x', `#'		Delete marked messages.
  `g'			Get new mail.
  `v'			Visit a different folder.
  `q'			Quit.
  `r'			Reply to the current message.
  `R'			Reply, quoting the current message.
  `f'			Follow-up to the current message (reply including
			 all recipients of the original message).
  `F'			Follow-up, quoting the current message.
  `z'			Forward the current message to someone else.
  `s'			Save the current message to a different folder.
  `*'			Burst an RFC-934 or RFC-1153 digest message into
			 its constituent messages.
  `|'			Pipe the current message through a shell command."
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "Mail:"
	major-mode 'read-mail-mode
	major-mode-kill 'read-mail-mode-kill
	mode-comment-fun 'c-insert-comment
	;;ctrl-c-keymap c-mode-ctrl-c-keymap
	keymap-path (cons 'rm-keymap keymap-path))
  (call-hook 'read-mail-mode-hook)
  ;; Build the message list and display the current message
  (rm-build-message-lists)
  (rm-create-summary)
  (when (zerop (rm-get-mail))
    ;; No point doing this twice
    (rm-display-current-message))
  (when mail-display-summary
    (rm-summary)))

(defun read-mail-mode-kill ()
  (setq mode-name nil
	major-mode nil
	major-mode-kill nil
	;;ctrl-c-keymap nil
	keymap-path (delq 'rm-keymap keymap-path)))


;; Internal message structure

;; FLAGS is a list of symbols, including replied, unread, filed,
;; forwarded, anything else?
(defconst rm-msg-mark 0)
(defconst rm-msg-total-lines 1)
(defconst rm-msg-header-lines 2)
(defconst rm-msg-flags 3)
(defconst rm-msg-cache 4)
(defconst rm-msg-struct-size 5)

(defmacro rm-set-msg-field (msg field value)
  (list 'aset msg field value))

(defmacro rm-get-msg-field (msg field)
  (list 'aref msg field))

(defmacro rm-make-msg ()
  '(make-vector rm-msg-struct-size))

(defun rm-set-flag (msg flag)
  (unless (memq flag (rm-get-msg-field msg rm-msg-flags))
    (rm-set-msg-field msg rm-msg-flags
		      (cons flag (rm-get-msg-field msg rm-msg-flags)))
    (rm-invalidate-summary msg)))

(defun rm-clear-flag (msg flag)
  (when (memq flag (rm-get-msg-field msg rm-msg-flags))
    (rm-set-msg-field msg rm-msg-flags
		      (delq flag (rm-get-msg-field msg rm-msg-flags)))
    (rm-invalidate-summary msg)))

(defmacro rm-test-flag (msg flag)
  (list 'memq flag (list 'rm-get-msg-field msg rm-msg-flags)))

;; Call like (rm-cached-form MSG TAG FORM) instead of just FORM, to
;; cache its value in MSG under key TAG
(defmacro rm-cached-form (msg tag form)
  `(let
       ((_tem_ (assq ,tag (rm-get-msg-field ,msg rm-msg-cache))))
     (if _tem_
	 (cdr _tem_)
       (setq _tem_ ,form)
       (rm-set-msg-field ,msg rm-msg-cache
			 (cons (cons ,tag _tem_)
			       (rm-get-msg-field ,msg rm-msg-cache)))
       _tem_)))
(put 'rm-cached-form 'lisp-indent 2)

;; Returns t if TAG is cached by MSG
(defmacro rm-tag-cached-p (msg tag)
  `(assq ,tag (rm-get-msg-field ,msg rm-msg-cache)))

;; If TAG is cached by MSG, remove its cached value
(defun rm-invalidate-tag (msg tag)
  (let
      ((cell (rm-tag-cached-p msg tag)))
    (when cell
      (rm-set-msg-field msg rm-msg-cache
			(delq cell (rm-get-msg-field msg rm-msg-cache))))))


;; Message structures and list manipulation

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

;; Create the lists of messages. The buffer should be unrestricted when
;; calling this. Returns the current message.
(defun rm-build-message-lists ()
  (let
      ;; If we scan the buffer from start to end we can create the
      ;; list in reverse order, just what we want.
      ((pos (start-of-buffer))
       (msgs nil)
       (count 0))
    (while (and pos (rm-message-start-p pos) (< pos (end-of-buffer)))
      (setq msgs (cons (rm-build-message-struct pos) msgs)
	    count (1+ count)
	    pos (forward-line (rm-get-msg-field (car msgs)
						rm-msg-total-lines) pos)))
    ;; Okay, the current message is the last in the buffer. There's
    ;; no messages after the current one, all the rest go before.
    (setq rm-current-msg (car msgs)
	  rm-before-msg-list (cdr msgs)
	  rm-after-msg-list '()
	  rm-current-msg-index (1- count)
	  rm-cached-msg-list 'invalid
	  rm-message-count count)
    rm-current-msg))

;; Parse one message and return a message structure. The buffer
;; should be unrestricted
(defun rm-build-message-struct (start)
  (let
      ((end (re-search-forward "^$" start))
       (msg (rm-make-msg))
       pos tem)
    (restrict-buffer start (or end (end-of-buffer)))
    (rm-set-msg-field msg rm-msg-mark (make-mark start))
    (rm-set-msg-field msg rm-msg-header-lines (- (pos-line (restriction-end))
						 (pos-line start)))
    (when (setq pos (mail-find-header "^X-Jade-Flags-v1" start))
      (rm-set-msg-field msg rm-msg-flags (read (cons (current-buffer) pos))))
    (when (setq pos (mail-find-header "^X-Jade-Cache-v1" start))
      (rm-set-msg-field msg rm-msg-cache (read (cons (current-buffer) pos))))
    (when (mail-find-header "Replied" start)
      ;; MH annotates replied messages like this
      (rm-set-flag msg 'replied))
    (unrestrict-buffer)
    ;; Find the total number of lines in the message
    (if (null end)
	(setq pos (end-of-buffer))
      ;; Find the start of the next message
      (setq pos (forward-line 1 start))
      (while (and pos (setq pos (re-search-forward mail-message-start pos))
		  (not (rm-message-start-p pos)))
	(setq pos (forward-line 1 pos))))
    (rm-set-msg-field msg rm-msg-total-lines (- (pos-line
						 (or pos (end-of-buffer)))
						(pos-line start)))
    msg))

;; Returns t if POS is the start of a message. Munges the regexp history
(defun rm-message-start-p (pos)
  (and (looking-at mail-message-start pos)
       (or (equal pos (start-of-buffer))
	   (looking-at "\n\n" (forward-char -2 (match-start))))))

;; Returns the position of the start of the last line in the current message.
;; Works no matter what the restriction is set to.
(defun rm-message-end (&optional msg)
  (unless msg (setq msg rm-current-msg))
  (pos 0 (+ (pos-line (mark-pos (rm-get-msg-field msg rm-msg-mark)))
	    (rm-get-msg-field msg rm-msg-total-lines) -1)))

;; Updates the flags embedded in the message headers. Leaves the buffer
;; unrestricted.
(defun rm-update-flags ()
  (let
      ((msg-lists (list rm-before-msg-list
			(cons rm-current-msg nil)
			rm-after-msg-list))
       (inhibit-read-only t)
       list msg start)
    (while msg-lists
      (setq list (car msg-lists)
	    msg-lists (cdr msg-lists))
      (while list
	(when (setq msg (car list))
	  (setq start (mark-pos (rm-get-msg-field msg rm-msg-mark)))
	  (unrestrict-buffer)
	  (when (re-search-forward "^$" start)
	    (restrict-buffer start (match-end)))
	  (let
	      ((lines-added 0)
	       (print-escape t)
	       tem)
	    ;; First put flags into X-Jade-Flags-v1 header
	    (if (re-search-forward "^X-Jade-Flags-v1[\t ]*:[\t ]*(.*)$"
				   start nil t)
		(progn
		  (setq tem (match-start 1))
		  (delete-area (match-start 1) (match-end 1)))
	      (setq tem (forward-char -1 (insert "X-Jade-Flags-v1: \n"
						 (mail-unfold-header tem)))
		    lines-added (1+ lines-added)))
	    (prin1 (rm-get-msg-field msg rm-msg-flags)
		   (cons (current-buffer) tem))
	    ;; Then selected cache items into X-Jade-Cache-v1 header
	    (if (re-search-forward "^X-Jade-Cache-v1[\t ]*:[\t ]*(.*)$"
				   start nil t)
		(progn
		  (setq tem (match-start 1))
		  (delete-area (match-start 1) (match-end 1)))
	      (setq tem (forward-char -1 (insert "X-Jade-Cache-v1: \n"
						 (mail-unfold-header tem)))
		    lines-added (1+ lines-added)))
	    (prin1 (delete-if #'(lambda (x)
				  (null (memq (car x) rm-saved-cache-tags)))
			      (copy-sequence
			       (rm-get-msg-field msg rm-msg-cache)))
		   (cons (current-buffer) tem))
	    ;; Adjust total-lines and header-lines message attrs
	    (rm-set-msg-field msg rm-msg-header-lines
			      (+ (rm-get-msg-field msg rm-msg-header-lines)
				 lines-added))
	    (rm-set-msg-field msg rm-msg-total-lines
			      (+ (rm-get-msg-field msg rm-msg-total-lines)
				 lines-added))))
	(setq list (cdr list))))
    (unrestrict-buffer)))

;; Call (mail-get-header HEADER LISTP NO-COMMA-SEP), with the current
;; restriction set to the headers of MSG
(defun rm-get-msg-header (msg header &optional listp no-comma-sep)
  (with-buffer (mark-file (rm-get-msg-field msg rm-msg-mark))
    (save-restriction
      (restrict-buffer (mark-pos (rm-get-msg-field msg rm-msg-mark))
		       (pos 0 (+ (rm-get-msg-field msg rm-msg-header-lines)
				 (pos-line (mark-pos (rm-get-msg-field
						      msg rm-msg-mark))))))
      (mail-get-header header listp no-comma-sep))))

;; Shortcuts to some common headers
(defun rm-get-senders (msg)
  (rm-cached-form msg 'sender-list
    (mapcar 'mail-parse-address (rm-get-msg-header msg "(From|Sender)" t))))

(defun rm-get-recipients (msg)
  (rm-cached-form msg 'recipient-list
    (mapcar 'mail-parse-address (rm-get-msg-header msg "(To|Cc|Bcc)" t))))

(defun rm-get-subject (msg)
  (rm-cached-form msg 'subject (rm-get-msg-header msg "Subject")))

(defun rm-get-date-vector (msg)
  (rm-cached-form msg 'date-vector
    (let
	((string (rm-get-msg-header msg "Date")))
      (if string
	  (mail-parse-date string)
	nil))))


;; Displaying messages

;; Display the current message
(defun rm-display-current-message (&optional no-summary-update)
  (unrestrict-buffer)
  (when rm-current-msg
    (let
	((header-start (mark-pos (rm-get-msg-field rm-current-msg
						   rm-msg-mark))))
      (unless (looking-at mail-message-start header-start)
	(error "Position isn't start of header: %s" header-start))
      (let
	  ((end-of-hdrs (re-search-forward "^$" header-start))
	   (inhibit-read-only t))
	(setq rm-current-msg-body end-of-hdrs)
	;; Just operate on the headers
	(restrict-buffer header-start end-of-hdrs)
	;; First of all, move all visible headers after non-visible ones
	(setq rm-current-msg-visible-start (rm-coalesce-visible-headers))
	;; Look for a header to highlight
	(when (re-search-forward mail-highlighted-headers header-start nil t)
	  (mark-block (match-start 1)
		      (forward-char -1 (or (mail-unfold-header (match-start 1))
					   (end-of-buffer)))))
	(unrestrict-buffer)
	(setq rm-current-msg-end (rm-message-end))
	(goto end-of-hdrs)
	(rm-restrict-to-message)
	(rm-clear-flag rm-current-msg 'unread)
	(rm-fix-status-info)
	;; Called when the current restriction is about to be
	;; displayed
	(call-hook 'read-mail-display-message-hook (list rm-current-msg)))))
  (unless no-summary-update
    ;; Fix the summary buffer if it exists
    (rm-with-summary
     (rm-summary-update-current))))

;; Set the minor-mode-names list to reflect the current status
(defun rm-fix-status-info ()
  (setq minor-mode-names
	(cons (format nil "%d/%d%s"
		      (1+ rm-current-msg-index)
		      rm-message-count
		      (if (memq 'delete (with-buffer rm-summary-buffer
					  (summary-get-pending-ops
					   (with-buffer rm-summary-mail-buffer
					     rm-current-msg))))
			  " deleted"
			""))
	      (mapcar 'symbol-name
		      (rm-get-msg-field rm-current-msg
					rm-msg-flags)))))

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
      ((first-visible (re-search-forward mail-visible-headers
					(start-of-buffer) nil t))
       (current-hdr first-visible)
       (next-hdr first-visible))
    (while (and current-hdr (< current-hdr (end-of-buffer)))
      ;; Move over all visible headers from CURRENT-HDR until
      ;; reaching an invisible one
      (setq next-hdr current-hdr)
      (while (and next-hdr (< next-hdr (end-of-buffer))
		  (looking-at mail-visible-headers next-hdr nil t))
	(setq next-hdr (mail-unfold-header next-hdr)))
      ;; Now we have a block of visible headers from CURRENT-HDR to
      ;; NEXT-HDR (but not including NEXT-HDR). Next find the following
      ;; visible header to delimit the block of invisible ones
      (setq current-hdr next-hdr)
      (while (and next-hdr (< next-hdr (end-of-buffer))
		  (not (looking-at mail-visible-headers next-hdr nil t)))
	(setq next-hdr (mail-unfold-header next-hdr)))
      ;; Now we have a block of invisible headers, CURRENT-HDR to NEXT-HDR
      ;; Move them to before the FIRST-VISIBLE-HDR, updating this to
      ;; point to the end of the insertion
      (unless next-hdr
	(setq next-hdr (end-of-buffer)))
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


;; Deleting messages

;; Delete the current message. Unless GO-BACKWARDS-P is t the next
;; message is displayed (unless there is no next message). NO-REDISPLAY-P
;; controls whether or not to display the new current message.
(defun rm-delete-current-message (&optional go-backwards-p no-redisplay-p)
  (unless rm-current-msg
    (error "No message to delete"))
  ;; When this hook returns t the message isn't deleted.
  (unless (call-hook 'read-mail-delete-message-hook (list rm-current-msg) 'or)
    (let
	((inhibit-read-only t)
	 ;; Don't use rm-curr-msg-end, it may not be initialised.
	 (end (rm-message-end)))
      (unrestrict-buffer)
      (unless (equal end (end-of-buffer))
	;; Now this points to the first character of the next message
	(setq end (forward-line 1 end)))
      (delete-area (mark-pos (rm-get-msg-field rm-current-msg rm-msg-mark))
		   end)
      (setq rm-message-count (1- rm-message-count))
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
;; rm-display-current-msg should be called after this has returned
(defun rm-delete-messages (del-msgs)
  ;; Need to delete del-msgs in the most efficient order, to
  ;; minimise the amount of list thrashing. The current method
  ;; isn't that great.
  ;; Having said that, it's a lot better than what could happen if we
  ;; just deleted messages in the order thrown at us by summary-execute
  (let
      ((old-curr-msg rm-current-msg)
       skip-count)
    ;; 1. Delete the current message as long as it's in the list
    (while (and del-msgs rm-current-msg (memq rm-current-msg del-msgs))
      (setq del-msgs (delq rm-current-msg del-msgs))
      (rm-delete-current-message nil t))
    ;; 2. Work forwards down the rm-after-msg-list looking for
    ;; messages
    (when (and del-msgs rm-after-msg-list)
      (rm-move-forwards)
      (setq skip-count 1)
      (while (and del-msgs rm-after-msg-list)
	(if (memq rm-current-msg del-msgs)
	    (progn
	      (setq del-msgs (delq rm-current-msg del-msgs))
	      (rm-delete-current-message nil t))
	  (rm-move-forwards)
	  (setq skip-count (1+ skip-count))))
      (when (and rm-current-msg (memq rm-current-msg del-msgs))
	(rm-delete-current-message nil t)
	(setq skip-count (1- skip-count)))
      ;; Then back to the old current message
      (while (> skip-count 0)
	(rm-move-backwards)
	(setq skip-count (1- skip-count))))
    ;; 3. Work backwards down the rm-before-list
    (when (and del-msgs rm-before-msg-list)
      (rm-move-backwards)
      (setq skip-count 1)
      (while (and del-msgs rm-before-msg-list)
	(if (memq rm-current-msg del-msgs)
	    (progn
	      (setq del-msgs (delq rm-current-msg del-msgs))
	      (rm-delete-current-message t t))
	  (rm-move-backwards)
	  (setq skip-count (1+ skip-count))))
      (when (and rm-current-msg (memq rm-current-msg del-msgs))
	(rm-delete-current-message nil t)
	(setq skip-count (1- skip-count)))
      ;; Then forwards to the old current message
      (while (> skip-count 0)
	(rm-move-forwards)
	(setq skip-count (1- skip-count))))))


;; Getting mail from inbox

;; Ensure that it's okay to insert a new message at the current cursor
;; position (must be preceded by the start of the buffer, or "\n\n")
;; Leaves the cursor at the position to insert at.
;; The buffer should be unrestricted
(defun rm-enforce-msg-separator ()
  (unless (or (equal (cursor-pos) (start-of-buffer))
	      (looking-at "\n\n" (forward-char -1)))
    (if (= (get-char) ?\n)
	(insert "\n")
      (insert "\n\n"))))
  
;; Insert the contents of file INBOX at the end of the current folder, fix
;; the message lists and display the first new message. Returns the number
;; of messages read if it's okay to try and read more inboxes, nil if it's
;; best not to.
(defun rm-append-inbox (inbox)
  (cond
   ((not (file-exists-p inbox))
    (error "Inbox file doesn't exist" inbox))
   ((zerop (file-size inbox))
    (message (concat "No new mail in " inbox))
    nil)
   ((or (null movemail-program)
	(not (file-exists-p movemail-program)))
    (error "The variable `movemail-program' is invalid"))
   (t
    (let*
	((tofile (file-name-concat (file-name-directory (buffer-file-name))
				   (concat ".newmail-"
					   (file-name-nondirectory inbox))))
	 (temp-buffer (make-buffer "*movemail-output*"))
	 (proc (make-process temp-buffer)))
      (if (zerop (call-process proc nil movemail-program inbox tofile))
	  ;; No errors
	  (progn
	    (destroy-buffer temp-buffer)
	    (unrestrict-buffer)
	    (let
		((inhibit-read-only t)
		 (keep-going t)
		 (count 0)
		 start pos msgs)
	      (while rm-after-msg-list
		(rm-move-forwards))
	      ;; Ensure that there's a blank line at the end of the buffer
	      (goto (end-of-buffer))
	      (rm-enforce-msg-separator)
	      (setq start (cursor-pos))
	      (insert-file-contents tofile)
	      (condition-case nil
		  (progn
		    ;; Try to save the folder..
		    (save-file)
		    ;; Don't delete the temporary file unless the folder
		    ;; was saved properly
		    (delete-file tofile))
		(error
		 (message (concat "Couldn't save folder; new messages left in "
				  tofile) t)
		 (setq keep-going nil)))
	      (setq pos (end-of-buffer))
	      (while (and pos (>= pos start)
			  (setq pos (re-search-backward mail-message-start pos)))
		(when (rm-message-start-p pos)
		  (setq msgs (cons (rm-build-message-struct pos) msgs)
			count (1+ count))
		  (rm-set-flag (car msgs) 'unread))
		(setq pos (forward-line -1 pos)))
	      (when msgs
		(when rm-current-msg
		  (setq rm-before-msg-list (cons rm-current-msg
						 rm-before-msg-list)
			rm-current-msg-index (1+ rm-current-msg-index)))
		(setq rm-current-msg (car msgs)
		      rm-after-msg-list (cdr msgs)
		      rm-message-count (+ rm-message-count count)
		      rm-cached-msg-list 'invalid))
	      (and keep-going count)))
	;; Errors
	(goto-buffer temp-buffer)
	(error "Couldn't move mail" inbox tofile))))))

;; Try and get new mail for the current folder. Returns the number of
;; messages actually read
(defun rm-get-mail ()
  (interactive)
  (let
      ((inboxes (mail-find-inboxes (buffer-file-name)))
       (old-msg rm-current-msg)
       (count 0)
       (this-ret 0)
       this)
    (while (and inboxes (numberp this-ret))
      (setq this (car inboxes)
	    inboxes (cdr inboxes))
      (if (or (file-name-absolute-p this)
	      (file-exists-p (expand-file-name this)))
	  (setq this (expand-file-name this))
	(setq this (expand-file-name (file-name-concat mail-folder-dir this))))
      (if (file-exists-p this)
	  (progn
	    (setq this-ret (rm-append-inbox this))
	    (when (and (numberp this-ret) (> this-ret 0))
	      (when (zerop count)
		;; If this is the first new message we want to display it
		;; afterwards
		(setq old-msg rm-current-msg))
	      (setq count (+ count this-ret))))
	(format t "Spool file %s doesn't exist" this))
      (when (numberp this-ret)
        (format t "Got %d new messages" count)))
    (rm-with-summary
     (summary-update))
    (rm-display-message old-msg)
    count))


;; Summary interface

(defmacro rm-command-with-folder (command)
  (list 'rm-with-folder
	(list 'call-command command 'current-prefix-arg)))

(defmacro rm-command-in-folder (command)
  (list 'rm-in-folder
	(list 'call-command command 'current-prefix-arg)))

(defvar rm-summary-keymap (copy-sequence summary-keymap))
(bind-keys rm-summary-keymap
  "n" '(rm-command-with-folder 'rm-next-undeleted-message)
  "p" '(rm-command-with-folder 'rm-previous-undeleted-message)
  "N" '(rm-command-with-folder 'rm-next-message)
  "P" '(rm-command-with-folder 'rm-previous-message)
  "SPC" '(rm-command-with-folder 'rm-next-page)
  "BS" '(rm-command-with-folder 'rm-previous-page)
  "t" '(rm-command-with-folder 'rm-toggle-all-headers)
  "g" '(rm-command-with-folder 'rm-get-mail)
  "k" '(rm-command-with-folder 'rm-kill-subject)
  "q" '(rm-command-with-folder 'rm-save-and-quit)
  "v" '(rm-command-with-folder 'read-mail-folder)
  "r" '(rm-command-in-folder 'rm-reply)
  "R" '(rm-command-in-folder '(rm-reply t))
  "f" '(rm-command-in-folder 'rm-followup)
  "F" '(rm-command-in-folder '(rm-followup t))
  "z" '(rm-command-in-folder 'rm-forward)
  "*" '(rm-command-with-folder 'rm-burst-message)
  "s" '(rm-command-with-folder 'rm-output)
  "|" '(rm-command-with-folder 'rm-pipe-message))

(defvar rm-summary-functions '((select . rm-summary-select-item)
			       (list . rm-summary-list)
			       (print . rm-summary-print-item)
			       (current . rm-summary-current-item)
			       (delete . rm-summary-delete-item)
			       (execute-end . rm-summary-execute-end)
			       (after-marking . rm-summary-after-marking)
			       (after-update . rm-summary-after-update))
  "Function vector for summary-mode.")

(defvar rm-summary-mail-buffer nil
  "The buffer whose folder is being summarised.")
(make-variable-buffer-local 'rm-summary-mail-buffer)

(defvar rm-summary-msgs-to-delete nil
  "List of messages to be deleted. Built up during the execute phase of
the summary buffer.")
(make-variable-buffer-local 'rm-summary-msgs-to-delete)


;; Macros for switching between the summary and mail views

;; When called from a folder buffer, will execute FORMS in the summary
;; buffer. If a view of the summary exists will be in that. Note that
;; FORMS should be as small as poss. since it's expanded twice.
(defmacro rm-with-summary (&rest forms)
  (list 'let
	'((view (get-buffer-view rm-summary-buffer)))
	(list 'if 'view
	      (cons 'with-view (cons 'view forms))
	      (cons 'with-buffer (cons 'rm-summary-buffer forms)))))

;; When called from a summary buffer, installs the summary's mail buffer
;; and executes FORMS.
(defmacro rm-with-folder (&rest forms)
  (list 'let
	'((view (get-buffer-view rm-summary-mail-buffer)))
	(list 'if 'view
	      (cons 'with-view (cons 'view forms))
	      (cons 'with-buffer (cons 'rm-summary-mail-buffer forms)))))

;; Switch to the buffer containing the folder and execute FORMS. Don't
;; switch back afterwards
(defmacro rm-in-folder (&rest forms)
  (cons 'progn
	(cons '(let
		   ((view (get-buffer-view rm-summary-mail-buffer)))
		 (if view
		     (set-current-view view)
		   (goto-buffer rm-summary-mail-buffer)))
	      forms)))

;; Create a summary buffer for the current buffer, and return it. Installs
;; it in rm-summary-buffer as well.
(defun rm-create-summary ()
  (unless rm-summary-buffer
    (setq rm-summary-buffer (make-buffer (concat "*summary of "
						 (buffer-name) ?*)))
    (let
	((mail-buf (current-buffer)))
      (with-buffer rm-summary-buffer
	(setq rm-summary-mail-buffer mail-buf
	      truncate-lines t)
	(call-hook 'read-mail-summary-mode-hook)
	(summary-mode "Mail-Summary" rm-summary-functions rm-summary-keymap)
	(setq major-mode 'read-mail-mode)))))


;; Summary mechanics

(defun rm-summary (&optional dont-update)
  "Display a summary of all messages in a separate view."
  (interactive)
  (rm-configure-views rm-summary-buffer (current-buffer))
  (unless dont-update
    (summary-update)
    (rm-summary-update-current)))

;; Configure the window to display the summary in one view, and the
;; mail buffer in the other. Return the view displaying the mail buffer
;; SUMMARY-BUFFER and MAIL-BUFFER are the buffers to display in the two
;; views
(defun rm-configure-views (summary-buffer mail-buffer)
  (let
      (mail-view summary-view)
    (if (= (window-view-count) 2)
	;; Single view + minibuf
	(setq summary-view (current-view)
	      mail-view (open-view))
      (let
	  ((orig (window-view-list)))
	(setq summary-view (car orig)
	      mail-view (nth 1 orig))
	(when (> (window-view-count) 3)
	  (mapc #'(lambda (v)
		    (unless (minibuffer-view-p v)
		      (close-view v)))
		(nthcdr 2 orig)))))
    (condition-case nil
	(if (eq mail-display-summary 'bottom)
	    ;; Summary at bottom
	    (progn
	      (setq mail-view (prog1
				  summary-view
				(setq summary-view mail-view)))
	      (set-view-dimensions mail-view nil (- (cdr (window-dimensions))
						    mail-summary-lines
						    3)))
	  (set-view-dimensions summary-view nil mail-summary-lines))
      ;; In case there's not enough room
      (window-error))
    (set-current-view summary-view)
    (goto-buffer summary-buffer)
    (with-view mail-view
      (goto-buffer mail-buffer))
    mail-view))

(defun rm-summary-list ()
  (rm-with-folder
   (if (eq rm-cached-msg-list 'invalid)
       (setq rm-cached-msg-list
	     (if rm-current-msg
		 (nconc (reverse rm-before-msg-list)
			(cons rm-current-msg
			       (copy-sequence rm-after-msg-list)))
	       '()))
     rm-cached-msg-list)))

(defun rm-summary-format-item (item)
  (let
      ((point 0)
       (date (rm-get-date-vector item))
       (pending-ops (summary-get-pending-ops item))
       (len (length rm-summary-format))
       (field-fun #'(lambda (s)
		      (if (null arg)
			  (insert s)
			(let
			    ((len (length s)))
			  (if (> arg len)
			      (progn
				(insert s)
				(insert (make-string (- arg len))))
			    (insert (substring s 0 (- arg 2)))
			    (insert ".."))))))
       char fun arg)
    (while (and (< point len)
		(string-match "%([0-9]*)[^0-9]" rm-summary-format point))
      (insert (substring rm-summary-format point (match-start)))
      (setq arg (if (/= (match-start 1) (match-end 1))
		    (read-from-string (substring rm-summary-format
						 (match-start 1)
						 (match-end 1)))
		  nil)
	    char (aref rm-summary-format (match-end 1))
	    point (match-end))
      (if (setq fun (cdr (assq char rm-summary-print-functions)))
	  (funcall fun item arg pending-ops)
	(cond
	 ((= char ?a)
	  (format (current-buffer) "%c%c%c"
		  (cond
		   ((rm-test-flag item 'unread) ?U)
		   ((memq 'delete pending-ops) ?D)
		   (t ? ))
		  (cond
		   ((rm-test-flag item 'replied) ?R)
		   ((rm-test-flag item 'forwarded) ?Z)
		   (t ? ))
		  (if (rm-test-flag item 'filed) ?F ?\ )))
	 ((= char ?d)
	  (when date
	    (format (current-buffer) "%d" (or (aref date mail-date-day) ""))))
	 ((= char ?f)
	  (let
	      ((from (car (rm-get-senders item))))
	    (when (car from)
	      (funcall field-fun (car from)))))
	 ((= char ?F)
	  (let*
	      ((from (car (rm-get-senders item))))
	    (funcall field-fun (or (cdr from) (car from) ""))))
	 ((= char ?i)
	  (indent-to arg))
	 ((= char ?m)
	  (when date
	    (format (current-buffer) "%d"
		    (or (aref date mail-date-month) ""))))
	 ((= char ?M)
	  (when date
	    (insert (or (aref date mail-date-month-abbrev) ""))))
	 ((= char ?s)
	  (funcall field-fun (or (rm-get-subject item) "")))
	 ((= char ?t)
	  (funcall field-fun (if date
				 (format nil "%d:%d"
					 (aref date mail-date-hour)
					 (aref date mail-date-minute)) "")))
	 ((= char ?T)
	  (funcall field-fun (if date (format nil "%d:%d:%d"
					      (aref date mail-date-hour)
					      (aref date mail-date-minute))
			       "")))
	 ((= char ?%)
	  (insert "%"))
	 ((= char ?r)
	  (let
	      ((to (car (rm-get-recipients item))))
	    (funcall field-fun (or (cdr to) (car to) ""))))
	 ((= char ?y)
	  (when date
	    (format (current-buffer) "%d"
		    (or (aref date mail-date-year) "")))))))
    (insert (substring rm-summary-format point))
    (copy-area (start-of-line) (cursor-pos))))

(defun rm-summary-print-item (item)
  ;; Cache the summary line with the summary buffer as the tag
  (let
      (value dont-insert)
    (setq value (rm-cached-form item (current-buffer)
		  (progn
		    (setq dont-insert t)
		    (rm-summary-format-item item))))
    (unless dont-insert
      (insert value))))

;; Delete all cached summary lines for MSG
(defun rm-invalidate-summary (msg)
  (rm-set-msg-field msg rm-msg-cache
		    (delete-if #'(lambda (x) (bufferp (car x)))
			       (rm-get-msg-field msg rm-msg-cache))))

(defun rm-summary-select-item (item)
  (with-view (rm-configure-views (current-buffer) rm-summary-mail-buffer)
    (rm-display-message item)))
  
(defun rm-summary-current-item ()
  (with-buffer rm-summary-mail-buffer
    rm-current-msg-index))

(defun rm-summary-delete-item (item)
  (setq rm-summary-msgs-to-delete (cons item rm-summary-msgs-to-delete)))

(defun rm-summary-execute-end ()
  (let
      ((del-msgs rm-summary-msgs-to-delete))
    (setq rm-summary-msgs-to-delete nil)
    (with-view (rm-configure-views (current-buffer) rm-summary-mail-buffer)
      (rm-delete-messages del-msgs)
      ;; After this function returns "summary-update" is called
      (rm-display-current-message t))))

(defun rm-summary-update-current ()
  (let
      (msg index)
    (with-buffer rm-summary-mail-buffer
      (setq index rm-current-msg-index
	    msg rm-current-msg))
    (if msg
	(progn
	  (summary-update-item msg)
	  (summary-goto-item index))
      ;; No messages, call update to clear everything
      (summary-update))))

(defun rm-summary-after-marking (msg)
  (rm-invalidate-summary msg)
  (if (and (eq (with-buffer rm-summary-mail-buffer rm-current-msg) msg)
	   rm-move-after-deleting)
      (when (with-buffer rm-summary-mail-buffer rm-after-msg-list)
	(rm-with-folder
	 (rm-next-undeleted-message 1)))
    (when (/= (summary-current-index)
	      (with-buffer rm-summary-mail-buffer rm-message-count))
      (summary-next-item 1))))

(defun rm-summary-after-update ()
  (let
      (msg index)
    (with-buffer rm-summary-mail-buffer
      (setq msg rm-current-msg
	    index rm-current-msg-index))
    (when msg
      (summary-highlight-index index))))


;; Commands, these must only be called from the folder buffer, *not*
;; from the summary.

(defun rm-next-message (&optional count skip-deleted)
  "Display the next message in the current mail folder."
  (interactive "p")
  (unless count
    (setq count 1))
  (let
      ((original-message rm-current-msg))
    (while (> count 0)
      (unless rm-after-msg-list
	(when original-message
	  (rm-make-message-current original-message))
	(error "No more messages"))
      (rm-move-forwards)
      (when (or (not skip-deleted)
		(not (memq 'delete (with-buffer rm-summary-buffer
				     (summary-get-pending-ops
				      (with-buffer rm-summary-mail-buffer
					rm-current-msg))))))
	(setq count (1- count))))
    (while (< count 0)
      (unless rm-before-msg-list
	(when original-message
	  (rm-make-message-current original-message))
	(error "No previous message"))
      (rm-move-backwards)
      (when (or (not skip-deleted)
		(not (memq 'delete (with-buffer rm-summary-buffer
				     (summary-get-pending-ops
				      (with-buffer rm-summary-mail-buffer
					rm-current-msg))))))
	(setq count (1+ count)))))
  (rm-display-current-message))

(defun rm-previous-message (&optional count skip-deleted)
  "Display the previous message in the current mail folder."
  (interactive "p")
  (rm-next-message (- (or count 1)) skip-deleted))

(defun rm-next-undeleted-message (&optional count)
  "Display the next undeleted message."
  (interactive "p")
  (rm-next-message count t))

(defun rm-previous-undeleted-message (&optional count)
  "Display the previous undeleted message."
  (interactive "p")
  (rm-previous-message count t))

(defun rm-next-page ()
  "Display the next page in the current message."
  (interactive)
  (if (>= (cursor-pos) (end-of-buffer))
      (when rm-auto-next-message
	(rm-next-undeleted-message))
    (next-screen)))

(defun rm-previous-page ()
  "Display the previous page in the current message."
  (interactive)
  (if (<= (cursor-pos) (start-of-buffer))
      (when rm-auto-next-message
	(rm-previous-message))
    (prev-screen)))

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
    (goto (start-of-buffer))))

(defun rm-mark-message-deletion ()
  "Marks that the current message should be deleted."
  (interactive)
  (rm-with-summary
   (summary-mark-delete))
  (rm-fix-status-info))

(defun rm-kill-subject ()
  "Marks all messages with the same subject as the current message as being
ready for deletion."
  (interactive)
  (let*
      ((kill-subject (rm-get-subject rm-current-msg)))
    (mapc #'(lambda (m)
		    (when (string= kill-subject (rm-get-subject m))
		      (with-buffer rm-summary-buffer
			(summary-mark-delete m))))
	  (append rm-before-msg-list
		  (and rm-current-msg (list rm-current-msg))
		  rm-after-msg-list))))

(defun rm-pipe-message (command &optional ignore-headers)
  "Pipes all of the current message through the shell command COMMAND (unless
IGNORE-HEADERS is non-nil, in which case only the body of the message is
used). All output is left in the `*shell output*' buffer. When called
interactively, COMMAND is prompted for, and IGNORE-HEADERS takes its value
from the prefix argument."
  (interactive "sShell command on message:\nP")
  (save-restriction
    (let
	((start (if ignore-headers
		    rm-current-msg-body
		  (mark-pos (rm-get-msg-field rm-current-msg rm-msg-mark)))))
      (shell-command-on-area command start rm-current-msg-end))))

(defun rm-save-and-quit ()
  "Quit from the mail reading subsystem. The current folder will be saved
automatically."
  (interactive)
  (let
      ((buffer (current-buffer)))
    (rm-update-flags)
    (when (save-file)
      (rm-quit-no-save t)
      (kill-buffer buffer))))

(defun rm-quit-no-save (&optional already-saved)
  "Quit from the mail reading subsystem without saving the current folder. The
buffer will not be deleted, so it may be saved later."
  (interactive)
  (let
      ((summary-view (get-buffer-view rm-summary-buffer)))
    (unless already-saved
      (rm-update-flags))
    (when summary-view
      (close-view summary-view))
    (kill-buffer rm-summary-buffer)
    (destroy-buffer rm-summary-buffer)
    (setq rm-current-msg nil
	  rm-before-msg-list nil
	  rm-after-msg-list nil
	  rm-cached-msg-list nil
	  rm-current-msg-index nil
	  rm-summary-buffer nil)
    (when (buffer-modified-p)
      (message (concat  "Folder " (buffer-name)
			" contains unsaved changes!")))
    (bury-buffer (current-buffer))))
