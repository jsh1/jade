;;;; maildefs.jl -- Mail configuration, and some utils
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

(provide 'maildefs)


;; Configuration

(defvar user-mail-address (concat (user-login-name) ?\@ (system-name))
  "Address to put in From: headers of outgoing mail.")

(defvar mail-address-style 'angles
  "How to put the sender's full name into From: headers, options include:\n
	'angles		`Foo Bar <foo@bar.baz>'
	'parens		`foo@bar.baz (Foo Bar)'
	nil		`foo@bar.baz'")

(defvar mail-self-blind nil
  "When non-nil a BCC'd copy of the message is sent to the sender. The
header line is included in the initialised message, and so may be deleted
if necessary.")

(defvar mail-archive-file-name nil
  "The name of a file to store a copy of each outgoing message in (through
the FCC header).")

(defvar mail-visible-headers
  "^((Resent-|)(From|Sender|Reply-To|To|Cc|Bcc|Date|Sender)|Subject|Keywords|Comments)[ \t]*:"
  "Regular expression matching the message headers that should be displayed
when showing a mail message.")

(defvar mail-folder-dir (expand-file-name "~/Mail")
  "The directory in which mail folders are stored by default.")

(defvar mail-default-folder "INBOX"
  "The default mail folder.")

(defvar mail-spool-files (concat "/usr/spool/mail/" (user-login-name))
  "The inboxes to check for new mail. This can be a single file name, a list
of file names, or a list of association-lists, each associating a mail
folder with a particular spool file (or list of spool files).")

(defvar mail-header-separator "--text follows this line--"
  "Text used to separate headers from message body; removed before the
message is sent.")

(defvar mail-setup-hook nil
  "A hook called after initialising a mail message in the current buffer.")

(defvar mail-default-reply-to nil
  "If non-nil an address to put in the Reply-to header.")

(defvar mail-yank-prefix ">"
  "String to insert before quoted text in mail messages.")

(defvar mail-signature nil
  "String inserted at end of message being sent. If t means to insert the
contents of the file specified by mail-signature-file.")

(defvar mail-signature-file "~/.signature"
  "File to insert at end of message being sent.")

(defvar mail-default-headers nil
  "Text to insert into the header section of all outgoing mail messages.")

(defvar mail-send-hook nil
  "Hook called immediately prior to sending the mail message in the current
buffer.")

(defvar mail-send-function 'sendmail-send-message
  "Function called when the mail message in the current buffer needs to
be sent. Should throw an error when unsucessful.")

(defvar sendmail-program "/usr/lib/sendmail"
  "Location in the filesystem of the sendmail program.")

(defvar movemail-program nil
  "Location of the movemail program.")

;; This is defined as 1 or more characters followed by a colon. The
;; characters may not include SPC, any control characters, or ASCII DEL.
;; Unfortunately the regexp library doesn't allow NUL bytes in regexps so
;; they slip through..
(defvar mail-header-name "^([^\001- \^?]+)[ \t]*:"
  "A regexp matching a header field name. The name is left in the match
buffer as the first substring. No other substrings may be used.")

(defvar mail-two-digit-year-prefix (substring (current-time-string) 20 22)
  "A two-digit string that will be prepended to year specifications that
only have two, lower order, digits. This is picked up automatically from
the current year, i.e. 1997 -> \"19\", 2001 -> \"20\".")

(defvar mail-summary-lines 16
  "The number of lines that the summary view of a mail folder contains.")

(defvar mail-display-summary nil
  "When non-nil a summary of the current folder is always displayed when
reading mail.")

(defvar mail-message-start "^From "
  "The regular expression separating each message. Actually it's interpreted
as a single blank line or the start of the buffer, followed by this regexp.")

(defvar mail-atom-re "[a-zA-Z0-9_*+!#$~%^&={}'|-]+"
  "Regular expression defining a single atom in a mail header. May not
include any parenthesised expressions!")


;;;; Mail utility functions

;; Return the start of the header following the header starting at POS
;; This returns nil to show that the header goes to the end of the
;; buffer or restriction
(defun mail-unfold-header (pos)
  (if (looking-at ".*\n([\t ].*\n)*" pos)
      (match-end)
    nil))

;; Parse a list of comma-separated mail addresses, returns a list of
;; strings. Stops parsing at the end of the header starting at POS.
(defun mail-parse-list (pos)
  (save-restriction
    ;; Restrict ourselves to the current header
    (restrict-buffer pos (prev-char 1 (or (mail-unfold-header pos)
					  (buffer-end))))
    (when (looking-at (concat mail-header-name "[\t ]*") pos)
      (setq pos (match-end)))
    (let
	((list '())
	 (start pos))
      (while pos
	(cond
	 ((looking-at "[\t\n ]*\"" pos)
	  ;; String to skip
	  (unless (find-next-regexp "[^\\]\"" (match-end))
	    (error "Unterminated string in list" pos))
	  (setq pos (match-end)))
	 ((looking-at "[\t\n ]*\\(" pos)
	  ;; Comment to skip
	  (unless (find-next-regexp "[^\\]\\)" (match-end))
	    (error "Unterminated comment in list" pos))
	  (setq pos (match-end)))
	 ((looking-at (concat ?\( mail-atom-re
			      "|[][.:;@<>]|[\t\n ]+\)+") pos)
	  ;; Skip several atoms, whitespace or specials (but not
	  ;; comma or parens)
	  (setq pos (match-end)))
	 ((looking-at "[\t\n ]*,[\t\n ]*" pos)
	  ;; At last the end of an item
	  (setq list (cons (translate-string (copy-area start (match-start))
					     flatten-table) list)
		pos (match-end)
		start pos))
	 ((>= pos (buffer-end))
	  (setq list (cons (translate-string (copy-area start (buffer-end))
					     flatten-table) list)
		pos nil))
	 (t
	  (error "Shouldn't happen"))))
      list)))

;; Delete the header at POS and return the position of the following
;; header, or nil for when the end of the buffer/restriction is reached
(defun mail-delete-header (pos)
  (let
      ((end (mail-unfold-header pos)))
    (if end
	(progn
	  (delete-area pos end)
	  pos)
      ;; Header goes up to the end of the restriction, delete the
      ;; previous newline instead of the next
      (delete-area (prev-char 1 (copy-pos pos)) (buffer-end))
      nil)))

;; Return a list of inboxes for the mail folder FOLDER
(defun mail-find-inboxes (folder)
  (cond
   ((null mail-spool-files)
    nil)
   ((stringp mail-spool-files)
    (list mail-spool-files))
   ((and (consp mail-spool-files) (stringp (car mail-spool-files)))
    mail-spool-files)
   (t
    ;; Must be a list of lists.
    (let
	((tem mail-spool-files)
	 list)
      (while tem
	(when (or (and (file-exists-p (car (car tem)))
		       (file-name= folder (car (car tem))))
		  (and (file-exists-p (file-name-concat mail-folder-dir
							(car (car tem))))
		       (file-name= folder (file-name-concat mail-folder-dir
							    (car (car tem))))))
	  (setq list (append list (if (consp (cdr (car tem)))
				      (cdr (car tem))
				    (cons (cdr (car tem)))))))
	(setq tem (cdr tem)))
      list))))
