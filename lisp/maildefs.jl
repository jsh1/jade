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

(require 'ring)
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

(defvar mail-highlighted-headers
  "^Subject[\t ]*:[\t ]*(.*)$"
  "Regexp matching headers to be highlighted; currently only the first will
that matches will actually be highlighted. Whatever is left in the first
match buffer is actually highlighted.")

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

(defvar mail-yank-hooks nil
  "A hook called when a message has been cited in a reply. Called with
two arguments, the start and end of the inserted text.")

(defvar mail-default-reply-to nil
  "If non-nil an address to put in the Reply-to header.")

(defvar mail-reply-prefix "Re: "
  "String to prepend to subject of replied messages.")

(defvar mail-yank-prefix ">"
  "String to insert before quoted text in mail messages.")

(defvar mail-fill-column 72
  "Column to wrap at when inserting lists, and filling messages.")

(defvar mail-signature nil
  "String inserted at end of message being sent. If t means to insert the
contents of the file specified by mail-signature-file.")

(defvar mail-signature-file "~/.signature"
  "File to insert at end of message being sent.")

(defvar mail-default-headers (format nil "X-Mailer: Jade %s.%s"
				     (major-version-number)
				     (minor-version-number))
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

(defvar mail-summary-lines 8
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

;; Return (START . END) of the atom-ish expression at POS. "atom-ish"
;; includes comments, strings, angle-delimited addresses, and groups of
;; normal atoms, importantly though, _no_ commas
(defun mail-parse-atom (pos &aux char)
  (when (looking-at "[\t\n ]+" pos)
    (setq pos (match-end)))
  (when (setq char (get-char pos))
    (cond
     ((member char '(?\  ?\t ?\n))
      ;; Whitespace
      (looking-at "[\t\n ]+" pos)
      (cons pos (match-end)))
     ((= char ?\")
      ;; A string
      (unless (find-next-regexp "[^\\]\"" (next-char 1 (copy-pos pos)))
	(error "Unterminated string in list, %s" pos))
      (cons pos (match-end)))
     ((= char ?\()
      ;; A comment
      (unless (find-next-regexp "[^\\]\\)" (next-char 1 (copy-pos pos)))
	(error "Unterminated comment in list, %s" pos))
      (cons pos (match-end)))
     ((= char ?\<)
      ;; An address spec
      (unless (find-next-regexp "[^\\]>" (next-char 1 (copy-pos pos)))
	(error "Unterminated address in list, %s" pos))
      (cons pos (match-end)))
     ((= char ?,)
      ;; A comma
      nil)
     (t
      ;; Some sort of atom
      (unless (looking-at (concat ?\( mail-atom-re
				      "|[][.:;@]|[\t\n ]+\)+") pos)
	(error "Can't parse atom, %s" pos))
      (cons pos (match-end))))))

;; Parse one list of atoms, ended by a comma or EOF. Returns (STRING . END),
;; the text of the group of atoms, and the position of the first non-included
;; character
(defun mail-parse-group (pos)
  (let
      ((list '())
       tem)
    (while (setq tem (mail-parse-atom pos))
      (setq list (cons (copy-area (car tem) (cdr tem)) list))
      (setq pos (cdr tem)))
    (when list
      (cons (nreverse list) pos))))

;; Parse a list of comma-separated mail addresses, returns a list of
;; strings. Stops parsing at the end of the header starting at POS.
;; NO-COMMA-SEPS controls whether a comma-separated list is parsed,
;; or simply a sequence of "groups" (from the above function)
(defun mail-parse-list (pos &optional no-comma-seps)
  (save-restriction
    ;; Restrict ourselves to the current header
    (restrict-buffer pos (prev-char 1 (or (mail-unfold-header pos)
					  (buffer-end))))
    (when (looking-at (concat mail-header-name "[\t ]*") pos)
      (setq pos (match-end)))
    (if no-comma-seps
	(car (mail-parse-group pos))
      (let
	  (list tem)
	(while (setq tem (mail-parse-group pos))
	  (setq list (cons (apply 'concat (car tem)) list)
		pos (if (looking-at "[\t\n ]*,[\t\n ]*" (cdr tem))
			(match-end)
		      (cdr tem))))
	(nreverse list)))))

;; Return the position at which a header matching HEADER occurs, or
;; nil if one doesn't
(defun mail-find-header (header &optional pos)
  (when (and (find-next-regexp (concat "^(" header "[\t ]*:[\t ]*|$)")
			       (or pos (buffer-start)) nil t)
	     (> (match-end) (match-start)))
    (match-end)))

;; Return a copy of the header named HEADER in the current buffer. This
;; will be a string with newlines converted to spaces, unless LISTP is
;; non-nil in which case the header will be split into a list of items
;; (separated by commas, unless NOT-COMMA-SEPARATED is t).
(defun mail-get-header (header &optional listp not-comma-separated)
  (let
      ((pos (mail-find-header header)))
    (when pos
      (if listp
	  (let
	      ((list (mail-parse-list pos not-comma-separated)))
	    (while (setq pos (mail-find-header header
					       (mail-unfold-header pos)))
	      (setq list (nconc list (mail-parse-list pos
						      not-comma-separated))))
	    list)
	(translate-string (copy-area pos (or (mail-unfold-header pos)
					     (buffer-end)))
			  flatten-table)))))

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
	(when (or (and (file-name-absolute-p (car (car tem)))
		       (file-name= folder (car (car tem))))
		  (file-name= folder (file-name-concat mail-folder-dir
						       (car (car tem)))))
	  (setq list (append list (if (consp (cdr (car tem)))
				      (cdr (car tem))
				    (cons (cdr (car tem)))))))
	(setq tem (cdr tem)))
      list))))

;; Insert a list of comma separated items. Breaks the list to satisfy
;; mail-fill-column. Unless NO-COMMAS is t, each item is separated by
;; a comma
(defun mail-insert-list (list &optional no-commas)
  (let
      ((initial-indent (pos-col (char-to-glyph-pos))))
    (while list
      (when (> (+ (length (car list))
		  (pos-col (char-to-glyph-pos (cursor-pos))))
	     mail-fill-column)
      (insert "\n")
      (indent-to initial-indent))
      (insert (car list))
      (when (and (not no-commas) (cdr list))
	(insert ", "))
      (setq list (cdr list)))))

;; History list of prompt-for-folder
(defvar mail-prompt-history (make-ring))

;; Prompt for the name of a mail folder
(defun prompt-for-folder (title &optional default)
  (prompt-for-file title nil
		   (and default (file-name-directory default)) default
		   mail-prompt-history))
