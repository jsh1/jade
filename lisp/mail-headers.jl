;;;; mail-headers.jl -- Mail header utilities
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

(require 'maildefs)
(provide 'mail-headers)


;; General parsing. Most of these functions don't follow RFC-822 to the
;; letter; instead they try to act pragmatically---accepting most forms
;; of input that will be encountered

;; Return (MAIL-ADDR . REAL-NAME) (both strings, or nil) from the position
;; POINT in STRING
(defun mail-parse-address (string &optional point)
  (unless point (setq point 0))
  (let*
      ((mail-addr-re (concat "[\t ]*" mail-atom-re "(\\." mail-atom-re ")*(@"
			     mail-atom-re "(\\." mail-atom-re ")*)?"))
       (mail-angle-addr-re (concat ".*<(" mail-addr-re ")>"))
       (mail-angle-name-re "[\t ]*\"?([^<\t\" \n\f]([\t ]*[^<\t\" \n\f])*)")
       (mail-paren-name-re "[\t ]*\\(\"?([^\n\"]+)\"?\\)")
       mail-addr real-name)
    (cond
     ((string-looking-at mail-angle-addr-re string point)
      ;; "..<foo@bar.baz>..." format
      (setq mail-addr (substring string (match-start 1) (match-end 1)))
      ;; Now look for a preceding name
      (when (string-looking-at mail-angle-name-re string point)
	(setq real-name (substring string (match-start 1) (match-end 1)))))
     ((string-looking-at mail-addr-re string point)
      ;; straightforward "foo@bar.baz" format..
      (setq mail-addr (substring string (match-start) (match-end)))
      (when (string-looking-at mail-paren-name-re string (match-end))
	;; ..with a "(Foo Bar)" comment following
	(setq real-name (substring string (match-start 1) (match-end 1))))))
    (cons mail-addr real-name)))

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
      (unless (re-search-forward "[^\\]\"" pos)
	(error "Unterminated string in list, %s" pos))
      (cons pos (match-end)))
     ((= char ?\()
      ;; A comment
      (unless (re-search-forward "[^\\]\\)" pos)
	(error "Unterminated comment in list, %s" pos))
      (cons pos (match-end)))
     ((= char ?\<)
      ;; An address spec
      (unless (re-search-forward "[^\\]>" pos)
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
    (restrict-buffer pos (forward-char -1 (or (mail-unfold-header pos)
					      (end-of-buffer))))
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


;; General header manipulation

;; Return the start of the header following the header starting at POS
;; This returns nil to show that the header goes to the end of the
;; buffer or restriction
(defun mail-unfold-header (pos)
  (if (looking-at ".*\n([\t ].*\n)*" pos)
      (match-end)
    nil))

;; Return the position at which a header matching HEADER occurs, or
;; nil if one doesn't. HEADER may be a regexp (i.e. "(From|Sender)")
(defun mail-find-header (header &optional pos)
  (when (and (re-search-forward (concat "^(" header "[\t ]*:[\t ]*|$)")
			       (or pos (start-of-buffer)) nil t)
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
					     (end-of-buffer)))
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
      (delete-area (forward-char -1 pos) (end-of-buffer))
      nil)))

;; Insert a list of comma separated items. Breaks the list to satisfy
;; mail-fill-column. Unless NO-COMMAS is t, each item is separated by
;; a comma
(defun mail-insert-list (list &optional no-commas)
  (let
      ((initial-indent (if (looking-at
			    (concat mail-header-name " *") (start-of-line))
			   ;; Get indent from start of header body
			   (pos-col (char-to-glyph-pos (match-end)))
			 ;; Get from current indentation
			 (pos-col (indent-pos)))))
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

;; Return a quoted version of phrase STRING if necessary (i.e. if it
;; contains any specials or CTLs
(defun mail-quote-phrase (string)
  (if (string-match "[][()<>@,;\\\".\001-\037\177]+" string)
      ;; It needs to be quoted
      (progn
	;; Escape any internal doublequotes
	(while (string-match "^(.*[^\\])\"(.*)$" string)
	  (setq string (expand-last-match "\\1\\\"\\2")))
	(concat ?" string ?"))
    string))

;; Return a string constructed from address ADDR and name NAME, according
;; to mail-address-style
(defun mail-format-address (addr name)
  ;; Need to handle quoting full name a la RFC-822
  (if name
      (cond
       ((eq mail-address-style 'angles)
	(concat (mail-quote-phrase name) " \<" addr "\>"))
       ((eq mail-address-style 'parens)
	(concat addr " \(" (mail-quote-phrase name) "\)"))
       (t
	addr))
    addr))

(defun mail-insert-address-list (list &optional no-commas)
  (mail-insert-list (mapcar #'(lambda (cell)
				(mail-format-address (car cell) (cdr cell)))
			    list) no-commas))

;; For a string from the Subject: header of a message, strip off any re:
;; prefixes, and return the string naming the _actual_ subject
(defun mail-get-actual-subject (string)
  (if (and string (string-match mail-re-regexp string nil t))
      (let
	  ((start (match-end)))
	(if (string-match "[ \t]+$" string start)
	    (substring string start (match-start))
	  (substring string start)))
    string))

;; Return t if ADDR1 and ADDR2 refer to the same mailbox.
(defun mail-compare-addresses (addr1 addr2)
  (equal (if (consp addr1) (car addr1) addr1)
	 (if (consp addr2) (car addr2) addr2)))

;; Return a list of addresses uniqified from lists X and Y
(defun mail-union-addresses (x y)
  (when y
    (setq x (copy-sequence x))
    (mapc #'(lambda (a)
	      (unless (assoc (car a) x)
		(setq x (nconc x (list a))))) y))
  x)
