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
      ((mail-addr-re (concat "[\t ]*" mail-atom-re "(\\." mail-atom-re ")*@"
			     mail-atom-re "(\\." mail-atom-re ")*"))
       (mail-angle-addr-re (concat ".*<(" mail-addr-re ")>"))
       (mail-angle-name-re "[\t ]*\"?([^<\t\" \n\f]([\t ]*[^<\t\" \n\f])*)")
       (mail-paren-name-re "[\t ]*\\(\"?([^\n\"]+)\"?\\)")
       mail-addr real-name)
    (cond
     ((string-looking-at mail-addr-re string point)
      ;; straightforward "foo@bar.baz" format..
      (setq mail-addr (substring string (match-start) (match-end)))
      (when (string-looking-at mail-paren-name-re string (match-end))
	;; ..with a "(Foo Bar)" comment following
	(setq real-name (substring string (match-start 1) (match-end 1)))))
     ((string-looking-at mail-angle-addr-re string point)
      ;; "..<foo@bar.baz>..." format
      (setq mail-addr (substring string (match-start 1) (match-end 1)))
      ;; Now look for a preceding name
      (when (string-looking-at mail-angle-name-re string point)
	(setq real-name (substring string (match-start 1) (match-end 1))))))
    (cons mail-addr real-name)))

;; Parse the date header at position POINT in STRING, returns vector
;; [DAY-ABBREV DAY MONTH-ABBREV MONTH YEAR HOUR MINUTE SECOND TZ-STRING]
(defun mail-parse-date (string &optional point)
  (unless point (setq point 0))
  (let
      (day-abbrev day month-abbrev month year hour minute second timezone)
    (when (string-looking-at "[\t ]*(Mon|Tue|Wed|Thu|Fri|Sat|Sun)[\t ]*,[\t ]*"
			     string point t)
      (setq day-abbrev (substring string (match-start 1) (match-end 1)))
      (setq point (match-end)))
    (when (string-looking-at "[\t ]*([0-9]+)[\t ]+([A-Za-z]+)[\t ]+([0-9]+)[\t ]+"
			     string point)
      (setq day (read-from-string (substring string (match-start 1)
					     (match-end 1)))
	    month-abbrev (substring string (match-start 2) (match-end 2))
	    month (cdr (assoc month-abbrev mail-month-alist))
	    year (read-from-string (if (= (- (match-end 3) (match-start 3)) 2)
				       ;; 2-digit year; concat a two 
				       ;; digit prefix onto the front
				       (concat mail-two-digit-year-prefix
					       (substring string
							  (match-start 3)
							  (match-end 3)))
				     (substring string (match-start 3)
						(match-end 3)))))
      (setq point (match-end)))
    (when (string-looking-at "([0-9]+):([0-9]+)(:[0-9]+|)[\t ]*([A-Z]+|[+-][0-9]+)"
		    string point)
      (setq hour (read-from-string (substring string (match-start 1)
					      (match-end 1)))
	  minute (read-from-string (substring string (match-start 2)
					      (match-end 2)))
	  second (if (equal (match-start 3) (match-end 3))
		     0
		   (read-from-string (substring string (1+ (match-start 3))
						(match-end 3))))
	  timezone (substring string (match-start 4) (match-end 4))))
    (vector day-abbrev day month-abbrev month
	    year hour minute second timezone)))


;; Constants defining date structure fields
(defconst mail-date-day-abbrev 0)
(defconst mail-date-day 1)
(defconst mail-date-month-abbrev 2)
(defconst mail-date-month 3)
(defconst mail-date-year 4)
(defconst mail-date-hour 5)
(defconst mail-date-minute 6)
(defconst mail-date-second 7)
(defconst mail-date-timezone 8)


;; Parsing groups and lists

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
      (unless (re-search-forward "[^\\]\"" (forward-char 1 pos))
	(error "Unterminated string in list, %s" pos))
      (cons pos (match-end)))
     ((= char ?\()
      ;; A comment
      (unless (re-search-forward "[^\\]\\)" (forward-char 1 pos))
	(error "Unterminated comment in list, %s" pos))
      (cons pos (match-end)))
     ((= char ?\<)
      ;; An address spec
      (unless (re-search-forward "[^\\]>" (forward-char 1 pos))
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

;; Return a string constructed from address ADDR and name NAME, according
;; to mail-address-style
(defun mail-format-address (addr name)
  ;; Need to handle quoting full name a la RFC-822
  (cond
   ((eq mail-address-style 'angles)
    (concat name " \<" addr "\>"))
   ((eq mail-address-style 'parens)
    (concat addr " \(" name "\)"))
   (t
    addr)))
