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
(defun mail-parse-address (string #!optional point)
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
(defun mail-parse-atom (p)
 (let (char)
  (when (looking-at "[\t\n ]+" p)
    (setq p (match-end)))
  (when (setq char (get-char p))
    (cond
     ((member char '(#\space #\tab #\newline))
      ;; Whitespace
      (looking-at "[\t\n ]+" p)
      (cons p (match-end)))
     ((= char #\")
      ;; A string
      (unless (re-search-forward "[^\\]\"" p)
	(error "Unterminated string in list, %s" p))
      (cons p (match-end)))
     ((= char #\()
      ;; A comment
      (unless (re-search-forward "[^\\]\\)" p)
	(error "Unterminated comment in list, %s" p))
      (cons p (match-end)))
     ((= char #\<)
      ;; An address spec
      (unless (re-search-forward "[^\\]>" p)
	(error "Unterminated address in list, %s" p))
      (cons p (match-end)))
     ((= char #\,)
      ;; A comma
      nil)
     (t
      ;; Some sort of atom
      (and (looking-at (concat #\( mail-atom-re "|[][.:;@]|[\t\n ]+\)+") p)
	   (cons p (match-end))))))))

;; Parse one list of atoms, ended by a comma or EOF. Returns (STRING . END),
;; the text of the group of atoms, and the position of the first non-included
;; character
(defun mail-parse-group (p)
  (let
      ((lst '())
       tem)
    (while (setq tem (mail-parse-atom p))
      (setq lst (cons (copy-area (car tem) (cdr tem)) lst))
      (setq p (cdr tem)))
    (when lst
      (cons (reverse! lst) p))))

;; Parse a list of comma-separated mail addresses, returns a list of
;; strings. Stops parsing at the end of the header starting at POS.
;; NO-COMMA-SEPS controls whether a comma-separated list is parsed,
;; or simply a sequence of "groups" (from the above function)
(defun mail-parse-list (p #!optional no-comma-seps)
  (save-restriction
    ;; Restrict ourselves to the current header
    (restrict-buffer p (forward-char -1 (or (mail-unfold-header p)
					      (end-of-buffer))))
    (when (looking-at (concat mail-header-name "[\t ]*") p)
      (setq p (match-end)))
    (if no-comma-seps
	(car (mail-parse-group p))
      (let
	  (lst tem)
	(while (setq tem (mail-parse-group p))
	  (setq lst (cons (apply concat (car tem)) lst)
		p (if (looking-at "[\t\n ]*,[\t\n ]*" (cdr tem))
			(match-end)
		      (cdr tem))))
	(reverse! lst)))))


;; decoding of RFC-2047 encoded phrases in headers

(defvar mail-decode-header-map
  (let
      ((map (make-string (1+ #\_)))
       (i 0))
    (while (< i #\_)
      (array-set! map i i)
      (setq i (1+ i)))
    (array-set! map #\_ #\space)
    map))

(defvar mail-encode-header-map
  (let
      ((map (make-string (1+ #\space)))
       (i 0))
    (while (< i #\space)
      (array-set! map i i)
      (setq i (1+ i)))
    (array-set! map #\space #\_)
    map))

(defun mail-decode-header (string)
  (let
      ((point 0)
       (re (concat "=\\?([^?]+)\\?([^?]+)\\?(.*?)\\?="))
       (out nil))
    (while (string-match re string point)
      (let
	  ((encoding (expand-last-match "\\2"))
	   (text (expand-last-match "\\3"))
	   (stream (make-string-output-stream)))
	(setq out (cons (substring string point (match-start)) out))
	(setq point (match-end))
	;; XXX ignore charset, just decode to 8-bit
	(cond
	 ((string-ci=? encoding "Q")
	  ;; quoted-printable
	  (require 'mime-decode)
	  (setq text (translate-string! text mail-decode-header-map))
	  (mime-decode-string 'quoted-printable text stream))
	 ((string-ci=? encoding "B")
	  ;; base64
	  (require 'mime-decode)
	  (mime-decode-string 'base64 text stream)))
	(setq out (cons (get-output-stream-string stream) out))))
    (if out
	(apply concat (reverse! (cons (substring string point) out)))
      string)))

;; encode non ASCII characters in STRING
(defun mail-encode-header-string (string)
  (if (string-match "[\200-\377]" string)
      (progn
	;; needs encoding
	(require 'mime-encode)
	(let
	    ((stream (make-string-output-stream)))
	  (setq string (translate-string! (copy-sequence string)
					 mail-encode-header-map))
	  (mime-encode-stream 'quoted-printable
			      (make-string-input-stream string) stream)
	  (concat "=?iso-8859-1?Q?" (get-output-stream-string stream) "?=")))
    string))


;; General header manipulation

;; Return the start of the header following the header starting at POS
;; This returns nil to show that the header goes to the end of the
;; buffer or restriction
(defun mail-unfold-header (p)
  (if (looking-at ".*\n([\t ].*\n)*" p)
      (match-end)
    nil))

;; Return the position at which a header matching HEADER occurs, or
;; nil if one doesn't. HEADER may be a regexp (i.e. "(From|Sender)")
(defun mail-find-header (header #!optional p)
  (when (and (re-search-forward (concat "^(" header "[\t ]*:[\t ]*|$)")
			       (or p (start-of-buffer)) nil t)
	     (> (match-end) (match-start)))
    (match-end)))

;; Return a copy of the header named HEADER in the current buffer. This
;; will be a string with newlines converted to spaces, unless LISTP is
;; non-nil in which case the header will be split into a list of items
;; (separated by commas, unless NOT-COMMA-SEPARATED is t).
(defun mail-get-header (header #!optional lstp not-comma-separated decode)
  (let
      ((p (mail-find-header header))
       out)
    (when p
      (if lstp
	  (progn
	    (setq out (mail-parse-list p not-comma-separated))
	    (while (setq p (mail-find-header header (mail-unfold-header p)))
	      (setq out (append! out (mail-parse-list p not-comma-separated))))
	    (when decode
	      (setq out (mapcar mail-decode-header out))))
	(setq out (translate-string! (copy-area p (or (mail-unfold-header p)
						     (end-of-buffer)))
				    flatten-table))
	(when decode
	  (setq out (mail-decode-header out)))))
    out))

;; Delete the header at POS and return the position of the following
;; header, or nil for when the end of the buffer/restriction is reached
(defun mail-delete-header (p)
  (let
      ((end (mail-unfold-header p)))
    (if end
	(progn
	  (delete-area p end)
	  p)
      ;; Header goes up to the end of the restriction, delete the
      ;; previous newline instead of the next
      (delete-area (forward-char -1 p) (end-of-buffer))
      nil)))

;; Insert a list of comma separated items. Breaks the list to satisfy
;; mail-fill-column. Unless NO-COMMAS is t, each item is separated by
;; a comma
(defun mail-insert-list (lst #!optional no-commas)
  (let*
      ((at-bol t)
       (initial-indent (if (looking-at
			    (concat mail-header-name " *") (start-of-line))
			   ;; Get indent from start of header body
			   (pos-col (char-to-glyph-pos (match-end)))
			 ;; Get from current indentation
			 (pos-col (indent-pos))
			 (setq at-bol nil))))
    (while lst
      (when (and (not at-bol)
		 (> (+ (length (car lst))
		       (pos-col (char-to-glyph-pos (cursor-pos))))
		    mail-fill-column))
	(insert "\n")
	(indent-to initial-indent))
      (setq at-bol nil)
      (insert (car lst))
      (setq lst (cdr lst))
      (when lst
	(insert (if no-commas " " ", "))))))

;; Return a quoted version of phrase STRING if necessary (i.e. if it
;; contains any specials or CTLs
(defun mail-quote-phrase (string)
  (when (string-match "[][()<>@,;\\\".\001-\037\177]+" string)
    ;; It needs to be quoted
    ;; Escape any internal doublequotes
    (while (string-match "^(.*[^\\])\"(.*)$" string)
      (setq string (expand-last-match "\\1\\\"\\2")))
    (setq string (concat #\" string #\")))
  (mail-encode-header-string string))

;; Return a string constructed from address ADDR and name NAME, according
;; to mail-address-style
(defun mail-format-address (addr name)
  ;; Need to handle quoting full name a la RFC-822
  (if name
      (cond
       ((eq? mail-address-style 'angles)
	(concat (mail-quote-phrase name) " \<" addr "\>"))
       ((eq? mail-address-style 'parens)
	(concat addr " \(" (mail-quote-phrase name) "\)"))
       (t
	addr))
    addr))

(defun mail-insert-address-list (lst #!optional no-commas)
  (mail-insert-list (mapcar (lambda (cell)
			      (mail-format-address (car cell) (cdr cell)))
			    lst) no-commas))

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
  (equal? (if (pair? addr1) (car addr1) addr1)
	 (if (pair? addr2) (car addr2) addr2)))

;; Return a list of addresses uniqified from lists X and Y
(defun mail-union-addresses (x y)
  (when y
    (setq x (copy-sequence x))
    (mapc (lambda (a)
	    (unless (assoc (car a) x)
	      (setq x (append! x (list a))))) y))
  x)

(defun make-message-id ()
  (let ((time (current-utime))
	(randomness (+ (ash (random) 32) (random))))
    (format nil "<%s.%s@%s>"
	    (number->string time 36)
	    (number->string randomness 36)
	    (system-name))))
