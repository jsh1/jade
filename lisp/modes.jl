;;;; modes.jl -- Code for handling editing modes.
;;;  Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
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


;;; Notes:

;;; Major Modes:
;;;
;;; Each major mode has a function which is called to install the mode,
;;; the first thing such a function should do is check if a mode is
;;; already installed and if so remove it:
;;;
;;;  (when major-mode-kill
;;;    (major-mode-kill))
;;;
;;; Now the new mode is free to install itself; generally this entails
;;; setting at least the `mode-name' and `major-mode-kill' variables
;;; and installing a local keymap. For example `lisp-mode' does this:
;;;
;;;   (set! mode-name "Lisp")
;;;   (set! major-mode 'lisp-mode)
;;;   (set! major-mode-kill lisp-mode-kill)
;;;   (set! mode-comment-fun lisp-mode-insert-comment)
;;;   (set! local-keymap lisp-mode-keymap)
;;;   (call-hook 'lisp-mode-hook)
;;;
;;; The function to be called when the mode is removed should remove the
;;; effects of the above, for example:
;;;
;;;   (set! local-keymap nil)
;;;   (set! major-mode nil)
;;;   (set! major-mode-kill nil)
;;;   (set! mode-comment-fun nil)
;;;   (set! mode-name nil)

;;; Minor Modes:
;;;
;;; These are usually harder to implement than major modes since they
;;; have to coexist with all other modes which are installed.
;;;
;;; Generally each minor mode maintains a buffer-local variable saying
;;; whether or not it's installed in the buffer. The minor mode's
;;; function usually toggles the mode on or off depending on the state of
;;; this variable.
;;;
;;; Each buffer has a keymap for the bindings of all the minor modes
;;; active in the buffer (called `minor-mode-keymap'). These bindings
;;; have to be added when the mode is enabled and removed when it
;;; is disabled.


;; Configuration

(defvar auto-mode-alist
  '(("\\.(c|h|cpp|cc)$|^c$" . c-mode)
    ("\\.(jl|lisp|scm)$|^.jaderc$|^lisp$" . lisp-mode)
    ("\\.(te?xt|doc|article|letter)$" . text-mode)
    ("^(text|(.*/|)draft)$" . text-mode)
    ("^indented-text$" . indented-text-mode)
    ("\\.[s]$|^asm$" . asm-mode)
    ("\\.[S]$|^asm-cpp$" . asm-cpp-mode)
    ("\\.texi(|nfo)|^Texinfo$" . texinfo-mode)
    ("\\.(tex|sty)$|^TeX$" . tex-mode)
    ("^LaTeX$" . latex-mode)
    ("ChangeLog$" . changelog-mode)
    ("\\.sh$|^sh(ell)?$" . sh-mode)
    ("\\.p[lm]$|^perl$" . perl-mode)
    ("\\.js$" . javascript-mode)
    ("\\.swift$" . swift-mode)
    ("\\.y$|^yacc$" . yacc-mode)
    ("\\.(m|mm)$|^objective-c$" . objective-c-mode))
  "List of all major modes which can be enabled by loading a file into
a buffer. List is made of `(REGEXP . MODE)' cells; the REGEXP is matched
against the mode specification (i.e. the filename), if it matches the
function MODE is called to install the mode.")

(defvar interpreter-mode-regexp "^#!"
  "Regexp matching a string at the start of a file specifiying that the
file is interpreted when executed.")

(defvar interpreter-mode-alist '(("/bin/(bash|ksh|sh)" . sh-mode)
				 ("/bin/perl" . perl-mode))
  "List of (REGEXP . MAJOR-MODE) defining modes to use for interpreted files.")

(defvar comment-column 41
  "Buffer-local variable containing the canonical column number which
comments should begin at. If the line extends past this column the next
tab stop after the end of the line is used instead.")
(make-variable-buffer-local 'comment-column)

(defvar default-major-mode 'fundamental-mode
  "The major mode that is installed in buffers for which no other mode either
matches or is specified.")

(defvar mode-name "Fundamental"
  "Name of the buffer's major mode.")
(make-variable-buffer-local 'mode-name)

(defvar minor-mode-alist nil
  "Alist of (VARIABLE . MODE-LINE-ELEMENT) defining how minor modes are mapped
to strings in the mode line.")

(defvar mode-line-format
  '("%m%*%+-%B %(" mode-name minor-mode-alist "%) %p %[%c, %l%]%-")
  "Value defining how the status-line of each view is formatted.")
(make-variable-buffer-local 'mode-line-format)


;; Generic expression configuration
(defvar generic-exp-single-delims '(#\")
  "A list of characters that delimit compound expressions in generic
expressions.")
(make-variable-buffer-local 'generic-exp-single-delims)

(defvar generic-exp-open-delims '(#\( #\[ #\{)
  "A list of characters that open compound expressions in generic
expressions. Can only be from the list #\\(, #\\[, #\\{, #\\< and #\\`.")
(make-variable-buffer-local 'generic-exp-open-delims)

(defvar generic-exp-close-delims '(#\) #\] #\})
  "A list of characters that close compound expressions in generic
expressions. Can only be from the list #\\), #\\], #\\}, #\\> and #\\'.")
(make-variable-buffer-local 'generic-exp-close-delims)

(defvar generic-exp-escape-char '#\\
  "The character that escapes the next character in a generic expression.")
(make-variable-buffer-local 'generic-exp-escape-char)

(defvar generic-exp-comment-string nil
  "When non-nil a string that begins a comment up to the end of the
current line.")
(make-variable-buffer-local 'generic-exp-comment-string)

(defvar generic-exp-symbol-re "[a-zA-Z0-9_]+"
  "Regexp matching a symbol")
(make-variable-buffer-local 'generic-exp-symbol)

(defvar generic-exp-special-re "[][(){}\"a-zA-Z0-9_]"
  "Characters to look for when finding the start of the next expression
after a symbol.")
(make-variable-buffer-local 'generic-exp-special-re)

;; Variables

(defvar major-mode nil
  "The function which was used to initialise the buffer's major mode.")
(make-variable-buffer-local 'major-mode)

(defvar major-mode-kill nil
  "The function which should be called to remove the buffer's major mode.")
(make-variable-buffer-local 'major-mode-kill)

(defvar mode-comment-header nil
  "The string that introduces a single-line comment in the current mode.")
(make-variable-buffer-local 'mode-comment-header)

(defvar mode-comment-fun nil
  "Function called to insert a comment in this mode, if mode-comment-header
is nil.")
(make-variable-buffer-local 'mode-comment-fun)

(defvar mode-indent-line nil
  "A function called to indent a specified line in the current buffer.")
(make-variable-buffer-local 'mode-indent-line)

(defvar mode-forward-exp nil
  "Function like `lisp-forward-sexp'.")
(make-variable-buffer-local 'mode-forward-exp)

(defvar mode-backward-exp nil
  "Function like `lisp-backward-sexp'.")
(make-variable-buffer-local 'mode-backward-exp)

(defvar mode-symbol-regexp generic-exp-symbol-re
  "Regular expression defining a ``symbol'' in the current major mode.")
(make-variable-buffer-local 'mode-symbol-regexp)

(defvar mode-defun-header nil
  "A regular expression matching a function header in the current major mode.
The name of the function should be stored in the first match expression.")
(make-variable-buffer-local 'mode-defun-header)

(defvar mode-defun-footer nil
  "A regular expression matching the end of a function definition in the
current major mode. If undefined, a single expression from the beginning
of the defun is assumed instead.")
(make-variable-buffer-local 'mode-defun-footer)


;; Major mode handling

(defun normal-mode ()
  "Initialise the standard major mode for the current buffer."
  (interactive)
  (unless major-mode
    (let ((text (and (looking-at
		      ".*-\\*-\\s*([^: \t]+).*-\\*-" (start-of-buffer))
		     (expand-last-match "\\1"))))
      (when (and text (> (string-length text) 0))
	(let ((pieces (string-split "\\s*;\\s*" text)))
	  (when pieces
	    (set! major-mode (cdr (assoc-regexp (car pieces)
						auto-mode-alist t))))))
      (unless major-mode
	(set! major-mode (or (and (looking-at interpreter-mode-regexp
					      (start-of-buffer))
				  (cdr (assoc-regexp
					(copy-area (start-of-buffer)
						   (end-of-line
						    (start-of-buffer)))
					interpreter-mode-alist t)))
			     (cdr (assoc-regexp
				   (buffer-file-name) auto-mode-alist t)))))))
  (let ((fun (or major-mode default-major-mode)))
    (when (symbol? fun)
      (set! fun (variable-ref fun)))
    (fun)))

(defun fundamental-mode ()
  "Remove the major mode being used to edit the current buffer, the
fundamental mode is used instead."
  (interactive)
  (when major-mode-kill
    (major-mode-kill)))


;; Comment handling

(defun insert-comment ()
  "Insert comment delimeters on the current line, place the cursor where the
comment should be written. This may or not be defined by each major mode."
  (interactive)
  (cond
   (mode-comment-header
    (if (looking-at (concat ".*(" (quote-regexp mode-comment-header) ")")
		    (start-of-line))
	(goto (match-end 1))
      (find-comment-pos)
      (insert mode-comment-header)))
   (mode-comment-fun
    (mode-comment-fun))
   (t
    (error "No defined method for inserting comments in this buffer"))))

(defun find-comment-pos ()
  (let
      ((p (glyph-to-char-pos (pos (1- comment-column) nil))))
    (goto (end-of-line))
    (if (>= (end-of-line) p)
	(insert "\t")
      (indent-to (1- comment-column)))))


;; Indentation

(defun indent-area (start end)
  "Use the `mode-indent-line' function to indent each line between START and
END."
  (interactive "-m\nM")
  (set! start (start-of-line start))
  (set! end (start-of-line end))
  (unless mode-indent-line
    (error "No method for indenting lines in this buffer"))
  (while (< start end)
    (mode-indent-line start)
    (set! start (forward-line 1 start))))

(defun newline-and-indent ()
  "Insert a newline then either call this buffer's `mode-indent-line' function
or insert a tab."
  (interactive)
  (if (null? mode-indent-line)
      (insert "\n\t")
    (insert "\n")
    (mode-indent-line)))

(defun indent-line ()
  "Indent the current line."
  (interactive)
  (if mode-indent-line
      (let
	  ((p (mode-indent-line)))
	(when (and (posp p) (< (char-to-glyph-pos (cursor-pos)) p))
	  (goto-glyph p))
	(when (> (glyph-to-char-pos p) (end-of-line))
	  (goto (end-of-line))))
    (error "No method for indentation in this buffer.")))


;; Expressions

(defun forward-exp (#!optional number p)
  "Find the end of the NUMBER'th next expression."
  (interactive "@p")
  (cond ((> number 0)
	 ((or mode-forward-exp forward-word) number p))
	((< number 0)
	 ((or mode-backward-exp backward-word) (- number) p))))

(defun backward-exp (#!optional number p)
  "Find the start of the NUMBER'th previous expression."
  (interactive "@p")
  (cond ((> number 0)
	 ((or mode-backward-exp backward-word) number p))
	((< number 0)
	 ((or mode-forward-exp forward-word) (- number) p))))

(defun kill-exp (#!optional number)
  "Kill the next NUMBER expressions."
  (interactive "p")
  (kill-area (cursor-pos) ((or mode-forward-exp forward-word) number)))

(defun backward-kill-exp (#!optional number)
  "Kills from the start of this NUMBER'th previous expression to the cursor."
  (interactive "p")
  (kill-area ((or mode-backward-exp backward-word) number) (cursor-pos)))

(defun transpose-exps (count)
  "Move the expression before the cursor COUNT expressions forwards."
  (interactive "p")
  (transpose-items (or mode-forward-exp forward-word)
		   (or mode-backward-exp backward-word)
		   count))


;; Other program units

(defun symbol-at-point ()
  "Return a string defining the symbol under the cursor."
  (let
      (start end)
    (if (looking-at mode-symbol-regexp)
	;; Find this symbol's beginning
	(if (and (re-search-backward mode-symbol-regexp)
		 (> (match-end) (cursor-pos)))
	    (progn
	      (set! start (match-start))
	      (set! end (match-end)))
	  (looking-at mode-symbol-regexp)
	  (set! start (cursor-pos))
	  (set! end (match-end)))
      (set! start (re-search-backward mode-symbol-regexp))
      (set! end (match-end)))
    (when (and start end)
      (copy-area start end))))

(defun defun-at-point ()
  "Return the name of the function defined under the cursor, or nil."
  (when (and mode-defun-header (re-search-backward mode-defun-header))
    (expand-last-match "\\1")))

(defun start-of-defun ()
  "Find the start of the current function definition."
  (interactive "@")
  (or mode-defun-header (error "Functions undefined in this mode"))
  (re-search-backward mode-defun-header))
    
(defun end-of-defun ()
  "Find the end of the current function definition."
  (interactive "@")
  (or mode-defun-header (error "Functions undefined in this mode"))
  (if (string? mode-defun-footer)
      (and (re-search-forward mode-defun-footer) (match-end))
    (if (function? mode-defun-footer)
	(mode-defun-footer (start-of-defun))
      (forward-exp 1 (start-of-defun)))))

(defun mark-defun ()
  "Mark the current function definition as a block."
  (interactive)
  (let
      ((start (start-of-defun))
       (end (end-of-defun)))
    (when (and start end)
      (mark-block (or (and (re-search-backward "^[ \t\f]*\n" start)
			   (match-end)) start) end))))

(defun indent-defun ()
  "Indent the current function."
  (interactive)
  (let ((start (start-of-defun))
	(end (end-of-defun)))
    (when (and start end)
      (indent-area (or (and (re-search-backward "^[ \t\f]*\n" start)
			    (match-end)) start)
		   (if (search-forward "\n" end) (match-end) end)))))


;; Generic expression handling

(defun generic-forward-exp (#!optional number p)
  "Return the position of the NUMBER'th next expression from POS."
  (unless number
    (set! number 1))
  (unless p
    (set! p (cursor-pos)))
  (let
      ((ws-re (if (null? generic-exp-comment-string)
		  "[\t\f\n ]+"
		(concat "([\t\f\n ]+|("
			(quote-regexp generic-exp-comment-string)
			".*\n))+"))))
    (while (> number 0)
      ;; first, skip white space and comments
      (when (looking-at ws-re p)
	(set! p (match-end)))
      (when (> p (end-of-buffer))
	(error "End of buffer"))
      (let
	  ((c (get-char p)))
	(cond
	 ((member c generic-exp-single-delims)
	  ;; move over string
	  (set! p (char-search-forward c (forward-char 1 p)))
	  (or p (error "String doesn't end!"))
	  (while (= (get-char (forward-char -1 p))
		    generic-exp-escape-char)
	    (set! p (char-search-forward c (forward-char 1 p)))
	    (or p (error "String doesn't end!")))
	  (set! p (forward-char 1 p)))
	 ((member c generic-exp-open-delims)
	  ;; move over brackets
	  (set! p (find-matching-bracket p nil generic-exp-escape-char))
	  (or p (error "Expression doesn't end!"))
	  (set! p (forward-char 1 p)))
	 ((member c generic-exp-close-delims)
	  (error "End of containing expression"))
	 (t
	  ;; a symbol of some sort
	  (if (looking-at generic-exp-symbol-re p)
	      (set! p (match-end))
	    (set! p (re-search-forward generic-exp-special-re p))
	    (or p (error "Can't find end of symbol"))
	    (set! number (1+ number))))))
      (set! number (1- number))))
  p)

(defun generic-backward-exp (#!optional number orig-pos)
  "Return the position of the NUMBER'th previous s-expression from ORIG-POS."
  (unless number
    (set! number 1))
  (unless orig-pos 
    (set! orig-pos (cursor-pos)))
  (let
      ((p orig-pos)
       comment-skip-some-re
       comment-skip-line-re)
    (when generic-exp-comment-string
      (set! comment-skip-some-re (concat
				  ".*([\t\f ]*"
				  (quote-regexp generic-exp-comment-string)
				  "|[\f\t ]*)$"))
      (set! comment-skip-line-re (concat
				  "^[\t\f ]*"
				  (quote-regexp generic-exp-comment-string)
				  "|^[\f\t ]*$")))
    (while (> number 0)
      ;; skip preceding white space
      (set! p (re-search-backward "[^\t\f\n ]" (forward-char -1 p)))
      (or p (error "No expression!"))
      (when generic-exp-comment-string
	(while (looking-at comment-skip-line-re (start-of-line p))
	  (set! p (forward-line -1 p))
	  (or p (error "Beginning of buffer"))
	  (set! p (end-of-line p)))
	(when (and (looking-at comment-skip-some-re (start-of-line p))
		   (< (match-start 1) p))
	  (set! p (forward-char -1 (match-start)))))
      (let
	  ((c (get-char p)))
	(cond
	 ((member c generic-exp-close-delims)
	  (set! p (find-matching-bracket p nil generic-exp-escape-char))
	  (or p (error "Brackets don't match")))
	 ((member c generic-exp-single-delims)
	  (set! p (char-search-backward c (forward-char -1 p)))
	  (or p (error "String doesn't start!"))
	  (while (= (get-char (forward-char -1 p))
		    generic-exp-escape-char)
	    (set! p (char-search-backward c (forward-char -1 p)))
	    (or p (error "String doesn't start!"))))
	 ((member c generic-exp-open-delims)
	  (error "Start of containing sexp"))
	 (t
	  ;; a symbol?
	  (if (looking-at generic-exp-symbol-re p)
	      (progn
		(set! p (re-search-backward generic-exp-symbol-re p))
		(or p (error "Can't classify expression")))
	    (set! number (1+ number))))))
      (set! number (1- number)))
    p))

(variable-set-default! 'mode-forward-exp generic-forward-exp)
(variable-set-default! 'mode-backward-exp generic-backward-exp)
