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
;;;    (funcall major-mode-kill))
;;;
;;; Now the new mode is free to install itself; generally this entails
;;; setting at least the `mode-name' and `major-mode-kill' variables
;;; and installing a local keymap. For example `lisp-mode' does this:
;;;
;;;   (setq mode-name "Lisp"
;;;	    major-mode 'lisp-mode
;;;	    major-mode-kill 'lisp-mode-kill
;;;	    mode-comment-fun 'lisp-mode-insert-comment
;;;	    keymap-path (cons 'lisp-mode-keymap keymap-path))
;;;   (call-hook 'lisp-mode-hook)
;;;
;;; The function to be called when the mode is removed should remove the
;;; effects of the above, for example:
;;;
;;;   (setq keymap-path (delq 'lisp-mode-keymap keymap-path)
;;;	    major-mode nil
;;;	    major-mode-kill nil
;;;	    mode-comment-fun nil
;;;	    mode-name nil)

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
  '(("\\.(c|h)$|^c$" . c-mode)
    ("\\.jl$|^.jaderc$|^lisp$" . lisp-mode)
    ("\\.(te?xt|doc|article|letter)$" . text-mode)
    ("^(text|(.*/|)draft)$" . text-mode)
    ("^indented-text$" . indented-text-mode)
    ("\\.[s]$|^asm$" . asm-mode)
    ("\\.[S]$|^asm-cpp$" . asm-cpp-mode)
    ("\\.texi(|nfo)|^Texinfo$" . texinfo-mode)
    ("\\.tex$|^TeX$" . tex-mode)
    ("^LaTeX$" . latex-mode)
    ("ChangeLog$" . changelog-mode)
    ("\\.sh$|^sh(ell)?$" . sh-mode)
    ("\\.pl$|^perl$" . perl-mode)
    ("\\.y$|^yacc$" . yacc-mode))
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
(defvar generic-exp-single-delims '(?\")
  "A list of characters that delimit compound expressions in generic
expressions.")
(make-variable-buffer-local 'generic-exp-single-delims)

(defvar generic-exp-open-delims '(?\( ?\[ ?\{)
  "A list of characters that open compound expressions in generic
expressions. Can only be from the list `\(', `\[', `\{', `\<' and `\`'.")
(make-variable-buffer-local 'generic-exp-open-delims)

(defvar generic-exp-close-delims '(?\) ?\] ?\})
  "A list of characters that close compound expressions in generic
expressions. Can only be from the list `\)', `\]', `\}', `\>' and `\''.")
(make-variable-buffer-local 'generic-exp-close-delims)

(defvar generic-exp-escape-char '?\\
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

(defvar mode-forward-exp 'generic-forward-exp
  "Function like `lisp-forward-sexp'.")
(make-variable-buffer-local 'mode-forward-exp)

(defvar mode-backward-exp 'generic-backward-exp
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

(defun assoc-regexp (input alist &optional fold-case)
  "Scan ALIST for an element whose car is a regular expression matching the
string INPUT."
  (catch 'return
    (mapc #'(lambda (cell)
	      (when (string-match (car cell) input nil fold-case)
		(throw 'return cell))) alist)))

(defun normal-mode ()
  "Initialise the standard major mode for the current buffer."
  (interactive)
  (unless major-mode
    (setq major-mode (or (and (looking-at ".*-\\*- *([^ ]+) *-\\*-"
					  (start-of-buffer))
			      (cdr (assoc-regexp (expand-last-match "\\1")
						 auto-mode-alist t)))
			 (and (looking-at interpreter-mode-regexp
					  (start-of-buffer))
			      (cdr (assoc-regexp
				    (copy-area (start-of-buffer)
					       (end-of-line (start-of-buffer)))
				    interpreter-mode-alist t)))
			 (cdr (assoc-regexp
			       (buffer-file-name) auto-mode-alist t)))))
  (funcall (if (functionp major-mode)
	       major-mode
	     default-major-mode)))

(defun fundamental-mode ()
  "Remove the major mode being used to edit the current buffer, the
fundamental mode is used instead."
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill)))


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
    (funcall mode-comment-fun))
   (t
    (error "No defined method for inserting comments in this buffer"))))

(defun find-comment-pos ()
  (let
      ((pos (glyph-to-char-pos (pos (1- comment-column) nil))))
    (goto (end-of-line))
    (if (>= (end-of-line) pos)
	(insert "\t")
      (indent-to (1- comment-column)))))


;; Indentation

(defun indent-area (start end)
  "Use the `mode-indent-line' function to indent each line between START and
END."
  (interactive "-m\nM")
  (setq start (start-of-line start)
	end (start-of-line end))
  (unless mode-indent-line
    (error "No method for indenting lines in this buffer"))
  (while (< start end)
    (funcall mode-indent-line start)
    (setq start (forward-line 1 start))))

(defvar newline-and-indent ()
  "(newline-and-indent)
Insert a newline then either call this buffer's `mode-indent-line' function
or insert a tab."
  (interactive)
  (if (null mode-indent-line)
      (insert "\n\t")
    (insert "\n")
    (funcall mode-indent-line)))

(defun indent-line ()
  "Indent the current line."
  (interactive)
  (if mode-indent-line
      (let
	  ((pos (funcall mode-indent-line)))
	(when (and (posp pos) (< (char-to-glyph-pos (cursor-pos)) pos))
	  (goto-glyph pos))
	(when (> (glyph-to-char-pos pos) (end-of-line))
	  (goto (end-of-line))))
    (error "No method for indentation in this buffer.")))


;; Expressions

(defun forward-exp (&optional number pos)
  "Find the end of the NUMBER'th next expression."
  (interactive "@p")
  (cond ((> number 0)
	 (funcall (or mode-forward-exp 'forward-word) number pos))
	((< number 0)
	 (funcall (or mode-backward-exp 'backward-word) (- number) pos))))

(defun backward-exp (&optional number pos)
  "Find the start of the NUMBER'th previous expression."
  (interactive "@p")
  (cond ((> number 0)
	 (funcall (or mode-backward-exp 'backward-word) number pos))
	((< number 0)
	 (funcall (or mode-forward-exp 'forward-word) (- number) pos))))

(defun kill-exp (&optional number)
  "Kill the next NUMBER expressions."
  (interactive "p")
  (kill-area (cursor-pos) (funcall (or mode-forward-exp 'forward-word)
				   number)))

(defun backward-kill-exp (&optional number)
  "Kills from the start of this NUMBER'th previous expression to the cursor."
  (interactive "p")
  (kill-area (funcall (or mode-backward-exp 'backward-word) number)
	     (cursor-pos)))

(defun transpose-exps (count)
  "Move the expression before the cursor COUNT expressions forwards."
  (interactive "p")
  (transpose-items (or mode-forward-exp 'forward-word)
		   (or mode-backward-exp 'backward-word)
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
	    (setq start (match-start)
		  end (match-end))
	  (looking-at mode-symbol-regexp)
	  (setq start (cursor-pos)
		end (match-end)))
      (setq start (re-search-backward mode-symbol-regexp)
	    end (match-end)))
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
  (if mode-defun-footer
      (and (re-search-forward mode-defun-footer)
	   (match-end))
    (forward-exp 1 (start-of-defun))))

(defun mark-defun ()
  "Mark the current function definition as a block."
  (interactive)
  (let
      ((start (start-of-defun))
       (end (end-of-defun)))
    (when (and start end)
      (mark-block (or (and (re-search-backward "^[ \t\f]*\n" start)
			   (match-end)) start) end))))


;; Generic expression handling

(defun generic-forward-exp (&optional number pos)
  "Return the position of the NUMBER'th next expression from POS."
  (unless number
    (setq number 1))
  (unless pos
    (setq pos (cursor-pos)))
  (let
      ((ws-re (if (null generic-exp-comment-string)
		  "[\t\f\n ]+"
		(concat "([\t\f\n ]+|("
			(quote-regexp generic-exp-comment-string)
			".*\n))+"))))
    (while (> number 0)
      ;; first, skip white space and comments
      (when (looking-at ws-re pos)
	(setq pos (match-end)))
      (when (> pos (end-of-buffer))
	(error "End of buffer"))
      (let
	  ((c (get-char pos)))
	(cond
	 ((member c generic-exp-single-delims)
	  ;; move over string
	  (if (setq pos (char-search-forward c (forward-char 1 pos)))
	      (while (= (get-char (forward-char -1 pos))
			generic-exp-escape-char)
		(unless (setq pos (char-search-forward c (forward-char 1 pos)))
		  (error "String doesn't end!")))
	    (error "String doesn't end!"))
	  (setq pos (forward-char 1 pos)))
	 ((member c generic-exp-open-delims)
	  ;; move over brackets
	  (unless (setq pos (find-matching-bracket pos nil generic-exp-escape-char))
	    (error "Expression doesn't end!"))
	  (setq pos (forward-char 1 pos)))
	 ((member c generic-exp-close-delims)
	  (error "End of containing expression"))
	 (t
	  ;; a symbol of some sort
	  (if (looking-at generic-exp-symbol-re pos)
	      (setq pos (match-end))
	    (unless (setq pos (re-search-forward generic-exp-special-re pos))
	      (error "Can't find end of symbol"))
	    (setq number (1+ number))))))
      (setq number (1- number))))
  pos)

(defun generic-backward-exp (&optional number orig-pos)
  "Return the position of the NUMBER'th previous s-expression from ORIG-POS."
  (unless number
    (setq number 1))
  (unless orig-pos 
    (setq orig-pos (cursor-pos)))
  (let
      ((pos orig-pos)
       comment-skip-some-re
       comment-skip-line-re)
    (when generic-exp-comment-string
      (setq comment-skip-some-re (concat
				  ".*([\t\f ]*"
				  (quote-regexp generic-exp-comment-string)
				  "|[\f\t ]*)$")
	    comment-skip-line-re (concat
				  "^[\t\f ]*"
				  (quote-regexp generic-exp-comment-string)
				  "|^[\f\t ]*$")))
    (while (> number 0)
      ;; skip preceding white space
      (unless (setq pos (re-search-backward "[^\t\f\n ]" (forward-char -1 pos)))
	(error "No expression!"))
      (when generic-exp-comment-string
	(while (looking-at comment-skip-line-re (start-of-line pos))
	  (unless (setq pos (forward-line -1 pos))
	    (error "Beginning of buffer"))
	  (setq pos (end-of-line pos)))
	(when (and (looking-at comment-skip-some-re (start-of-line pos))
		   (< (match-start 1) pos))
	  (setq pos (forward-char -1 (match-start)))))
      (let
	  ((c (get-char pos)))
	(cond
	 ((member c generic-exp-close-delims)
	  (unless (setq pos (find-matching-bracket pos nil generic-exp-escape-char))
	    (error "Brackets don't match")))
	 ((member c generic-exp-single-delims)
	  (if (setq pos (char-search-backward c (forward-char -1 pos)))
	      (while (= (get-char (forward-char -1 (copy-pos pos)))
			generic-exp-escape-char)
		(unless (setq pos (char-search-backward c (forward-char -1 pos)))
		  (error "String doesn't start!")))
	    (error "String doesn't start!")))
	 ((member c generic-exp-open-delims)
	  (error "Start of containing sexp"))
	 (t
	  ;; a symbol?
	  (if (looking-at generic-exp-symbol-re pos)
	      (unless (setq pos (re-search-backward generic-exp-symbol-re pos))
		(error "Can't classify expression"))
	    (setq number (1+ number))))))
      (setq number (1- number)))
    pos))
