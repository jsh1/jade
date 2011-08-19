;;;; c-mode.jl -- Primitive mode for editing C source
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

(eval-when-compile (require 'info))

(provide 'c-mode)

;; Commentary:
;;
;; This does not claim, or even attempt, to indent C correctly, it uses
;; simple heuristics to do a naive but fairly decent job. The most blatant
;; lossage is that it can't indent continued statements, for example:
;;
;;	x = y
;;	+ z
;;	+ a;
;;
;; instead of:
;;
;;	x = y
;;	    + z
;;	    + a;
;;
;; There is a solution however. Just ensure that expressions like this
;; have some extra parentheses, e.g.:
;;
;;	x = (y
;;	     + z
;;	     + a);
;;
;; Other problems also undoubtedly exist.


;; Configuration
;;
;; Example settings:
;;                   BSD  GNU  K&R
;; c-body-indent      4    2    5
;; c-brace-indent    -4    0   -5
;; c-case-indent     -4   -2   -5
;; c-label-indent    -4   -2   -5

(defvar c-body-indent 4
  "Indentation of code with respect to its containing block.")
(make-variable-buffer-local 'c-body-indent)

(defvar c-brace-indent -4
  "Extra indentation of braces relative to the body of the code they
contain.")
(make-variable-buffer-local 'c-brace-indent)

(defvar c-case-indent -4
  "Extra indentation for case statements.")
(make-variable-buffer-local 'c-case-indent)

(defvar c-label-indent -4
  "Extra indentation for labels.")
(make-variable-buffer-local 'c-label-indent)

(defvar c-styles
  '((bsd
     (c-body-indent . 4)
     (c-brace-indent . -4)
     (c-case-indent . -4)
     (c-label-indent . -4))
    (gnu
     (c-body-indent . 2)
     (c-brace-indent . 0)
     (c-case-indent . -2)
     (c-label-indent . -2))
    (k&r
     (c-body-indent . 5)
     (c-brace-indent . -5)
     (c-case-indent . -5)
     (c-label-indent . -5))
    (linux
     (c-body-indent . 8)
     (c-brace-indent . -8)
     (c-case-indent . -8)
     (c-label-indent . -8)))
  "Alist of C styles.")

(defvar c-style 'bsd
  "C style of current buffer.")
(make-variable-buffer-local 'c-style)


;; Variables

(defvar c-mode-keymap
  (bind-keys (make-sparse-keymap)
    "{" 'c-open-brace
    "}" 'c-close-brace
    ":" 'c-colon
    "TAB" 'indent-line))

(defvar c-mode-ctrl-c-keymap
  (bind-keys (make-sparse-keymap)
    "Ctrl-\\" 'c-backslash-area))


;; Code

(defun c-set-style (name)
  (interactive "SStyle:")
  (setq c-style name)
  (let ((style (cdr (assoc c-style c-styles))))
    (when style
      (mapc (lambda (x) (set (car x) (cdr x))) style))))

;;;###autoload
(defun c-mode ()
  "C Mode:\n
Simple mode for editing C source code. Its main feature is to be able to
indent lines to their (probably) correct depth.\n
Commands defined by this mode are:\n
\\{c-mode-keymap}\\{c-mode-ctrl-c-keymap,Ctrl-c}"
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (when c-style
    (let ((style (cdr (assoc c-style c-styles))))
      (when style
	(mapc (lambda (x) (set (car x) (cdr x))) style))))
  (setq mode-name "C"
	major-mode 'c-mode
	major-mode-kill kill-all-local-variables
	mode-comment-fun c-insert-comment
	mode-indent-line c-indent-line
	mode-forward-exp c-forward-exp
	mode-backward-exp c-backward-exp
	mode-defun-header "^([a-zA-Z0-9_]+)[\t ]*\\([^}]+\n{"
	mode-defun-footer "^}"
	paragraph-separate "^[\n\t\f ]*\n"
	paragraph-start paragraph-separate
	local-ctrl-c-keymap c-mode-ctrl-c-keymap
	local-keymap c-mode-keymap)
  (make-local-variable 'info-documentation-files)
  (setq info-documentation-files '("libc"))
  (call-hook 'c-mode-hook))

(defun c-open-brace ()
  (interactive)
  (insert "{")
  (indent-line))

(defun c-close-brace ()
  (interactive)
  (insert "}")
  (indent-line)
  (blinking-insert))

(defun c-colon ()
  (interactive)
  (insert ":")
  (indent-line))


;; Indentation

(defun c-indent-line (#!optional p)
  "Indent the line at POS (or the cursor) assuming that it's C source code."
  (set-indent-pos (c-indent-pos p)))

;; Attempt to find the previous statement. perl-mode also uses this
(defun c-backward-stmt (p #!optional skip-blocks)
  (let
      (stmt-pos back-1-pos)
    (condition-case nil
	(while (setq p (c-backward-exp 1 p (not skip-blocks)))
	  (cond
	   ((null back-1-pos)
	    (unless (= (get-char p) #\{)
	      (setq back-1-pos p)))
	   ((/= (pos-line back-1-pos) (pos-line p))
	    ;; Gone past the start of this line, break the loop
	    (error "Ignored")))
	  (setq stmt-pos p))
      (error))
    stmt-pos))

;; POS should point to an `else' keyword, the position of it's matching `if'
;; will be returned.
(defun c-balance-ifs (p #!optional depth)
  (unless depth
    (setq depth 1))
  (while (and (/= depth 0) (setq p (c-backward-stmt p t)))
    (cond
     ((and (looking-at "else[\t ]*" p)
	   (not (looking-at "[\t ]*if[\t ]*\\(" (match-end))))
      (setq depth (1+ depth)))
     ((looking-at "if" p)
      (setq depth (1- depth)))))
  (when (zerop depth)
    p))

;; Work out where to indent LINE-POS to.
(defun c-indent-pos (#!optional line-pos)
  (setq line-pos (start-of-line line-pos))
  ;; Check for cpp op
  (if (looking-at "^[\t ]*#" line-pos)
      ;; Always indent preprocessor lines to the leftmost column
      (pos 0 (pos-line line-pos))
    (let
	((p line-pos)
	 (exp-pos (c-backward-stmt line-pos))
	 exp-ind)

      ;; Find the beginning of the expression to indent relative to
      (unless exp-pos
	;; Start of the containing expression
	(when (re-search-backward "[\{\(\[]" p)
	  (setq exp-pos (match-start))))
      (setq exp-ind (char-to-glyph-pos exp-pos))

      (unless (or (equal (indent-pos exp-pos) exp-ind)
		  (memq (get-char (forward-char -1 exp-pos)) '(?\( ?\{ ?\[)))
	(when (and (looking-at "^[\t ]*([^][(){}\"'a-zA-Z0-9_\t ]+)"
			       (start-of-line exp-pos))
		   (< (match-start 1) exp-pos))
	  ;; Back up over the bits of punctuation
	  (setq exp-ind (char-to-glyph-pos (match-start 1)))))

      ;; First look at previous line and see how it affects the one we're
      ;; trying to indent
      (cond
       ((= (get-char exp-pos) ?\})
	;; A closing brace
	(unless (zerop (pos-col exp-pos))
	  (setq exp-ind (left-char (+ c-body-indent c-brace-indent) exp-ind))))

       ((looking-at ".*{" exp-pos)
	;; An opening brace
	(setq exp-ind (right-char c-body-indent (indent-pos exp-pos))))

       ((and (/= (pos-col exp-pos) 0)
	     (or #| (looking-at "(struct|union|enum)\\s" exp-pos) |#
		 (looking-at "(static|const)\\s.*\\s=\\s*$" exp-pos)))
	;; Something else that causes the next statement to be
	;; indented one level
	(setq exp-ind (right-char c-body-indent exp-ind)))

       ((looking-at
	 "(if|for|while|switch)[\t ]*\\(.*$|(else|do)([^a-zA-Z0-9_]|$)"
	 exp-pos)
	;; Something that causes the next statement to be
	;; indented one level
	(setq exp-ind (right-char c-body-indent exp-ind)))

       ((looking-at ".*\;[\t ]*(\n|/\\*)" exp-pos)
	;; A full expression, indent to the level of the first
	;; line in the expression
	(let
	    ((prev (c-backward-stmt exp-pos t)))
	  ;; *Need to loop here searching back to the correct level*
	  (when (and prev (/= (pos-col prev) (pos-col exp-pos))
		     (not (looking-at "case .*:|default[\t ]*:|[a-zA-Z_][a-zA-Z0-9_]+:|.*;[\t ]*(\n|/\\*)" prev)))
	    ;; A continuation?
	    (when (and (looking-at "else[\t ]*" prev)
		       (not (looking-at "[\t ]*if[\t ]*\\(" (match-end))))
	      (unless (setq prev (c-balance-ifs prev))
		(error "Beginning of buffer"))
	      (let
		  ((tmp (c-backward-stmt prev)))
		(while (and tmp (looking-at "if[\t ]*\\(" tmp))
		  (setq prev tmp)
		  (unless (setq tmp (c-backward-stmt tmp))
		    (error "Beginning of buffer")))))
	    (setq exp-ind (pos (pos-col (char-to-glyph-pos prev))
			       (pos-line exp-ind))))))

       ((looking-at "case .*:|default[\t ]*:" exp-pos)
	;; A switch-statement label, these are indented back by c-case-indent
	(setq exp-ind (left-char c-case-indent exp-ind)))

       ((looking-at "[a-zA-Z_][a-zA-Z0-9_]+:([\t ]|$)" exp-pos)
	;; A goto label, indented back by c-label-indent
	(setq exp-ind (or (left-char c-label-indent exp-ind)
			  (pos 0 (pos-line exp-pos))))))

      ;; Next, look at the contents of this line and see if it needs any
      ;; special treatment
      (unless (empty-line-p line-pos)
	;; Skip leading whitespace
	(when (looking-at "^[\t\f ]+" line-pos)
	  (setq line-pos (match-end)))

	(cond
	 ((= (get-char line-pos) ?\{)
	  ;; An opening brace at the start of the line, indent back by
	  ;; c-brace-indent
	  (setq exp-ind (pos (max 0 (+ (pos-col exp-ind) c-brace-indent)))))

	 ((= (get-char line-pos) ?\})
	  ;; A closing brace, indent outwards by c-brace-indent
	  (setq exp-ind (left-char c-body-indent exp-ind)))

	 ((looking-at "case .*:|default[\t ]*:" line-pos)
	  ;; A switch label
	  (setq exp-ind (right-char c-case-indent exp-ind)))

	 ((looking-at "[a-zA-Z_]+[a-zA-Z0-9_]*:([\t ]|$)" line-pos)
	  ;; A goto label
	  (setq exp-ind (right-char c-label-indent exp-ind)))))

      ;; Finished
      (pos (pos-col exp-ind) (pos-line line-pos)))))


;; Movement over C expressions. perl-mode also uses these functions
;; (that's why they allow $ and @ characters in symbols)

(defun c-forward-exp (#!optional number p)
  (unless number
    (setq number 1))
  (while (> number 0)
    ;; first, skip empty lines & comments
    (while (looking-at "[\t\f ]*$|[\t\f ]*/\\*.*$" p)
      (if (looking-at "[\t\f ]*/\\*" p)
	  (progn
	    (unless (re-search-forward "\\*/" p)
	      (error "Comment doesn't end!"))
	    (setq p (match-end)))
	(setq p (forward-line 1 (start-of-line p)))
	(when (> p (end-of-buffer))
	  (error "End of buffer"))))
    ;; Check for a cpp line
    (if (looking-at "^[\t ]*#" (start-of-line p))
	(setq p (end-of-line p))
      ;; now any other whitespace
      (when (looking-at "[\t\f ]+" p)
	(setq p (match-end)))
      ;; Skip weird stuff
      (while (looking-at "[!*~&<>/+%?:^-]+" p)
	(setq p (match-end))
	(when (equal p (end-of-line p))
	  (setq p (forward-char 1 p))))
      (let
	  ((c (get-char p)))
	(cond
	 ((member c '(?\" ?\'))
	  ;; move over string/character
	  (if (setq p (char-search-forward c (forward-char 1 p)))
	      (while (= (get-char (forward-char -1 p)) ?\\ )
		(unless (setq p (char-search-forward c (forward-char 1 p)))
		  (error "String doesn't end!")))
	    (error "String doesn't end!"))
	  (setq p (forward-char 1 p)))
	 ((member c '(?\( ?\[ ?\{))
	  ;; move over brackets
	  (unless (setq p (find-matching-bracket p))
	    (error "Expression doesn't end!"))
	  (setq p (forward-char 1 p)))
	 ((member c '(?, ?\; ?:))
	  (setq p (forward-char 1 p)
		number (1+ number)))
	 ((member c '(?\) ?\] ?\}))
	  (error "End of containing expression"))
	 (t
	  ;; a symbol?
	  (if (looking-at "[a-zA-Z0-9_$@]+" p)
	      (setq p (match-end))
	    (unless (setq p (re-search-forward "[][$@a-zA-Z0-9_ \t\f(){}'\"]"
						p))
	      (error "Can't classify expression"))
	    (setq number (1+ number))))))
      (setq number (1- number))))
  p)
  
(defun c-backward-exp (#!optional number orig-pos no-blocks)
  (unless number
    (setq number 1))
  (unless orig-pos 
    (setq orig-pos (cursor-pos)))
  (let
      ((p orig-pos)
       tmp)
    (while (> number 0)
      ;; skip preceding white space
      (when (or (equal p (start-of-buffer))
		(not (setq p (re-search-backward "[^\t\f\n ]"
						 (forward-char -1 p)))))
	(error "No expression!"))
      (setq tmp (forward-char -1 p))
      (while (looking-at "\\*/" tmp)
	;; comment to skip
	(unless (setq tmp (re-search-backward "/\\*" tmp))
	  (error "Comment doesn't start!"))
	(when (or (equal tmp (start-of-buffer))
		  (not (setq tmp (re-search-backward "[^\t\f\n ]"
						   (forward-char -1 tmp)))))
	  (error "Beginning of buffer"))
	(setq p tmp))
      ;; Check for a cpp line
      (if (looking-at "^[\t ]*#" (start-of-line p))
	  (setq p (start-of-line p))
	(let
	    ((c (get-char p)))
	  (cond
	   ((member c '(?\) ?\] ?\}))
	    (when (or (/= c ?\}) (not no-blocks))
	      (unless (setq p (find-matching-bracket p))
		(error "Brackets don't match"))))
	   ((member c '(?\" ?\'))
	    (if (setq p (char-search-backward c (forward-char -1 p)))
		(while (= (get-char (forward-char -1 p)) ?\\ )
		  (unless (setq p (char-search-backward c (forward-char -1 p)))
		    (error "String doesn't start!")))
	      (error "String doesn't start!")))
	   ((member c '(?\; ?: ?,))
	    ;; loop again
	    (setq number (1+ number)))
	   ((member c '(?\( ?\[ ?\{))
	    (error "Start of containing expression"))
	   (t
	    ;; a symbol?
	    (if (looking-at "[$@a-zA-Z0-9_]" p)
		(unless (setq p (re-search-backward
				   "(^#[\t ]*|)[$@a-zA-Z0-9_]+" p))
		  (error "Can't classify expression"))
	      ;; Assume that it's some extraneous piece of punctuation..
;	      (unless (setq p (re-search-backward
;				 "[][$@a-zA-Z0-9_ \t\f(){}'\"]" p))
;		(error "Can't classify expression"))
	      (setq number (1+ number)))))
	  (when (member (get-char (forward-char -1 p)) '(?! ?* ?- ?~))
	    ;; unary operator, skip over it
	    (setq p (forward-char -1 p))))
	(setq number (1- number))))
    p))


;; Miscellaneous functions

;;;###autoload
(defun c-backslash-area (start end)
  "Insert (or align) backslash characters at the end of each line in between
START and END except for the last line."
  (interactive "-m\nM")
  (let
      ((max-width 0)
       (p (start-of-line start))
       tmp)
    (while (<= p end)
      (setq tmp (char-to-glyph-pos (if (looking-at ".*([\t ]*\\\\ *)$" p)
				       (match-start 1)
				     (end-of-line p))))
      (when (> (pos-col tmp) max-width)
	(setq max-width (pos-col tmp)))
      (setq p (forward-line 1 p)))
    (setq max-width (1+ max-width))
    (unless (= (% max-width (tab-size)) 0)
      (setq max-width (* (1+ (quotient max-width (tab-size))) (tab-size))))
    (setq p (pos max-width (pos-line start)))
    (while (< p end)
      (when (looking-at ".*([\t ]*\\\\ *)$" (start-of-line p))
	(delete-area (match-start 1) (match-end 1)))
      (goto (end-of-line p))
      (indent-to max-width)
      (insert "\\")
      (setq p (forward-line 1 p)))
    (goto end)))

(defun c-insert-comment ()
  (interactive)
  (if (looking-at ".*(/\\* ?)" (start-of-line))
      (goto (match-end 1))
    (find-comment-pos)
    (insert "/*  */")
    (goto (forward-char -3))))
