;;;; lisp-mode.jl -- Simple mode for editing Lisp files
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

(require 'completion)
(provide 'lisp-mode)

(defvar symbol-word-regexps ["[^][()?'`,@\"#; ]" "[][()?'`,@\"#; ]|$"])

(defvar lisp-mode-keymap
  (bind-keys (make-sparse-keymap)
    "Ctrl-j" 'eval-insert-sexp
    "Meta-Ctrl-x" 'eval-print-sexp
    "TAB" 'indent-line))

;;;###autoload
(defun lisp-mode ()
  "Lisp Mode:\n
Major mode for editing Lisp source. Local bindings in this mode are:\n
\\{lisp-mode-keymap}"
  (interactive)
  (when major-mode-kill
    (major-mode-kill (current-buffer)))
  (set! mode-name "Lisp")
  (set! major-mode 'lisp-mode)
  (set! major-mode-kill kill-all-local-variables)
  (set! mode-comment-header ";")
  (set! mode-indent-line lisp-indent-line)
  (set! mode-forward-exp lisp-forward-sexp)
  (set! mode-backward-exp lisp-backward-sexp)
  (set! mode-symbol-regexp "[^][()?'`,@\"#; \t\f\n]+")
  (set! mode-defun-header "^\\s*\\(def[a-z]+\\s+\\(?([^ \t\f\n)]+)")
  (set! mode-defun-footer nil)
  (set! paragraph-separate "^[\n\t\f ]*\n")
  (set! paragraph-start paragraph-separate)
  (set! local-keymap lisp-mode-keymap)
  (set! completion-hooks (cons lisp-complete-sexp completion-hooks))
  (make-local-variable 'info-documentation-files)
  (set! info-documentation-files '("librep"))
  (call-hook 'lisp-mode-hook))

;;;###autoload
(defun eval-sexp ()
  "Evaluates the Lisp expression before the cursor and returns its value."
  (interactive)
  (goto (lisp-backward-sexp))
  (eval (read (current-buffer))))

;;;###autoload
(defun eval-insert-sexp ()
  "Evaluates the Lisp expression before the cursor, then inserts its value
into the buffer."
  (interactive)
  (format (current-buffer) "\n%S\n" (eval-sexp)))

;;;###autoload
(defun eval-print-sexp ()
  "Evaluates the Lisp expression before the cursor, then displays its value
in the status line."
  (interactive)
  (prin1 (eval-sexp) t))

;;;###autoload
(defun eval-and-print (form)
  "Eval FORM then print its value in the status line."
  (interactive "xEval:")
  (prin1 (eval form) t))

(defun lisp-indent-line (#!optional p)
  (unless p
    (set! p (cursor-pos)))
  (set-indent-pos (lisp-indent-pos p)))

(defun lisp-complete-sexp (sexp beg end)
  (declare (unused end))
  (let
      ((is-function nil))
    (when (and (> beg (start-of-buffer))
	       (= (get-char (forward-char -1 beg)) #\())
      (set! is-function t))
    (mapcar symbol-name (apropos (concat #\^ (quote-regexp sexp))
				 (if is-function
				     (lambda (x)
				       (and (variable-bound? x)
					    (function?
					     (variable-ref x))))
				   variable-bound?)))))


;; Expressions

(defun lisp-forward-sexp (#!optional number p)
  "Return the position of the NUMBER'th next s-expression from POS."
  (unless number
    (set! number 1))
  (while (> number 0)
    ;; first, skip empty lines & comments
    (while (looking-at "[\t\f ]*$|[\t\f ]*;.*$" p)
      (set! p (forward-line 1 (start-of-line p)))
      (when (> p (end-of-buffer))
	(error "End of buffer")))
    ;; now any other whitespace
    (when (looking-at "[\t\f ]+" p)
      (set! p (match-end)))
    (while (member (get-char p) '(#\' #\# #\` #\, #\@))
      (set! p (forward-char 1 p)))
    (let
        ((c (get-char p)))
      (cond
       ((= c #\")
	;; move over string
	(set! p (char-search-forward #\" (forward-char 1 p)))
	(or p (error "String doesn't end!"))
	(while (= (get-char (forward-char -1 p)) #\\)
	  (set! p (char-search-forward #\" (forward-char 1 p)))
	  (or p (error "String doesn't end!")))
	(set! p (forward-char 1 p)))
       ((member c '(#\( #\[ #\<))
	;; move over brackets
	(set! p (find-matching-bracket p))
	(or p (error "Expression doesn't end!"))
	(set! p (forward-char 1 p)))
       ((member c '(#\) #\]))
	(error "End of containing sexp"))
       (t
	;; a symbol
       (if (looking-at "[^][\t\f\n ()'\";]+" p)
	   (set! p (match-end))
	 (error "Can't find end of symbol")))))
    (set! number (1- number)))
  p)

(defun lisp-backward-sexp (#!optional number orig-pos)
  "Return the position of the NUMBER'th previous s-expression from ORIG-POS."
  (unless number
    (set! number 1))
  (unless orig-pos 
    (set! orig-pos (cursor-pos)))
  (let
      ((p orig-pos))
    (while (> number 0)
      ;; skip preceding white space
      (set! p (re-search-backward "[^\t\f\n ]" (forward-char -1 p)))
      (or p (error "No expression!"))
      (while (looking-at "^[\f\t ]*;|^[\f\t ]*$" (start-of-line p))
	(set! p (forward-line -1 p))
	(or p (error "Beginning of buffer"))
	(set! p (end-of-line p)))
      (when (if (/= (pos-line orig-pos) (pos-line p))
		(looking-at ".*([\f\t ]+;|[\f\t ]*$)" (start-of-line p))
	      (looking-at ".*([\f\t ]+;)" (start-of-line p)))
	(set! p (forward-char -1 (match-start 1))))
      (let
	  ((c (get-char p)))
	(cond
	 ((member c '(#\) #\] #\>))
	  (set! p (find-matching-bracket p))
	  (or p (error "Brackets don't match"))
	  (when (= c #\>)
	    (forward-char -1 p)))
	 ((= c #\")
	  (set! p (char-search-backward #\" (forward-char -1 p)))
	  (or p (error "String doesn't start!"))
	  (while (= (get-char (forward-char -1 p)) #\\)
	    (set! p (char-search-backward #\" (forward-char -1 p)))
	    (or p (error "String doesn't start!"))))
	 ((member c '(#\( #\[))
	  (error "Start of containing sexp"))
	 (t
	  ;; a symbol?
	  (set! p (re-search-backward "[^][\f\t\n ()'\"]+|^" p))
	  (or p (error "Symbol doesn't start??"))))
	(while (member (get-char (forward-char -1 p)) '(#\' #\# #\` #\, #\@))
	  (set! p (forward-char -1 p))))
      (set! number (1- number)))
    p))


;; Indentation

(defvar lisp-body-indent 2
  "Number of columns to indent code bodies by.")

(defun lisp-indent-pos (#!optional line-pos)
  "Returns the correct indentation position for the specified line."
  (unless line-pos
    (set! line-pos (cursor-pos)))
  (catch 'return
    (let*
	((p (start-of-line line-pos))
	 (index 0)
	 (sexp-ind p)
	 last-ind
	 (form-pos (end-of-buffer))
	 form)
      (if (looking-at "^[\t\f ]*(;;;|;[^;])" p)
	  (set! sexp-ind (pos (if (looking-at "^[\t\f ]*;;;" p)
				  0
				(1- comment-column))
			      (pos-line sexp-ind)))
	;; Work back to the beginning of the containing sexp. The error-handler
	;; catches the error that's signalled when the start is reached.
	(condition-case nil
	    (let loop ()
	      (set! p (lisp-backward-sexp 1 p))
	      (when p
		(when (<= form-pos p)
		  (error "Infinite loop"))
		(when (zero? (pos-col p))
		  (set! sexp-ind (pos 0 (pos-line sexp-ind)))
		  (throw 'return sexp-ind))
		(set! form-pos p)
		(set! index (1+ index))
		(when (or (null? last-ind) (= (pos-line (car last-ind))
					      (pos-line p)))
		  (set! last-ind (cons (char-to-glyph-pos p) last-ind)))
		(loop)))
	  (error))
	;; If there weren't any previous sexps to indent against stop now
	(unless (zero? index)
	  (if last-ind
	      (set! last-ind (if (and (= (pos-line p)
					 (pos-line (car last-ind)))
				      (>= (list-length last-ind) 2))
				 (list-ref last-ind 1)
			       (car last-ind)))
	    (set! last-ind p))
	  ;; pos now points to the first sexp in the containing sexp
	  (set! sexp-ind (pos (pos-col (char-to-glyph-pos
					(or (re-search-backward "[([]" p)
					    p)))
			      (pos-line sexp-ind)))
	  (set! form (read (cons (current-buffer) p)))
	  (when (symbol? form)
	    (let
		((type (get form 'lisp-indent)))
	      (cond
	       ((null? type)
		;; standard indentation
		(if (and (= (- (pos-line line-pos) (pos-line p)) 1)
			 (< index 2))
		    ;; on the second line of this sexp, with the first
		    ;; argument, line up under the function name
		    (set! sexp-ind (pos (pos-col (char-to-glyph-pos p))
					(pos-line sexp-ind)))
		  ;; otherwise line up under the first argument
		  (set! sexp-ind (pos (pos-col last-ind)
				      (pos-line sexp-ind)))))
	       ((eq? type 'defun)
		;; defun type indentation
		(if (or (= index 2)
			(= (- (pos-line line-pos) (pos-line p)) 1))
		    (set! sexp-ind (right-char lisp-body-indent sexp-ind))
		  (set! sexp-ind (pos (pos-col last-ind)
				      (pos-line sexp-ind)))))
	       ((number? type)
		;; first TYPE sexps are indented double
		(set! sexp-ind (right-char (if (<= index type)
					       (* 2 lisp-body-indent)
					     lisp-body-indent)
					   sexp-ind))))))))
      sexp-ind)))

;; Set up indentation hints

(put 'let 'lisp-indent 1)
(put 'let* 'lisp-indent 1)
(put 'letrec 'lisp-indent 1)
(put 'let-fluids 'lisp-indent 1)
(put 'let-escape 'lisp-indent 1)
(put 'if 'lisp-indent 2)
(put 'when 'lisp-indent 1)
(put 'unless 'lisp-indent 1)
(put 'while 'lisp-indent 1)
(put 'lambda 'lisp-indent 'defun)
(put 'object 'lisp-indent 'defun)
(put 'defun 'lisp-indent 'defun)
(put 'defmacro 'lisp-indent 'defun)
(put 'defsubst 'lisp-indent 'defun)
(put 'defvar 'lisp-indent 'defun)
(put 'define-special-variable 'lisp-indent 'defun)
(put 'defconst 'lisp-indent 'defun)
(put 'defface 'lisp-indent 'defun)
(put 'progn 'lisp-indent 0)
(put 'prog1 'lisp-indent 1)
(put 'prog2 'lisp-indent 2)
(put 'unwind-protect 'lisp-indent 1)
(put 'condition-case 'lisp-indent 2)
(put 'with-object 'lisp-indent 1)
(put 'with-buffer 'lisp-indent 1)
(put 'with-window 'lisp-indent 1)
(put 'with-view 'lisp-indent 1)
(put 'catch 'lisp-indent 1)
(put 'bind-keys 'lisp-indent 1)
(put 'lazy-bind-keys 'lisp-indent 2)
(put 'unbind-keys 'lisp-indent 1)
(put 'save-restriction 'lisp-indent 0)
(put 'save-excursion 'lisp-indent 0)
(put 'case 'lisp-indent 1)
(put 'define 'lisp-indent 1)
(put 'do 'lisp-indent 2)

(put 'define-module 'lisp-indent '3)
(put 'define-interface 'lisp-indent '1)
(put 'module 'lisp-indent '2)
(put 'define-record-type 'lisp-indent 'defun)
(put 'define-record-discloser 'lisp-indent 'defun)
(put 'define-self-test 'lisp-indent 'defun)
(put 'define-macro 'lisp-indent 1)

;; Now lisp-mode is loaded we may as well make the *jade* buffer use it
(with-buffer default-buffer
  (unless major-mode
    (lisp-mode)))
