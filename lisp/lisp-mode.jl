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

(provide 'lisp-mode)

(defvar symbol-word-regexps ["[^][()?'\"#; ]" "[][()?'\"#; ]|$"])

(defvar lisp-mode-keymap (make-keylist))
(bind-keys lisp-mode-keymap
  "Ctrl-j"	'eval-insert-sexp
  "Meta-Ctrl-x"	'eval-print-sexp
  "TAB"		'indent-line)

;;;###autoload
(defun lisp-mode ()
  "Lisp Mode:\n
Major mode for editing Lisp source. Special commands in this mode are,\n
  `Ctrl-j'		Evaluate the current s-expression as Jade Lisp code
			and insert its value into the current buffer.
  `TAB'			Indent the current line.
  `Ctrl-Meta-x'		Evaluate the current sexp and print its value in
			the status line.
  `Ctrl-Meta-f'		Move to the past the current s-expression.
  `Ctrl-Meta-b'		Move to the beginning of the current sexp.
  `Ctrl-Meta-k'		Kill from the cursor to the end of this sexp.
  `Ctrl-Meta-BS'	Kill from the start of this sexp to the cursor."
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "Lisp"
	major-mode 'lisp-mode
	major-mode-kill 'lisp-mode-kill
	mode-comment-fun 'lisp-insert-comment
	mode-indent-line 'lisp-indent-line
	mode-forward-exp 'lisp-forward-sexp
	mode-backward-exp 'lisp-backward-sexp
	keymap-path (cons 'lisp-mode-keymap keymap-path))
  (eval-hook 'lisp-mode-hook)
  t)

(defun lisp-mode-kill ()
  (setq keymap-path (delq 'lisp-mode-keymap keymap-path)
	major-mode nil
	major-mode-kill nil
	mode-comment-fun nil
	mode-indent-line nil
	mode-forward-exp nil
	mode-backward-exp nil
	mode-name nil)
  t)

;; Now lisp-mode is loaded we may as well make the *jade* buffer use it
(with-buffer default-buffer
  (unless major-mode
    (lisp-mode)))

;;;###autoload
(defun eval-sexp ()
  "Evaluates the Lisp expression before the cursor and returns its value."
  (interactive)
  (goto-char (lisp-backward-sexp))
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

(defun lisp-indent-line (&optional pos)
  (unless pos
    (setq pos (cursor-pos)))
  (set-indent-pos (lisp-indent-pos pos)))

(defun lisp-insert-comment ()
  (interactive)
  (find-comment-pos)
  (insert ";"))


;; Expressions

(defun lisp-forward-sexp (&optional number pos)
  "Return the position of the NUMBER'th next s-expression from POS."
  (unless number
    (setq number 1))
  (while (> number 0)
    ;; first, skip empty lines & comments
    (while (looking-at "[\t\f ]*$|[\t\f ]*;.*$" pos)
      (setq pos (next-line 1 (line-start pos)))
      (when (> pos (buffer-end))
	(error "End of buffer")))
    ;; now any other whitespace
    (when (looking-at "[\t\f ]+" pos)
      (setq pos (match-end)))
    (let
        ((c (get-char pos)))
      (cond
       ((= c ?\")
	;; move over string
	(if (setq pos (find-next-char ?\" (next-char 1 pos)))
	    (while (= (get-char (prev-char 1 (copy-pos pos))) ?\\ )
	      (unless (setq pos (find-next-char ?\" (next-char 1 pos)))
		(error "String doesn't end!")))
	  (error "String doesn't end!"))
	(setq pos (next-char 1 pos)))
       ((member c '(?\( ?\[ ?\<))
	;; move over brackets
	(unless (setq pos (match-brackets pos))
	  (error "Expression doesn't end!"))
	(setq pos (next-char 1 pos)))
       ((member c '(?' ?#))
	;; iterate one more time
	(setq number (1+ number)
	      pos (next-char 1 pos)))
       ((member c '(?\) ?\]))
	(error "End of containing sexp"))
       (t
	;; a symbol
       (if (looking-at "[^][\t\f\n ()'\";]+" pos)
	   (setq pos (match-end))
	 (error "Can't find end of symbol")))))
    (setq number (1- number)))
  pos)

(defun lisp-backward-sexp (&optional number orig-pos)
  "Return the position of the NUMBER'th previous s-expression from ORIG-POS."
  (unless number
    (setq number 1))
  (unless orig-pos 
    (setq orig-pos (cursor-pos)))
  (let
      ((pos (copy-pos orig-pos)))
    (while (> number 0)
      ;; skip preceding white space
      (unless (setq pos (find-prev-regexp "[^\t\f\n ]" (prev-char 1 pos)))
	(error "No expression!"))
      (while (regexp-match-line "^[\f\t ]*;|^[\f\t ]*$" pos)
	(unless (setq pos (prev-line 1 pos))
	  (error "Beginning of buffer"))
	(setq pos (line-end pos)))
      (when (if (/= (pos-line orig-pos) (pos-line pos))
		(regexp-match-line "[\f\t ]+;|[\f\t ]*$" pos)
	      (regexp-match-line "[\f\t ]+;" pos))
	(setq pos (prev-char 1 (match-start))))
      (let
	  ((c (get-char pos)))
	(cond
	 ((member c '(?\) ?\] ?\>))
	  (unless (setq pos (match-brackets pos))
	    (error "Brackets don't match"))
	  (when (= c ?\>)
	    (prev-char 1 pos)))
	 ((= c ?\")
	  (if (setq pos (find-prev-char ?\" (prev-char 1 pos)))
	      (while (= (get-char (prev-char 1 (copy-pos pos))) ?\\ )
		(unless (setq pos (find-prev-char ?\" (prev-char 1 pos)))
		  (error "String doesn't start!")))
	    (error "String doesn't start!")))
	 ((member c '(?\( ?\[))
	  (error "Start of containing sexp"))
	 (t
	  ;; a symbol?
	 (unless (setq pos (find-prev-regexp "[^][\f\t\n ()'\"]+|^" pos))
	   (error "Symbol doesn't start??"))))
	(when (= (get-char (prev-char 1 (copy-pos pos))) ?')
	  (setq pos (prev-char 1 pos))
	  (when (= (get-char (prev-char 1 (copy-pos pos))) ?#)
	    (setq pos (prev-char 1 pos)))))
      (setq number (1- number)))
    pos))


;; Indentation

(defvar lisp-body-indent 2
  "Number of columns to indent code bodies by.")

(defun lisp-indent-pos (&optional line-pos)
  "Returns the correct indentation position for the specified line."
  (unless line-pos
    (setq line-pos (cursor-pos)))
  (let*
      ((pos (line-start line-pos))
       (index 0)
       (sexp-ind (copy-pos pos))
       last-ind
       (form-pos (buffer-end))
       form)
    (if (looking-at "^[\t\f ]*(;;;|;[^;])" pos)
	(set-pos-col sexp-ind (if (looking-at "^[\t\f ]*;;;" pos)
				  0
				(1- comment-column)))
      ;; Work back to the beginning of the containing sexp. The error-handler
      ;; catches the error that's signalled when the start is reached.
      (error-protect
	  (while (setq pos (lisp-backward-sexp 1 pos))
	    (when (<= form-pos pos)
	      (error "Infinite loop"))
	    (when (zerop (pos-col pos))
	      (set-pos-col sexp-ind 0)
	      (return sexp-ind))
	    (setq form-pos pos
		  index (1+ index))
	    (when (or (null last-ind) (= (pos-line (car last-ind))
					 (pos-line pos)))
	      (setq last-ind (cons (char-to-glyph-pos pos) last-ind))))
	(error))
      ;; If there weren't any previous sexps to indent against stop now
      (unless (zerop index)
	(if last-ind
	    (setq last-ind (if (and (= (pos-line pos) (pos-line (car last-ind)))
				    (>= (length last-ind) 2))
			       (nth 1 last-ind)
			     (car last-ind)))
	  (setq last-ind (copy-pos pos)))
	;; pos now points to the first sexp in the containing sexp
	(set-pos-col sexp-ind
		     (pos-col (char-to-glyph-pos
			       (or (find-prev-regexp "[\(\[]" pos)
				   pos))))
	(setq form-pos (copy-pos pos)
	      form (read (cons (current-buffer) form-pos)))
	(when (symbolp form)
	  (let
	      ((type (get form 'lisp-indent)))
	    (cond
	     ((null type)
	      ;; standard indentation
	      (if (= (- (pos-line line-pos) (pos-line pos)) 1)
		  ;; on the second line of this sexp
		  (if (< index 2)
		      (set-pos-col sexp-ind (pos-col (char-to-glyph-pos pos)))
		    (when (looking-at "[\t\f ]*" form-pos)
		      (set-pos-col sexp-ind
				   (pos-col (char-to-glyph-pos
					     (match-end))))))
		(set-pos-col sexp-ind (pos-col last-ind))))
	     ((eq type 'defun)
	      ;; defun type indentation
	      (if (or (= index 2) (= (- (pos-line line-pos) (pos-line pos)) 1))
		  (right-char lisp-body-indent sexp-ind)
		(set-pos-col sexp-ind (pos-col last-ind))))
	     ((numberp type)
	      ;; first TYPE sexps are indented double
	      (right-char (if (<= index type)
			      (* 2 lisp-body-indent)
			    lisp-body-indent)
			  sexp-ind)))))))
    sexp-ind))

;; Set up indentation hints

(put 'let 'lisp-indent 1)
(put 'let* 'lisp-indent 1)
(put 'if 'lisp-indent 2)
(put 'when 'lisp-indent 1)
(put 'unless 'lisp-indent 1)
(put 'while 'lisp-indent 1)
(put 'lambda 'lisp-indent 'defun)
(put 'defun 'lisp-indent 'defun)
(put 'defmacro 'lisp-indent 'defun)
(put 'defvar 'lisp-indent 'defun)
(put 'defconst 'lisp-indent 'defun)
(put 'progn 'lisp-indent 0)
(put 'prog1 'lisp-indent 1)
(put 'prog2 'lisp-indent 2)
(put 'unwind-protect 'lisp-indent 1)
(put 'error-protect 'lisp-indent 1)
(put 'with-buffer 'lisp-indent 1)
(put 'with-window 'lisp-indent 1)
(put 'with-view 'lisp-indent 1)
(put 'catch 'lisp-indent 1)
(put 'bind-keys 'lisp-indent 1)
(put 'unbind-keys 'lisp-indent 1)
(put 'save-restriction 'lisp-indent 0)
(put 'save-cursor 'lisp-indent 0)
