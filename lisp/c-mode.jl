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

(provide 'c-mode)

;; Example settings:
;;                   BSD  GNU  K&R
;; c-body-indent      4    2    5
;; c-brace-indent    -4    0   -5
;; c-case-indent     -4   -2   -5
;; c-label-indent    -4   -2   -5

(defvar c-body-indent 4
  "Indentation of code with respect to its containing block.")
(defvar c-brace-indent -4
  "Extra indentation of braces relative to the body of the code they
contain.")
(defvar c-case-indent -4
  "Extra indentation for case statements.")
(defvar c-label-indent -4
  "Extra indentation for labels.")

(defvar c-mode-keymap (make-keylist))
(bind-keys c-mode-keymap
  "{" 'c-open-brace
  "}" 'c-close-brace
  ":" 'c-colon
  "TAB" 'indent-line)

(defvar c-mode-ctrl-c-keymap (make-keylist))
(bind-keys c-mode-ctrl-c-keymap
  "Ctrl-\\" 'c-backslash-area)

;;;###autoload
(defun c-mode ()
  "C Mode:\n
Simple mode for editing C source code. Its main feature is to be able to
indent lines to their (probably) correct depth.\n
Special commands are,\n
  `{', `}', `:'			Insert the character then indent the line
  `TAB'				Indent the current line
  `Ctrl-c Ctrl-\\'		Aligns backslash characters at the end
				of each line in the current block.
  `ESC Ctrl-b'			Move backwards one expression.
  `ESC Ctrl-f'			Move forward one expression."
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "C"
	major-mode 'c-mode
	major-mode-kill 'c-mode-kill
	mode-comment-fun 'c-insert-comment
	mode-indent-line 'c-indent-line
	mode-forward-exp 'c-forward-exp
	mode-backward-exp 'c-backward-exp
	ctrl-c-keymap c-mode-ctrl-c-keymap
	keymap-path (cons 'c-mode-keymap keymap-path))
  (call-hook 'c-mode-hook))

(defun c-mode-kill ()
  (setq mode-name nil
	major-mode nil
	major-mode-kill nil
	mode-comment-fun nil
	mode-indent-line nil
	mode-forward-exp nil
	mode-backward-exp nil
	ctrl-c-keymap nil
	keymap-path (delq 'c-mode-keymap keymap-path)))

(defun c-open-brace ()
  (interactive)
  (insert "{")
  (indent-line))

(defun c-close-brace ()
  (interactive)
  (insert "}")
  (indent-line))

(defun c-colon ()
  (interactive)
  (insert ":")
  (indent-line))


;; Indentation

(defun c-indent-line (&optional pos)
  "Indent the line at POS (or the cursor) assuming that it's C source code."
  (set-indent-pos (c-indent-pos pos)))

;; Attempt to find the previous statement
(defun c-backward-stmt (pos)
  (let
      (stmt-pos back-1-pos)
    (condition-case nil
	(while (setq pos (c-backward-exp 1 pos t))
	  (cond
	   ((null back-1-pos)
	    (setq back-1-pos pos))
	   ((/= (pos-line back-1-pos) (pos-line pos))
	    ;; Gone past the start of this line, break the loop
	    (error "Ignored")))
	  (setq stmt-pos pos))
      (error))
    stmt-pos))

;; POS should point to an `else' keyword, the position of it's matching `if'
;; will be returned.
(defun c-balance-ifs (pos &optional depth)
  (unless depth
    (setq depth 1))
  (while (and (/= depth 0) (setq pos (c-backward-stmt pos)))
    (cond
     ((and (looking-at "else[\t ]*" pos)
	   (not (looking-at "[\t ]*if[\t ]*\\(" (match-end))))
      (setq depth (1+ depth)))
     ((looking-at "if" pos)
      (setq depth (1- depth)))))
  (when (zerop depth)
    pos))

;; Work out where to indent LINE-POS to.
(defun c-indent-pos (&optional line-pos)
  (setq line-pos (start-of-line line-pos))
  ;; Check for cpp op
  (if (looking-at "^[\t ]*#" line-pos)
      ;; Always indent preprocessor lines to the leftmost column
      (pos 0 (pos-line line-pos))
    (let
	((pos line-pos)
	 (exp-pos (c-backward-stmt line-pos))
	 exp-ind)

      ;; Find the beginning of the expression to indent relative to
      (unless exp-pos
	;; Start of the containing expression
	(when (re-search-backward "[\{\(]" pos)
	  (setq exp-pos (match-start))))
      (setq exp-ind (char-to-glyph-pos exp-pos))
      (unless (equal (indent-pos exp-pos) exp-ind)
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

       ((looking-at
	 "(if|for|while|switch)[\t ]*\\(.*$|(else|do)([^a-zA-Z0-9_]|$)"
	 exp-pos)
	;; Something that causes the next statement to be
	;; indented one level
	(setq exp-ind (right-char c-body-indent exp-ind)))

       ((looking-at ".*\;" exp-pos)
	;; A full expression, indent to the level of the first
	;; line in the expression
	(let
	    ((prev (c-backward-stmt exp-pos)))
	  ;; *Need to loop here searching back to the correct level*
	  (when (and prev (/= (pos-col prev) (pos-col exp-pos))
		     (not (looking-at "case .*:|default[\t ]*:|.*;" prev)))
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
	(unless (left-char c-label-indent exp-ind)
	  (setq exp-ind (pos 0 (pos-line exp-pos))))))

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


;; Movement over C expressions

(defun c-forward-exp (&optional number pos)
  (unless number
    (setq number 1))
  (while (> number 0)
    ;; first, skip empty lines & comments
    (while (looking-at "[\t\f ]*$|[\t\f ]*/\\*.*$" pos)
      (if (looking-at "[\t\f ]*/\\*" pos)
	  (progn
	    (unless (re-search-forward "\\*/" pos)
	      (error "Comment doesn't end!"))
	    (setq pos (match-end)))
	(setq pos (forward-line 1 (start-of-line pos)))
	(when (> pos (end-of-buffer))
	  (error "End of buffer"))))
    ;; Check for a cpp line
    (if (looking-at "^[\t ]*#" (start-of-line pos))
	(setq pos (end-of-line pos))
      ;; now any other whitespace
      (when (looking-at "[\t\f ]+" pos)
	(setq pos (match-end)))
      ;; Skip weird stuff
      (while (looking-at "[!*~&<>/+%?:^-]+" pos)
	(setq pos (match-end))
	(when (equal pos (end-of-line pos))
	  (setq pos (forward-char 1 pos))))
      (let
	  ((c (get-char pos)))
	(cond
	 ((member c '(?\" ?\'))
	  ;; move over string/character
	  (if (setq pos (char-search-forward c (forward-char 1 pos)))
	      (while (= (get-char (forward-char -1 pos)) ?\\ )
		(unless (setq pos (char-search-forward c (forward-char 1 pos)))
		  (error "String doesn't end!")))
	    (error "String doesn't end!"))
	  (setq pos (forward-char 1 pos)))
	 ((member c '(?\( ?\[ ?\{))
	  ;; move over brackets
	  (unless (setq pos (find-matching-bracket pos))
	    (error "Expression doesn't end!"))
	  (setq pos (forward-char 1 pos)))
	 ((member c '(?, ?\; ?:))
	  (setq pos (forward-char 1 pos)
		number (1+ number)))
	 ((member c '(?\) ?\] ?\}))
	  (error "End of containing expression"))
	 (t
	  ;; a symbol?
	  (if (looking-at "[a-zA-Z0-9_]+" pos)
	      (setq pos (match-end))
	    (unless (setq pos (re-search-forward "[][a-zA-Z0-9_ \t\f(){}'\"]"
						pos))
	      (error "Can't classify expression"))
	    (setq number (1+ number))))))
      (setq number (1- number))))
  pos)
  
(defun c-backward-exp (&optional number orig-pos no-blocks)
  (unless number
    (setq number 1))
  (unless orig-pos 
    (setq orig-pos (cursor-pos)))
  (let
      ((pos orig-pos)
       tmp)
    (while (> number 0)
      ;; skip preceding white space
      (when (or (equal pos (start-of-buffer))
		(not (setq pos (re-search-backward "[^\t\f\n ]"
						 (forward-char -1 pos)))))
	(error "No expression!"))
      (setq tmp (forward-char -1 pos))
      (while (looking-at "\\*/" tmp)
	;; comment to skip
	(unless (setq tmp (re-search-backward "/\\*" tmp))
	  (error "Comment doesn't start!"))
	(when (or (equal tmp (start-of-buffer))
		  (not (setq tmp (re-search-backward "[^\t\f\n ]"
						   (forward-char -1 tmp)))))
	  (error "Beginning of buffer"))
	(setq pos tmp))
      ;; Check for a cpp line
      (if (looking-at "^[\t ]*#" (start-of-line pos))
	  (setq pos (start-of-line pos))
	(let
	    ((c (get-char pos)))
	  (cond
	   ((member c '(?\) ?\] ?\}))
	    (when (or (/= c ?\}) (not no-blocks))
	      (unless (setq pos (find-matching-bracket pos))
		(error "Brackets don't match"))))
	   ((member c '(?\" ?\'))
	    (if (setq pos (char-search-backward c (forward-char -1 pos)))
		(while (= (get-char (forward-char -1 pos)) ?\\ )
		  (unless (setq pos (char-search-backward c (forward-char -1 pos)))
		    (error "String doesn't start!")))
	      (error "String doesn't start!")))
	   ((member c '(?\; ?: ?,))
	    ;; loop again
	    (setq number (1+ number)))
	   ((member c '(?\( ?\[ ?\{))
	    (error "Start of containing expression"))
	   (t
	    ;; a symbol?
	    (if (looking-at "[a-zA-Z0-9_]" pos)
		(unless (setq pos (re-search-backward "(^#[\t ]*|)[a-zA-Z0-9_]+"
						    pos))
		  (error "Can't classify expression"))
	      ;; Assume that it's some extraneous piece of punctuation..
;	      (unless (setq pos (re-search-backward "[][a-zA-Z0-9_ \t\f(){}'\"]"
;						  pos))
;		(error "Can't classify expression"))
	      (setq number (1+ number)))))
	  (when (member (get-char (forward-char -1 pos)) '(?! ?* ?- ?~))
	    ;; unary operator, skip over it
	    (setq pos (forward-char -1 pos))))
	(setq number (1- number))))
    pos))


;; Miscellaneous functions

;;;###autoload
(defun c-backslash-area (start end)
  "Insert (or align) backslash characters at the end of each line in between
START and END except for the last line."
  (interactive "-m\nM")
  (let
      ((max-width 0)
       (pos (start-of-line start))
       tmp)
    (while (<= pos end)
      (setq tmp (char-to-glyph-pos (if (looking-at ".*([\t ]*\\\\ *)$" pos)
				       (match-start 1)
				     (end-of-line pos))))
      (when (> (pos-col tmp) max-width)
	(setq max-width (pos-col tmp)))
      (setq pos (forward-line 1 pos)))
    (setq max-width (1+ max-width))
    (unless (= (% max-width tab-size) 0)
      (setq max-width (* (1+ (/ max-width tab-size)) tab-size)))
    (setq pos (pos max-width (pos-line start)))
    (while (< pos end)
      (when (looking-at ".*([\t ]*\\\\ *)$" (start-of-line pos))
	(delete-area (match-start 1) (match-end 1)))
      (goto (end-of-line pos))
      (indent-to max-width)
      (insert "\\")
      (setq pos (forward-line 1 pos)))
    (goto end)))

;;;###autoload
(defun c-insert-comment ()
  (interactive)
  (find-comment-pos)
  (insert "/*  */")
  (goto (forward-char -3)))
