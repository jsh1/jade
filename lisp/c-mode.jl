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

(defvar c-mode-tab 4
  "Size of indentation for c-mode")

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
  (eval-hook 'c-mode-hook))

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

(defun c-indent-line (&optional pos)
  "Indent the line at POS (or the cursor) assuming that it's C source code."
  (set-indent-pos (c-indent-pos pos)))

(defun c-indent-pos (&optional line-pos)
  "*Attempts* to guess the correct indentation for this line. Returns the
position for the first non-space in the line."
  (setq line-pos (line-start line-pos))
  (let*
      ((ind-pos (c-indent-pos-empty line-pos)))
    (when (not (empty-line-p line-pos))
      (cond
       ((regexp-match-line "^[\t ]*({|}|case .*:|default *:)" line-pos)
	(prev-tab 1 ind-pos c-mode-tab))
       ((regexp-match-line "^[\t ]*([a-zA-Z0-9_]*:|#)" line-pos)
	(set-pos-col ind-pos 0))))
    ind-pos))

(defun c-indent-pos-empty (&optional line-pos)
  "Returns the position for the first non-space in the line. Bases its guess
upon the assumption that the line is empty.
All positions depend on the indentation of the previous line(s)."
  (setq line-pos (line-start line-pos))
  (let*
      ((p-line-pos (prev-line 1 (copy-pos line-pos))))
    (while (or (empty-line-p p-line-pos)
	       (regexp-match-line "^([a-zA-Z0-9_]+:|#)" p-line-pos))
      (unless (prev-line 1 p-line-pos)
	(return)))
    (let*
	((ind-pos (indent-pos p-line-pos)))
      (set-pos-line ind-pos (pos-line line-pos))
      (cond
       ((regexp-match-line "{|case .*:|default[\t ]*:|do($| )|else|(if|for|while|switch)[\t ]*\\(.*\\)" p-line-pos)
	(next-tab 1 ind-pos c-mode-tab))
       ((regexp-match-line ";" p-line-pos)
	(prev-line 1 p-line-pos)
	(while (or (empty-line-p p-line-pos)
		   (regexp-match-line "^([a-zA-Z0-9_]+:|#)" p-line-pos))
	  (unless (prev-line 1 p-line-pos)
	    (return)))
	(when (and (regexp-match-line
		    "do($| )|else|(if|for|while|switch)[\t ]*\\(.*\\)"
		    p-line-pos)
		   (not (regexp-match-line " {[\t ]*(/\\*.*\\*/|)[\t ]*$"
					   p-line-pos)))
	  (prev-tab 1 ind-pos c-mode-tab)))
       ((regexp-match-line "^[\t ]*/\\*" p-line-pos)
	(unless (regexp-match-line "\\*/" p-line-pos)
	  (right-char 3 ind-pos)))
       ((regexp-match-line "^[\t ]*\\*/ *$" p-line-pos)
	(left-char 1 ind-pos))
       ((regexp-match-line "\\*/" p-line-pos)
	(left-char 3 ind-pos)))
      ind-pos)))

;;;###autoload
(defun c-backslash-area (start end)
  "Insert (or align) backslash characters at the end of each line in between
START and END except for the last line."
  (interactive "-m\nM")
  (let
      ((max-width 0)
       (pos (copy-pos start))
       tmp)
    (while (<= pos end)
      (setq tmp (char-to-glyph-pos (if (regexp-match-line "[\t ]*\\\\ *$" pos)
				       (match-start)
				     (line-end pos))))
      (when (> (pos-col tmp) max-width)
	(setq max-width (pos-col tmp)))
      (setq pos (next-line 1 pos)))
    (setq max-width (1+ max-width))
    (unless (= (% max-width tab-size) 0)
      (setq max-width (* (1+ (/ max-width tab-size)) tab-size)))
    (set-pos-line pos (pos-line start))
    (set-pos-col pos max-width)
    (while (< pos end)
      (when (regexp-match-line "[\t ]*\\\\ *$" pos)
	(delete-area (match-start) (match-end)))
      (goto-char (line-end pos))
      (indent-to max-width)
      (insert "\\")
      (setq pos (next-line 1 pos)))
    (goto-char end)))

;;;###autoload
(defun c-insert-comment ()
  (interactive)
  (find-comment-pos)
  (insert "/*  */")
  (goto-left-char 3))


;; Experimental expression stuff

(defun c-forward-exp (&optional number pos)
  (unless number
    (setq number 1))
  (while (> number 0)
    ;; first, skip empty lines & comments
    (while (looking-at "[\t\f ]*$|[\t\f ]*/\\*.*$" pos)
      (if (looking-at "[\t\f ]*/\\*" pos)
	  (progn
	    (unless (find-next-regexp "\\*/" pos)
	      (error "Comment doesn't end!"))
	    (setq pos (match-end)))
	(setq pos (next-line 1 (line-start pos)))
	(when (> pos (buffer-end))
	  (error "End of buffer"))))
    ;; Check for a cpp line
    (if (regexp-match-line "^[\t ]*#" pos)
	(setq pos (line-end pos))
      ;; now any other whitespace
      (when (looking-at "[\t\f ]+" pos)
	(setq pos (match-end)))
      ;; Skip weird stuff
      (while (looking-at "[!*~&<>/+%?:^-]+" pos)
	(setq pos (match-end))
	(when (equal pos (line-end pos))
	  (setq pos (next-char 1 pos))))
      (let
	  ((c (get-char pos)))
	(cond
	 ((member c '(?\" ?\'))
	  ;; move over string/character
	  (if (setq pos (find-next-char c (next-char 1 pos)))
	      (while (= (get-char (prev-char 1 (copy-pos pos))) ?\\ )
		(unless (setq pos (find-next-char c (next-char 1 pos)))
		  (error "String doesn't end!")))
	    (error "String doesn't end!"))
	  (setq pos (next-char 1 pos)))
	 ((member c '(?\( ?\[ ?\{))
	  ;; move over brackets
	  (unless (setq pos (match-brackets pos))
	    (error "Expression doesn't end!"))
	  (setq pos (next-char 1 pos)))
	 ((member c '(?, ?\; ?:))
	  (setq pos (next-char 1 pos)
		number (1+ number)))
	 ((member c '(?\) ?\] ?\}))
	  (error "End of containing expression"))
	 (t
	  ;; a symbol?
	  (if (looking-at "[a-zA-Z0-9_]+" pos)
	      (setq pos (match-end))
	    (unless (setq pos (find-next-regexp "[][a-zA-Z0-9_ \t\f(){}'\"]"
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
      ((pos (copy-pos orig-pos))
       tmp)
    (while (> number 0)
      ;; skip preceding white space
      (when (or (equal pos (buffer-start))
		(not (setq pos (find-prev-regexp "[^\t\f\n ]" (prev-char 1 pos)))))
	(error "No expression!"))
      (setq tmp (prev-char 1 (copy-pos pos)))
      (while (looking-at "\\*/" tmp)
	;; comment to skip
	(unless (setq tmp (find-prev-regexp "/\\*" tmp))
	  (error "Comment doesn't start!"))
	(when (or (equal tmp (buffer-start))
		  (not (setq tmp (find-prev-regexp "[^\t\f\n ]" (prev-char 1 tmp)))))
	  (error "Beginning of buffer"))
	(setq pos tmp))
      ;; Check for a cpp line
      (if (regexp-match-line "^[\t ]*#" pos)
	  (setq pos (line-start pos))
	(let
	    ((c (get-char pos)))
	  (cond
	   ((member c '(?\) ?\] ?\}))
	    (when (or (/= c ?\}) (not no-blocks))
	      (unless (setq pos (match-brackets pos))
		(error "Brackets don't match"))))
	   ((member c '(?\" ?\'))
	    (if (setq pos (find-prev-char c (prev-char 1 pos)))
		(while (= (get-char (prev-char 1 (copy-pos pos))) ?\\ )
		  (unless (setq pos (find-prev-char c (prev-char 1 pos)))
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
		(unless (setq pos (find-prev-regexp "(^#[\t ]*|)[a-zA-Z0-9_]+"
						    pos))
		  (error "Can't classify expression"))
	      ;; Assume that it's some extraneous piece of punctuation..
;	      (unless (setq pos (find-prev-regexp "[][a-zA-Z0-9_ \t\f(){}'\"]"
;						  pos))
;		(error "Can't classify expression"))
	      (setq number (1+ number)))))
	  (when (member (get-char (prev-char 1 (copy-pos pos))) '(?! ?* ?- ?~))
	    ;; unary operator, skip over it
	    (setq pos (prev-char 1 pos))))
	(setq number (1- number))))
    pos))
