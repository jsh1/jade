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
  (setq line-pos (start-of-line line-pos))
  (let*
      ((ind-pos (c-indent-pos-empty line-pos)))
    (when (not (empty-line-p line-pos))
      (cond
       ((looking-at "^[\t ]*({|}|case .*:|default *:)" line-pos)
	(setq ind-pos (forward-tab -1 ind-pos c-mode-tab)))
       ((looking-at "^[\t ]*([a-zA-Z0-9_]*:|#)" line-pos)
	(setq ind-pos (pos 0 (pos-line ind-pos))))))
    ind-pos))

(defun c-indent-pos-empty (&optional line-pos)
  "Returns the position for the first non-space in the line. Bases its guess
upon the assumption that the line is empty.
All positions depend on the indentation of the previous line(s)."
  (setq line-pos (start-of-line line-pos))
  (let*
      ((p-line-pos (forward-line -1 line-pos)))
    (while (or (empty-line-p p-line-pos)
	       (looking-at "^([a-zA-Z0-9_]+:|#)" p-line-pos))
      (unless (setq p-line-pos (forward-line -1 p-line-pos))
	(return)))
    (let*
	((ind-pos (pos (pos-col (indent-pos p-line-pos)) (pos-line line-pos))))
      (cond
       ((looking-at ".*({|case .*:|default[\t ]*:|do($| )|else|(if|for|while|switch)[\t ]*\\(.*\\))" p-line-pos)
	(setq ind-pos (forward-tab 1 ind-pos c-mode-tab)))
       ((looking-at ".*;" p-line-pos)
	(setq p-line-pos (forward-line -1 p-line-pos))
	(while (or (empty-line-p p-line-pos)
		   (looking-at "^([a-zA-Z0-9_]+:|#)" p-line-pos))
	  (unless (setq p-line-pos (forward-line -1 p-line-pos))
	    (return)))
	(when (and (looking-at
		    ".*(do($| )|else|(if|for|while|switch)[\t ]*\\(.*\\))"
		    p-line-pos)
		   (not (looking-at ".* {[\t ]*(/\\*.*\\*/|)[\t ]*$"
				    p-line-pos)))
	  (setq ind-pos (forward-tab -1 ind-pos c-mode-tab))))
       ((looking-at "^[\t ]*/\\*" p-line-pos)
	(unless (looking-at ".*\\*/" p-line-pos)
	  (setq ind-pos (forward-char 3 ind-pos))))
       ((looking-at "^[\t ]*\\*/ *$" p-line-pos)
	(setq ind-pos (forward-char -1 ind-pos)))
       ((looking-at ".*\\*/" p-line-pos)
	(setq ind-pos (forward-char -3 ind-pos))))
      ind-pos)))

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


;; Experimental expression stuff

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
