;;;; fill.jl -- functions and minor mode for filling text
;;;  Copyright (C) 1994, 1998 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'fill)


;; Configuration

(defvar fill-column 72
  "Maximum allowed length of lines when filling.")
(make-variable-buffer-local 'fill-column)

(defvar fill-break-re "[\n\t\f ]"
  "Regexp matching where it's permittable to break a line.")
(make-variable-buffer-local 'fill-break-re)

(defvar fill-prefix nil
  "When non-nil, a string that is removed from the start of each line before
filling, and added to the start of each line after filling.\n
If this is set to the name of a function (i.e. a symbol or lambda list),
the function will be called with two arguments, the first a symbol
defining the operation to be performed, the second the position at which
to perform it. The three operations are:\n
  insert	The function should insert the correct fill prefix
		 for the line beginning at the specified position.
  width		The function should return the width in glyhps of the
		 fill prefix that would be inserted at the position.
  delete	The function should delete the fill prefix from the
		 specified position.")
(make-variable-buffer-local 'fill-prefix)

(defvar fill-prefix-width nil
  "When fill-prefix is a string, this is the number of _glyphs_ required
to display it, assuming that it's inserted at column zero.")
(make-variable-buffer-local 'fill-prefix-width)


;; Low level functions

;; Returns t if we may need to deal with a fill prefix
(defmacro fill-has-prefix-p ()
  '(or (stringp fill-prefix) (functionp fill-prefix)))

;; Insert the fill-prefix before the cursor
(defun fill-insert-prefix (p)
  (cond
   ((stringp fill-prefix)
    (insert fill-prefix p))
   ((functionp fill-prefix)
    (fill-prefix 'insert p))))

;; Delete the fill-prefix from POS
(defun fill-delete-prefix (p)
  (cond
   ((stringp fill-prefix)
    (when (buffer-compare-string fill-prefix p)
      (delete-area (match-start) (match-end))))
   ((functionp fill-prefix)
    (fill-prefix 'delete p))))

;; Return the width of the fill-prefix that would be inserted at POS
(defun fill-get-prefix-width (p)
  (cond
   ((stringp fill-prefix)
    fill-prefix-width)
   ((functionp fill-prefix)
    (fill-prefix 'width p))))


;; Filling functions

;;;###autoload
(defun set-fill-column (#!optional column)
  "Sets the column number for filling to (the variable `fill-column') to
COLUMN or the current column."
  (interactive)
  (setq fill-column (if (numberp column)
			column 
		      (1+ (pos-col (char-to-glyph-pos (cursor-pos))))))
  (format t "Fill column set to %d." fill-column))

;;;###autoload
(defun set-fill-prefix (p)
  "Sets the fill prefix to the string between the start of the current line
and POS. When called interactively, POS is bound to the cursor position."
  (interactive "d")
  (if (functionp fill-prefix)
      (error "Fill prefix is defined by a function in this buffer")
    (setq fill-prefix (copy-area (start-of-line p) p)
	  fill-prefix-width (pos-col (char-to-glyph-pos p)))
    (format t "Set fill prefix to %S" fill-prefix)))


;;;###autoload
(defun fill-paragraph ()
  "Fills the current paragraph so that no lines are wider than fill-column."
  (interactive)
  (let
      ((edges (paragraph-edges 1)))
    (fill-area (car edges) (cdr edges))))

;; This macro is only used by fill-area
(defmacro fill-area-next-line ()
  '(progn
     (when has-prefix
       (fill-insert-prefix line-start))
     (setq line-start (forward-line 1 line-start)
	   goal (1- (if has-prefix
			(- fill-column
			   (fill-get-prefix-width line-start))
		      fill-column)))))
;;;###autoload
(defun fill-area (start end)
  "Fills from START to END."
  (interactive "-m\nM")
  (let
      ((has-prefix (fill-has-prefix-p))
       (line-start (start-of-line start))
       (seen-non-blank nil)
       goal line-end g-line-end)

    ;; Make end into a mark so that it preserves its logical position
    (setq end (make-mark end))

    ;; Delete all existing fill prefixes and other leading white space,
    ;; except on the first non-blank line
    (while (< line-start (mark-pos end))
      (when has-prefix
	(fill-delete-prefix line-start))
      (cond
       (seen-non-blank
	(when (looking-at "^[\t ]+" line-start)
	  (delete-area (match-start) (match-end))))
       ((looking-at "^[\t ]*$" line-start)
	(when (> (match-end) (match-start))
	  (delete-area (match-start) (match-end))))
       (t
	(setq seen-non-blank t)))
      (setq line-start (forward-line 1 line-start)))

    ;; Find the column we're aiming at in the first line
    (setq line-start (start-of-line start)
	  goal (1- (if has-prefix
		       (- fill-column
			  (fill-get-prefix-width line-start))
		     fill-column)))

    ;; Loop over all lines in the area
    (while (< line-start (mark-pos end))
      ;; Find the end of the line in characters and glyphs
      (setq line-end (end-of-line line-start)
	    g-line-end (char-to-glyph-pos line-end))

      ;; Check the current length against the goal column
      (cond
       ((> (pos-col g-line-end) goal)
	;; The current line is too long
	(let
	    ((p (re-search-backward fill-break-re
				      (glyph-to-char-pos
				       (pos goal (pos-line line-start))))))
	  (when (and pos (/= (pos-col p) 0))
	    (when (= (get-char p) ?\ )
	      (delete-area p (forward-char 1 p)))
	    (insert "\n" p))
	  ;; Move on to the next line (reinserting the fill prefix)
	  (fill-area-next-line)))

       ((and (/= (pos-col g-line-end) 0)
	     (< (pos-col g-line-end) goal)
	     (< (pos-line line-start) (pos-line (mark-pos end))))
	;; The current line may be too short
	(let*
	    ((space (- goal (pos-col g-line-end) 1))
	     (move-start (forward-line 1 line-start))
	     (move-end (min (glyph-to-char-pos (pos space
						    (pos-line move-start)))
			    (end-of-line move-start))))
	  (if (>= move-end (mark-pos end))
	      (setq move-end (mark-pos end))
	    (setq move-end (re-search-backward fill-break-re move-end)))
	  (if (and move-end (> move-end move-start))
	      ;; We can move some words from the next line to
	      ;; fill some of the gap
	      (progn
		(when (and (< move-end (mark-pos end))
			   (= (get-char move-end) ?\ ))
		  (delete-area move-end (forward-char 1 move-end)))
		(insert (cut-area move-start move-end) (insert " " line-end))
		(when (empty-line-p move-start)
		  ;; We've created a blank line, delete it
		  (if (>= (mark-pos end) (end-of-buffer))
		      ;; Can't delete to the next line
		      (delete-area (forward-char -1 move-start) move-start)
		    (delete-area move-start (forward-line 1 move-start)))))
	    ;; The current line is as good as it gets, move
	    ;; on to the next line
	    (fill-area-next-line))))

       (t
	;; The current line is exactly the right length
	(fill-area-next-line))))))


;; Filling minor mode

(defvar fill-mode-active nil
  "Non-nil when fill-mode is enabled.")
(make-variable-buffer-local 'fill-mode-active)

(unless (assq 'fill-mode-active minor-mode-alist)
  (setq minor-mode-alist (cons '(fill-mode-active " Fill")
			       minor-mode-alist))
  (setq minor-mode-keymap-alist (cons '(fill-mode-active . fill-mode-keymap)
				      minor-mode-keymap-alist)))

(defvar fill-mode-keymap
  (bind-keys (make-sparse-keymap)
    "SPC" 'fill-mode-spc
    "RET" 'fill-mode-ret)
  "Keymap for fill-mode.")

;;;###autoload
(defun fill-mode ()
  "Minor mode for automatically filling lines, i.e. word-wrapping. This makes
the SPC and RET keys check if the cursor is past the fill-column. If so,
the next line is started."
  (interactive)
  (if fill-mode-active
      (setq fill-mode-active nil)
    (setq fill-mode-active t)))

;;;###autoload
(defun fill-mode-on ()
  (interactive)
  (unless fill-mode-active
    (fill-mode))
  nil)

;; These two functions are bound to SPC and RET in fill-mode-keymap
(defun fill-mode-spc ()
  (interactive)
  (fill-check-line)
  (insert " "))
(defun fill-mode-ret ()
  (interactive)
  (fill-check-line)
  (insert "\n"))

;; Checks whether the current line needs to be broken to cope with an
;; insertion before the cursor, if so breaks it and inserts the fill
;; prefix
(defun fill-check-line ()
  (when (>= (pos-col (char-to-glyph-pos (cursor-pos))) fill-column)
    (let
	((p (re-search-backward fill-break-re
				  (glyph-to-char-pos
				   (pos (1- fill-column)
					(pos-line (cursor-pos)))))))
      (when (and p (/= (pos-col p) 0))
	(when (= (get-char p) ?\ )
	  (delete-area p (forward-char 1 p)))
	(insert "\n" p)
	(if mode-indent-line
	    (mode-indent-line)
	  (fill-insert-prefix (start-of-line)))))))


;; Centering

;;;###autoload
(defun center-line (#!optional p)
  "Centre the line at POS."
  (interactive)
  (let*
      ((spos (indent-pos p))
       (epos (char-to-glyph-pos (re-search-forward " *$" (start-of-line p))))
       (len (- (pos-col epos) (pos-col spos))))
    (cond
      ((<= len 0))
      ((> len fill-column)
	(set-indent-pos (start-of-line p)))
      (t
	(setq spos (pos (quotient (- fill-column len) 2) (pos-line spos)))
	(set-indent-pos spos)))))

;;;###autoload
(defun center-paragraph (#!optional p)
  "Centre the paragraph surrounding POS."
  (interactive)
  (let*
      ((epos (forward-paragraph 1 p))
       (spos (forward-paragraph -1 epos)))
    (while (< spos epos)
      (center-line spos)
      (forward-line 1 spos))))


;; Miscellaneous

;;;###autoload
(defun unfill-paragraph (count)
  "Change all newline characters with spaces in the COUNT paragraphs
surrounding the cursor. When called interactively COUNT is taken from the
prefix argument."
  (interactive "p")
  (let*
      ((start (paragraph-edges count))
       (end (make-mark (forward-char -1 (cdr start)))))
    (setq start (car start))
    (while (and (setq start (char-search-forward ?\n start))
		(< start (mark-pos end)))
      (delete-area start (forward-char 1 start))
      (insert " " start))))
