;;;; fill-mode.jl -- minor mode and functions for filling text
;;;  Copyright (C) 1994 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'fill-mode)

(defvar fill-column 71
  "Position at which the text filling commands break lines.")

(defvar fill-mode-p nil)
(make-variable-buffer-local 'fill-mode-p)

;;;###autoload
(defun fill-mode ()
  "Minor mode for automatically filling lines, i.e. word-wrapping. This makes
the SPC key checks if the cursor is past the fill-column. If so, the next
line is started."
  (interactive)
  (if fill-mode-p
      (progn
	(setq fill-mode-p nil)
	(remove-minor-mode 'fill-mode "Fill")
	(unbind-keys minor-mode-keymap "SPC" "RET"))
    (add-minor-mode 'fill-mode "Fill")
    (setq fill-mode-p t)
    (bind-keys minor-mode-keymap
      "SPC" 'fill-mode-spc
      "RET" 'fill-mode-ret)))

;;;###autoload
(defun fill-mode-on ()
  (interactive)
  (unless fill-mode-p
    (fill-mode))
  nil)

;; Checks whether the current line needs to be broken
(defun fill-check-line ()
  (when (> (pos-col (char-to-glyph-pos (cursor-pos))) fill-column)
    (let
	((pos (glyph-to-char-pos (pos fill-column
				      (pos-line (cursor-pos))))))
      (setq pos (or (word-start pos) (forward-word -1 pos)))
      (insert "\n" pos)
      (let
	  ((end (left-char 1 (copy-pos pos))))
	(when (equal (get-char end) ?\ )
	  (delete-area end pos)))
      ;; Hack to auto-indent new line in indented-text-mode
      (when (eq major-mode 'indented-text-mode)
	(set-indent-pos (next-line 1 (indent-pos (prev-line))))))))

(defun fill-mode-spc ()
  (interactive)
  (fill-check-line)
  (insert " "))

(defun fill-mode-ret ()
  (interactive)
  (fill-check-line)
  (insert "\n"))

;;;###autoload
(defun set-fill-column (&optional column)
  "Sets the column number for filling to (the variable `fill-column') to
COLUMN or the current column."
  (interactive)
  (setq fill-column (if (numberp column)
			column 
		      (pos-col (char-to-glyph-pos (cursor-pos)))))
  (format t "Fill column set to %d." (1+ fill-column)))

;;;###autoload
(defun fill-paragraph ()
  "Fills the current paragraph so that no lines are wider than fill-column."
  (interactive "p")
  (let
      ((start (backward-paragraph (forward-paragraph)))
       word-start word-end)
    (while (looking-at paragraph-regexp start)
      (setq start (next-line 1 start)))
    ;; TODO: the next regexp hardcodes blank-line as paragraph delimiter
    (while (and (< start (buffer-end))
		(not (looking-at "[\t ]*\n[\t ]*\n" start)))
      (setq word-end (forward-word 1 start)
	    word-start (forward-word -1 word-end))
      (when (looking-at "[][<>\"%^`',.-;:!?(){}]+" word-end)
	(setq word-end (match-end)))
      (if (/= (pos-line start) (pos-line word-start))
	  ;; The next word is on the following line
	  (if (<= (+ (pos-col (char-to-glyph-pos start))
		     (- (pos-col word-end) (pos-col word-start)))
		  fill-column)
	      ;; We can fit the current word onto the end of the previous line
	      (if (looking-at "[\t ]*\n[\t ]*" start)
		  (progn
		    (delete-area (match-start) (match-end))
		    (setq start (insert " " (match-start))))
		(error "Shouldn't happen"))
	    ;; Can't fit it onto the current line. Leave it where
	    ;; it is and advance the `current' pointer
	    (setq start word-end))
	;; The next word is on the same line.
	(if (> (pos-col (char-to-glyph-pos word-end)) fill-column)
	    ;; It doesn't fit, move it to the start of the next line
	    (progn
	      (if (and (find-prev-regexp "[\t ]+" word-start)
		       (equal (match-end) word-start))
		  (progn
		    (delete-area (match-start) (match-end))
		    (setq start (insert "\n" (match-start))))
		(setq start (insert "\n" word-start)))
	      ;; Hack to auto-indent new line in indented-text-mode
	      (when (eq major-mode 'indented-text-mode)
		(set-indent-pos (next-line 1 (indent-pos
					      (prev-line
					       1 (copy-pos start)))))))
	  ;; Everything's ok, advance START
	  (setq start word-end))))))
