;;;; rectangle.jl -- Rectangular region manipulation
;;;  Copyright (C) 1997 John Harper <john@dcs.warwick.ac.uk>
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

;;;###autoload
(defun insert-rectangle (text &optional p)
  "Insert TEXT rectangularly at position POS in the current buffer."
  (unless p
    (setq p (cursor-pos)))
  (let
      ((index 0)
       (column (pos-col (char-to-glyph-pos p)))
       (row (pos-line p))
       sub-string has-nl)
    (while (and (< index (length text))
		(string-match "[^\n]*" text index))
      (setq sub-string (substring text (match-start) (match-end))
	    index (if (equal (aref text (match-end)) ?\n)
		      (1+ (match-end))
		    (match-end)))
      (insert sub-string p)
      (setq row (1+ row))
      (when (> row (pos-line (end-of-buffer)))
	(insert "\n" (end-of-buffer)))
      (setq p (glyph-to-char-pos (pos column row))))))

;;;###autoload
(defun copy-rectangle (start end)
  "Return a string containing the text in the rectangle defined by START
and END (the characters at opposite corners)."
  (when (> start end)
    ;; Swap start and end
    (setq start (prog1 end (setq end start))))
  (let
      ((start-col (pos-col (char-to-glyph-pos start)))
       (end-col (pos-col (char-to-glyph-pos end)))
       (row (pos-line end))
       strings)
    (when (> start-col end-col)
      ;; Swap start-col and end-col
      (setq start-col (prog1 end-col (setq end-col start-col))))
    (while (>= row (pos-line start))
      (setq strings (cons (copy-area (glyph-to-char-pos (pos start-col row))
				     (glyph-to-char-pos (pos end-col row)))
			  (cons ?\n strings))
	    row (1- row)))
    (apply concat strings)))

;;;###autoload
(defun delete-rectangle (start end)
  "Delete the rectangle of text defined by START and END (the characters
at opposite corners)."
  (when (> start end)
    ;; Swap start and end
    (setq start (prog1 end (setq end start))))
  (let
      ((start-col (pos-col (char-to-glyph-pos start)))
       (end-col (pos-col (char-to-glyph-pos end)))
       (row (pos-line end))
       strings)
    (when (> start-col end-col)
      ;; Swap start-col and end-col
      (setq start-col (prog1 end-col (setq end-col start-col))))
    (while (>= row (pos-line start))
      (delete-area (glyph-to-char-pos (pos start-col row))
		   (glyph-to-char-pos (pos end-col row)))
      (setq row (1- row)))))

;;;###autoload
(defun cut-rectangle (start end)
  "Delete the rectangle START, END; returning its contents as a string."
  (prog1
      (copy-rectangle start end)
    (delete-rectangle start end)))
