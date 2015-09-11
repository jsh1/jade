;;;; bookmarks.jl -- Named bookmarks
;;;  Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
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

(require 'summary)

(defvar bookmark-alist '()
  "List of (NAME . MARK) defining all named bookmarks.")

;;;###autoload
(defun set-bookmark (name)
  "Prompt for the NAME of a bookmark to set to the current position."
  (interactive (list (prompt-from-list (mapcar car bookmark-alist)
				       "Bookmark to set:" nil t)))
  (let
      ((mark (assoc name bookmark-alist)))
    (if mark
	(progn
	  (set-mark-pos (cdr mark) (cursor-pos))
	  (set-mark-file (cdr mark) (current-buffer)))
      (setq bookmark-alist (cons (cons name (make-mark)) bookmark-alist)))))

;;;###autoload
(defun goto-bookmark (name)
  "Prompt for the name of a previously set bookmark and display it."
  (interactive (list (prompt-from-list (mapcar car bookmark-alist)
				       "Bookmark to set:")))
  (let
      ((mark (assoc name bookmark-alist)))
    (goto-mark (cdr mark))))

;;;###autoload
(defun kill-bookmark (name)
  "Prompt for the name of a previously set bookmark and delete it."
  (interactive (list (prompt-from-list (mapcar car bookmark-alist)
				       "Bookmark to delete:")))
  (let
      ((mark (assoc name bookmark-alist)))
    (setq bookmark-alist (delq mark bookmark-alist))))

;;;###autoload
(defun kill-all-bookmarks ()
  "Delete all named bookmarks."
  (interactive)
  (setq bookmark-alist nil))

;;;###autoload
(defun list-bookmarks ()
  "List all named bookmarks in a buffer."
  (interactive)
  (goto-other-view)
  (goto-buffer (open-buffer "*bookmarks*"))
  (if (eq? major-mode 'summary-mode)
      (summary-update)
    (clear-buffer)
    (insert "Bookmark Summary:\n\n  Name")
    (indent-to 24)
    (insert "Location\n  ----")
    (indent-to 24)
    (insert "--------\n")
    (summary-mode "Bookmarks"
		  (list (cons 'select (lambda (x) (goto-bookmark (car x))))
			(cons 'delete (lambda (x) (kill-bookmark (car x))))
			(cons 'print
			      (lambda (x)
				(format (current-buffer) "%c %s "
					(if (memq 'delete (summary-get-pending-ops x)) #\D #\space)
					(car x))
				(indent-to 24)
				(format (current-buffer) "Line %d of %s"
					(pos-line (mark-pos (cdr x)))
					(mark-file (cdr x)))))
			(cons 'after-marking (lambda ()
						 (summary-next-item 1)))
			(cons 'list (lambda () bookmark-alist))))))
