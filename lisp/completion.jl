;;;; completion.jl -- Code for displaying completions
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

(provide 'completion)

(defvar completion-sorted-lists t
  "When t, displayed completion-lists are sorted into alphabetical order.")

;; t when the view displaying this buffer was created specially
(defvar completion-deletable-view nil)
(make-variable-buffer-local 'completion-deletable-view)

(defun completion-find-view ()
  (catch 'return
    (mapc #'(lambda (v)
	      (and (string-match "^\\*completions\\*"
				 (buffer-name (current-buffer v)))
		   (throw 'return v)))
	  (window-view-list))
    nil))

;; Return a view visible in the current window that is used to display
;; completion lists (in its current buffer)
(defun completion-setup-view ()
  (or (completion-find-view)
      ;; No existing view has a *completions* buffer. Make one
      (let
	  ((new-view (and (= (window-view-count) 2) (split-view))))
	(with-view (or new-view (other-view))
	  (goto-buffer (make-buffer "*completions*"))
	  (setq completion-deletable-view new-view)
	  (current-view)))))

;; Try to remove any completion buffer, and if a view was created specially
;; the view itself
(defun completion-remove-view ()
  (let
      ((view (completion-find-view)))
    (when view
      (when (prog1
		(with-view view completion-deletable-view)
	      (kill-buffer (current-buffer view)))
	;; Ok to remove this view
	(delete-view view)))))

;; Display a list of completions in a *completions* buffer in the
;; current window
(defun completion-list (completions)
  (when completion-sorted-lists
    (setq completions (sort (copy-sequence completions))))
  (let*
      ((max-width 0)
       column-width columns
       (view (completion-setup-view))
       (view-width (car (view-dimensions view))))
    (mapc #'(lambda (c)
	      (and (> (length c) max-width)
		   (setq max-width (length c)))) completions)
    (if (= max-width view-width)
	(setq columns 1
	      column-width view-width)
      (setq columns (max (/ view-width (+ max-width 8)) 1)
	    column-width (/ view-width columns)))
    (with-view view
      (let
	  ((column 0))
	(clear-buffer)
	(insert "Completions:\n\n")
	(while completions
	  (indent-to (* column column-width))
	  (insert (car completions))
	  (setq column (% (1+ column) columns))
	  (when (zerop column)
	    (insert "\n"))
	  (setq completions (cdr completions)))
	(goto (start-of-buffer))
	(when completion-deletable-view
	  (shrink-view-if-larger-than-buffer))))))
