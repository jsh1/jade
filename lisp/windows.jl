;;;; windows.jl -- Window and view handling
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


;; Some macros

(defmacro defface (name #!optional doc #!rest forms)
  "Create a face called NAME, the Lisp FORMS are evaluated to initialise it.
If the symbol NAME is already bound, only the documentation property is set."
  `(or (prog1 (variable-bound? ',name)
	 (defvar ,name (make-face ,(symbol-name name)) ,doc))
       (progn ,@forms)))

(defmacro with-view (view #!rest forms)
  "Set the editor's current view to VIEW (and the current window to that
containing VIEW) evaluate FORMS..., then reinstall the originals
afterwards, returning the value of (progn FORMS...)"
  (list 'call-with-object view (list* 'lambda nil forms)))

(defmacro with-window (win #!rest forms)
  "Set the editor's current window to WINDOW and evaluate FORMS, then
reinstall the original window as the current one."
  (list 'call-with-object win (list* 'lambda nil forms)))


;; Some more standard faces

(defface underline-face "Face used for underlined text."
  (set-face-attribute underline-face 'underline t))

(defface bold-face "Face used for bold text."
  (set-face-attribute bold-face 'bold t))

(defface italic-face "Face used for italicized text."
  (set-face-attribute italic-face 'italic t))

(defface active-face "Face used for ``active'' text, i.e. clickable"
  (set-face-attribute active-face 'background "#90ee90"))	;palegreen2



(defun in-new-window (command)
  (goto-new-window)
  (call-command command))

(defun goto-new-window ()
  (interactive)
  (set-current-window (make-window) t))

(defun toggle-iconic ()
  "Toggle the current window between iconified and normal states."
  (interactive)
  (if (window-asleep-p)
      (unsleep-window)
    (sleep-window)))


;; View handling

(defun delete-other-views (#!optional view)
  "Close all views in the current window except for VIEW, or the current one."
  (interactive)
  (unless view
    (set! view (current-view)))
  (let
      ((doomed (window-first-view))
       next)
    (while (> (window-view-count) 2)
      (set! next (next-view doomed))
      (unless (or (eq? doomed view) (minibuffer-view-p doomed))
	(delete-view doomed))
      (set! doomed next))))

(defun in-other-view (command)
  "Switches to the `other' view in this window then calls the command
COMMAND in it."
  (goto-other-view)
  (call-command command))

(defun goto-other-view ()
  "Switch to a different view in the current window."
  (interactive)
  (set-current-view (other-view)))

(defun other-view (#!optional lines)
  "Return a different view in the current window. LINES defines the lower
bound on the number of lines in the new view; alternately, if it is the
symbol nil the other view will be roughly half the size of the current view,
or if it is the symbol t the size of the other view won't be changed."
  (if (= 2 (window-view-count))
      (split-view nil lines)
    (let
	((view (next-view))
	 total desired)
      (when (minibuffer-view-p view)
	(set! view (previous-view)))
      (unless (eq? lines t)
	(set! total (+ (cdr (view-dimensions))
		       (cdr (view-dimensions view))))
	(set! desired (or lines (quotient total 2)))
	;; Only enlarge if the other-view is currently _smaller_
	;; than it's desired size
	(unless (> (cdr (view-dimensions view)) desired)
	  (enlarge-view (- total desired (cdr (view-dimensions))))))
      view)))

(defun goto-next-view (#!optional all-windows-p)
  "Cycles through the available views. If ALL-WINDOWS-P is t views in
windows other than the current window are used when needed."
  (interactive "P")
  (let
      ((view (next-view nil all-windows-p)))
    (when (and (minibuffer-view-p view) (not (minibuffer-active-p view)))
      (set! view (next-view view all-windows-p)))
    (set-current-view view all-windows-p)))

(defun scroll-next-view (#!optional count)
  "Scroll the view following the current view in this window by COUNT
screenfuls. When called interactively COUNT is taken from the prefix arg.
Negative arguments scroll backwards."
  (interactive "p")
  (with-view (other-view t)
    (next-screen count)))

(defun enlarge-view (#!optional count)
  "Enlarge the current view by one line. If COUNT is specified enlarge
by COUNT lines. When called interactively, COUNT is taken from the prefix
argument."
  (interactive "p")
  (unless count (set! count 1))
  (let*
      ((views (window-view-list))
       (view-count (window-view-count))
       (view-index (- view-count (1- (list-length (memq (current-view) views)))))
       view)
    (cond
     ((> view-index (- view-count 2))
      ;; Last view in window or minibuffer, expand the previous view negatively
      (set! view (previous-view))
      (set! count (- count)))
     ((= view-count 2)
      (error "Can't resize a single view"))
     (t
      (set! view (current-view))))
    (set-view-dimensions view nil (+ (cdr (view-dimensions view)) count))))

(defun shrink-view (#!optional count)
  "Shrink the current view by one line. If COUNT is specified shrink by
COUNT lines. When called interactively, COUNT is taken from the prefix
argument."
  (interactive "p")
  (enlarge-view (- (or count 1))))

(defun shrink-view-if-larger-than-buffer ()
  "If the current view is larger than the buffer that it is displaying, shrink
it so that the buffer just fits the view."
  (interactive)
  (if (<= (window-view-count) 2)
      (error "Can't resize a single view")
    (when (equal? (view-origin) (start-of-buffer))
      (let
	  ((view-rows (cdr (view-dimensions)))
	   (end (char-to-display-pos (end-of-buffer))))
	(when (and end (>= view-rows (pos-line end)))
	  (goto (start-of-buffer))
	  ;; add one since positions count from zero
	  (enlarge-view (- (pos-line end) view-rows -1)))))))
