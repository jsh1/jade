;;;; windows.jl -- Window handling
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

(defvar window-list (cons (current-window) nil)
  "List of all opened windows.")

(defvar window-closed-hook '(close-window)
  "Hook called when an input event saying that a window should be closed
is received.")

(defun open-window (&optional buffer x y w h)
  "Creates a new window display BUFFER or the buffer that the current window is
showing."
  (interactive)
  (let
      ((old-buf-list buffer-list)
       win)
    (unless buffer
      (setq buffer (current-buffer)))
    (when (setq win (make-window x y w h))
      (setq window-list (cons win window-list))
      (with-window win
	(setq buffer-list (cons buffer (delq buffer
					     (copy-sequence old-buf-list))))
	(set-current-buffer buffer win))
      win)))

(defun close-window (&optional win)
  "Close window WIN, or the current window."
  (interactive)
  (unless win
    (setq win (current-window)))
  (if (= (window-count) 1)
      (save-and-quit)
    (setq window-list (delq win window-list))
    (destroy-window win)))

(defun in-new-window (command)
  (goto-new-window)
  (call-command command))

(defun goto-new-window ()
  (interactive)
  (set-current-window (open-window) t))

(defun toggle-iconic ()
  "Toggle the current window between iconified and normal states."
  (interactive)
  (if (window-asleep-p)
      (unsleep-window)
    (sleep-window)))


;; View handling

(defun open-view (&optional sibling lines)
  "Create a new view next to view SIBLING. LINES defines the number of
lines it contains."
  (interactive)
  (unless sibling
    (setq sibling (current-view)))
  (let
      ((view (make-view sibling nil lines))
       (old-buf-list buffer-list))
    (with-view view
      (setq buffer-list (copy-sequence old-buf-list)))
    view))

(defun close-view (&optional view)
  "Close VIEW."
  (interactive)
  (if (minibuffer-view-p view)
      (error "Can't close minibuffer view")
    (destroy-view view)))

(defun close-other-views (&optional view)
  "Close all views in the current window except for VIEW, or the current one."
  (interactive)
  (unless view
    (setq view (current-view)))
  (let
      ((doomed (window-first-view))
       next)
    (while (> (window-view-count) 2)
      (setq next (next-view doomed))
      (unless (or (eq doomed view) (minibuffer-view-p doomed))
	(destroy-view doomed))
      (setq doomed next))))

(defun in-other-view (command)
  "Switches to the `other' view in this window then calls the command
COMMAND in it."
  (goto-other-view)
  (call-command command))

(defun goto-other-view ()
  "Switch to a different view in the current window."
  (interactive)
  (set-current-view (other-view)))

(defun other-view (&optional lines)
  "Return a different view in the current window. If LINES is given it
defines the number of lines to give the view. If LINES is the symbol t
then no change is made to the size of the chosen view, otherwise it will be
set so that it and the current view are roughly the same size."
  (if (= 2 (window-view-count))
      ;; open-view sets the new view as the current view; I don't
      ;; want that, so protect the current context.
      (with-view (current-view)
	(open-view nil lines))
    (let
	((view (next-view))
	 total desired)
      (when (minibuffer-view-p view)
	(setq view (prev-view)))
      (unless (eq lines t)
	(setq total (+ (cdr (view-dimensions))
		       (cdr (view-dimensions view)))
	      desired (or lines (/ total 2)))
	(enlarge-view (- total desired (cdr (view-dimensions)))))
      view)))

(defun goto-next-view (&optional all-windows-p)
  "Cycles through the available views. If ALL-WINDOWS-P is t views in
windows other than the current window are used when needed."
  (interactive "P")
  (set-current-view (next-view nil all-windows-p) all-windows-p))

(defun scroll-next-view (&optional count)
  "Scroll the view following the current view in this window by COUNT
screenfuls. When called interactively COUNT is taken from the prefix arg.
Negative arguments scroll backwards."
  (interactive "p")
  (with-view (next-view)
    (next-screen count)))

(defun enlarge-view (&optional count)
  "Enlarge the current view by one line. If COUNT is specified enlarge
by COUNT lines. When called interactively, COUNT is taken from the prefix
argument."
  (interactive "p")
  (unless count (setq count 1))
  (let*
      ((views (window-view-list))
       (view-count (1- (window-view-count)))	;ignore minibuf
       (view-index (- view-count (1- (length (memq (current-view) views)))))
       view)
    (cond
     ((= view-count 1)
      (error "Can't resize a single view"))
     ((= view-index (1- view-count))
      ;; Last view in window, expand the previous window negatively
      (setq view (previous-view)
	    count (- count)))
     (t
      (setq view (current-view))))
    (set-view-dimensions view nil (+ (cdr (view-dimensions view)) count))))

(defun shrink-view (&optional count)
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
    (when (equal (view-origin) (start-of-buffer))
      (let
	  ((view-rows (cdr (view-dimensions)))
	   (end (char-to-display-pos (end-of-buffer))))
	(when (and end (>= view-rows (pos-line end)))
	  (goto (start-of-buffer))
	  ;; add one since positions count from zero
	  (enlarge-view (- (pos-line end) view-rows -1)))))))


;; Misc

(defun add-buffer (buffer)
  "Make sure that BUFFER is in the `buffer-list' of all open windows. It gets
put at the end of the list."
  (let
      ((win-list window-list))
    (while (consp win-list)
      (with-window (car win-list)
	(let
	    ((view-list (window-view-list)))
	  (while (and (consp view-list)
		      (not (minibuffer-view-p (car view-list))))
	    (with-view (car view-list)
	       (setq buffer-list (nconc (delq buffer buffer-list)
					(cons buffer nil))))
	    (setq view-list (cdr view-list)))))
      (setq win-list (cdr win-list)))))

(defun remove-buffer (buffer)
  "Delete all references to BUFFER in any of the windows' `buffer-list'"
  (let
      ((win-list window-list))
    (while (consp win-list)
      (with-window (car win-list)
	(let
	    ((view-list (window-view-list)))
	  (while (and (consp view-list)
		      (not (minibuffer-view-p (car view-list))))
	    (with-view (car view-list)
	      (setq buffer-list (delq buffer buffer-list))
	      (when (eq (current-buffer (car view-list)) buffer)
		(set-current-buffer (car buffer-list) (car view-list))))
	    (setq view-list (cdr view-list)))))
      (setq win-list (cdr win-list)))))
