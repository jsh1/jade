;;;; gtk-decor.jl -- Funky GTK window tricks
;;;  Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
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

(require 'gtk)

(defun gdecor-find-container (base pred)
  (while (and base (not (funcall pred base)))
    (setq base (gtk-widget-parent base)))
  base)


;; Notebooks

(defun notebook-split-window (label)
  (interactive "sPane label")
  (let*
      ((window (current-window))
       (new (gtk-widget-get-toplevel (gtk-jade-new)))
       (book (gdecor-find-container
	      (gtk-jade-window-widget window) 'gtk-notebook-p)))
    (unless book
      (let*
	  ((toplevel (gtk-widget-get-toplevel
		      (gtk-jade-window-widget window)))
	   (child (car (gtk-container-children toplevel))))
	(setq book (gtk-notebook-new))
	(gtk-container-remove toplevel child)
	(gtk-container-add toplevel book)
	(gtk-widget-show-all toplevel)
	(gtk-notebook-append-page book child (gtk-label-new "Original"))))
    (gtk-notebook-append-page book new (gtk-label-new label))
    (gtk-widget-show-all new)
    (gtk-widget-show-all book)))

(defun notebook-next-page (count)
  (interactive "p")
  (let
      ((book (gdecor-find-container
	      (gtk-jade-window-widget (current-window)) 'gtk-notebook-p)))
    (while (> count 0)
      (gtk-notebook-next-page book)
      (setq count (1- count)))
    (while (< count 0)
      (gtk-notebook-prev-page book)
      (setq count (1+ count)))))


;; Framed windows

(defvar gdecor-framed-windows nil)

(defun gdecor-frame-widget (widget &optional label)
  (let*
      ((parent (gtk-widget-parent widget))
       (frame (gtk-frame-new (or label "jade"))))
    (when parent
      (gtk-container-remove parent widget))
    (gtk-container-add frame widget)
    (when parent
      (gtk-container-add parent frame))
    (gtk-widget-show frame)))

(defun framed-window-mode ()
  (interactive)
  (if gdecor-framed-windows
      (progn
	(mapc #'(lambda (win)
		  (let
		      ((frame (gdecor-find-container
			       (gtk-jade-window-widget win) 'gtk-frame-p)))
		    (when frame
		      (let
			  ((parent (gtk-widget-parent frame))
			   (child (car (gtk-container-children frame))))
			(gtk-container-remove frame child)
			(gtk-container-remove parent frame)
			(gtk-container-add parent child)))))
	      (window-list))
	(remove-hook 'gtk-jade-new-hook 'gdecor-frame-widget))
    (mapc #'(lambda (win)
	      (gdecor-frame-widget (gtk-jade-window-widget win)))
	  (window-list))
    (add-hook 'gtk-jade-new-hook 'gdecor-frame-widget))
  (setq gdecor-framed-windows (not gdecor-framed-windows)))
