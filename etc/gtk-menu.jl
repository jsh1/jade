;;;; gtk-menu.jl -- Popup menus for GTK
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
(require 'popup-menus)
(provide 'gtk-menu)

(defun gtk-jade-create-menu (spec #!optional bar)
  (let
      ((menu (if bar (gtk-menu-bar-new) (gtk-menu-new))))
    (mapc #'(lambda (cell)
	      (let
		  (label item)
		(when (and cell (symbol? (car cell)))
		  (setq cell (symbol-value (car cell))))
		(if (null? cell)
		    (setq item (gtk-menu-item-new))
		  (setq label (car cell))
		  (cond ((function? (cdr cell))
			 (setq cell (funcall (cdr cell))))
			((and (symbol? (cdr cell))
			      (function? (symbol-value (cdr cell))))
			 (setq cell (funcall (symbol-value (cdr cell)))))
			(t
			 (setq cell (cdr cell))))
		  (cond
		   ((or (commandp (car cell)) (function? (car cell)))
		    (when popup-menus-show-shortcuts
		      (let
			  ((loc (where-is (car cell))))
			(when loc
			  (setq label (concat label " \(" (car loc) ?\))))))
		    (setq item (gtk-menu-item-new-with-label label))
		    (gtk-signal-connect item "activate"
					#'(lambda ()
					    (popup-menu-dispatch-command
					     (car cell)))))
		   ((pair? (car cell))
		    (let
			((sub (gtk-jade-create-menu cell)))
		      (setq item (gtk-menu-item-new-with-label label))
		      (gtk-menu-item-set-submenu item sub)))
		   ((eq? (car cell) t)
		    (setq item (gtk-menu-item-new-with-label label))
		    (gtk-signal-connect item "activate" (nth 1 cell)))))
		(when item
		  (funcall (if bar gtk-menu-bar-append
			     gtk-menu-append) menu item)
		  (gtk-widget-show item))))
	  spec)
    menu))

(defun popup-menu-from-spec (spec)
  (gtk-menu-popup-interp (gtk-jade-create-menu spec)
			 nil nil 0 (gtk-last-timestamp)))
