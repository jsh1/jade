;;;; gtk-mail-dir.jl -- GTK interface to the mail directory
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
(require 'mail-dir)

(defun gmds-concat-list (list)
  (let
      ((out nil))
    (while list
      (setq out (cons (car list) out))
      (when (cdr list)
	(setq out (cons ?  out)))
      (setq list (cdr list)))
    (apply 'concat (nreverse out))))

(defun gtk-mail-dir-summary ()
  (interactive)
  (let*
      ((titles ["name" "address"])
       (window (gtk-window-new 'toplevel))
       (scrolled-window (gtk-scrolled-window-new))
       (clist (gtk-clist-new-with-titles titles)))
    (gtk-container-add window scrolled-window)
    (gtk-container-add scrolled-window clist)
    (gtk-scrolled-window-set-policy scrolled-window 'automatic 'automatic)
    (gtk-widget-set-usize window 300 400)
    (gtk-signal-connect window "delete_event"
			#'(lambda (w) (gtk-widget-destroy w)))
    (mapc #'(lambda (item)
	      (gtk-clist-append
	       clist (vector (car (md-get-field item ':name))
			     (gmds-concat-list
			      (md-get-field item ':net)))))
	  mail-address-list)
    (let
	((i 0))
      (while (< i (length titles))
	(gtk-clist-set-column-auto-resize clist i t)
	(setq i (1+ i))))
    (gtk-widget-show-all window)))
