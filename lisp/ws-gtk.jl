;;;; ws-gtk.jl -- GTK initialisation
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

;; Called when the block changes
(defun gtk-jade-block-status-function ()
  (if (blockp)
      (gtk-jade-set-selection 'xa-primary (block-start) (block-end))
    (gtk-jade-lose-selection 'xa-primary)))

;; Called after killing some text
(defun gtk-jade-after-kill-function ()
  ;; destroy the selection
  (gtk-jade-set-selection 'xa-primary nil))

;; Supplies the yanked string if selection is active
(defun gtk-jade-pre-yank-function ()     
  (when (gtk-jade-selection-active-p 'xa-primary)
    (gtk-jade-get-selection 'xa-primary)))

(add-hook 'block-status-hook 'gtk-jade-block-status-function)
(add-hook 'after-kill-hook 'gtk-jade-after-kill-function)
(add-hook 'pre-yank-hook 'gtk-jade-pre-yank-function)

(autoload 'popup-menu-from-spec "gtk-menu" t)
