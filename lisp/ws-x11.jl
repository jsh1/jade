;;;; ws-x11.jl -- X11 initialisation
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
(defun x11-block-status-function ()
  (if (blockp)
      (x11-set-selection 'xa-primary (block-start) (block-end))
    (x11-lose-selection 'xa-primary)))

;; Called after killing some text
(defun x11-after-kill-function ()
  (x11-set-selection 'xa-primary (killed-string)))

;; Supplies the yanked string if selection is active
(defun x11-pre-yank-function ()     
  (when (x11-selection-active-p 'xa-primary)
    (x11-get-selection 'xa-primary)))

(add-hook 'block-status-hook 'x11-block-status-function)
(add-hook 'after-kill-hook 'x11-after-kill-function)
(add-hook 'pre-yank-hook 'x11-pre-yank-function)
