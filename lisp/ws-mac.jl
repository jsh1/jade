;;;; ws-mac.jl -- Mac OS X initialisation
;;;  Copyright (C) 1998-2007 John Harper <jsh@unfactored.org>

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

;; Called after killing some text
(defun mac-after-kill-function ()
  (mac-set-pasteboard (killed-string)))

;; Supplies the yanked string if selection is active
(defun mac-pre-yank-function ()     
  (mac-get-pasteboard))

(add-hook 'after-kill-hook mac-after-kill-function)
(add-hook 'pre-yank-hook mac-pre-yank-function)
