;;;; dired-hooks.jl -- Hooks to install dired
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

;; Hook to call dired whenever someone tries to open a directory
(defun dired-find-file-hook (f)
  (when (file-directory-p f)
    ;; hack for hook conventions
    (with-buffer (current-buffer)
      (dired f)
      (current-buffer))))

(add-hook 'find-file-hook 'dired-find-file-hook)
