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

(defun dired-read-file-hook (filename buffer)
  (when (file-directory? filename)
    (with-buffer buffer
      (set! *default-directory* (file-name-as-directory filename))
      (set-buffer-file-name buffer (directory-file-name filename))
      (set! buffer-file-modtime (file-modtime filename))
      (dired-mode)
      t)))

(defun dired-write-file-hook (filename buffer)
  (declare (unused buffer))
  (and (file-directory? filename)
       (error "Can't write to directories, %s" filename)))

(defun dired-insert-file-hook (filename)
  (and (file-directory? filename)
       (error "Can't insert directories, %s" filename)))

(add-hook 'read-file-hook dired-read-file-hook)
(add-hook 'write-file-hook dired-write-file-hook)
(add-hook 'insert-file-hook dired-insert-file-hook)

;;;###autoload (load "dired-hooks")
