;;;; rcs-hooks.jl -- Always-loaded parts of the RCS interface
;;;  Copyright (C) 1997 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'rcs-hooks)

;; Returns t if FILE-NAME is under RCS control
(defun rcs-file-p (file-name)
  (or (file-exists-p (concat file-name ",v"))
      (file-exists-p (expand-file-name
		      (concat (file-name-nondirectory file-name) ",v")
		      (concat (file-name-directory file-name) "RCS")))))

;; Function called from the after-read-file-hook
(defun rcs-read-file-function (buffer)
  (when (rcs-file-p (buffer-file-name buffer))
    (rcs-init-file buffer)))

(add-hook 'after-read-file-hook 'rcs-read-file-function)

;;;###autoload (load "rcs-hooks")
