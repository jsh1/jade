;;;; tex-shell.jl -- Run TeX within jade
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

(require 'shell)
(provide 'tex-shell)

(defvar tex-directory "."
  "Directory for running TeX in.")

(defvar tex-run-command "tex %s"
  "Program for running TeX")

(defun tex-init-shell ()
  (let
      ((buffer (get-buffer "*tex-shell*")))
    (unless buffer
      (setq buffer (open-buffer "*tex-shell*")))
    (with-buffer buffer
      (unless shell-process
	(clear-buffer)
	(kill-all-local-variables)
	(shell-mode)
	(call-hook 'tex-shell-hook)))
    (with-view (other-view)
      (goto-buffer buffer))
    buffer))

;;;###autoload
(defun tex-file (file-name)
  (interactive (list (buffer-file-name)))
  (let
      ((buffer (tex-init-shell))
       (local-file (or (local-file-name file-name)
		       (error "Can't run TeX on remote files!")))
       (dir (local-file-name
	     (file-name-as-directory (expand-file-name tex-directory))))
       (command tex-run-command))
    (with-buffer buffer
      (goto (end-of-buffer))
      ;; cd to the correct directory
      (format shell-process "cd %s\n" dir)
      (format buffer "cd %s\n" dir)
      ;; then run TeX
      (format shell-process command local-file)
      (write shell-process ?\n)
      (format buffer command local-file)
      (write buffer ?\n))))
