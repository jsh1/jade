;;;; miranda.jl -- Miranda session in a buffer
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

(require 'shell)
(provide 'miranda)

(defvar miranda-program "mira"
  "Program to start for Miranda session.")

(defvar miranda-prompt "^(Miranda |::.*:)"
  "Regexp matching Miranda session's prompt.")

;;;###autoload
(defun miranda (&optional arg)
  "Run a Miranda interpreter in a buffer called `*miranda*' using the major
mode `shell-mode'. ARG is a string to pass as a command line argument."
  (interactive (list (prompt-for-file "Miranda script:" t
				      (buffer-file-name)
				      (buffer-file-name))
		     t))
  (let
      ((buffer (get-buffer "*miranda*"))
       (dir (if arg
		(file-name-directory arg)
	      default-directory)))
    (goto-other-view)
    (if (or (not buffer) (with-buffer buffer shell-process))
	(progn
	  (goto-buffer (open-buffer "*miranda*" t))
	  (setq default-directory dir
		shell-program miranda-program
		shell-prompt-regexp miranda-prompt
		shell-program-args (and arg (list arg)))
	  (shell-mode))
      (goto-buffer buffer)
      (setq default-directory dir
	    shell-program-args (and arg (list arg)))
      (shell-start-process))))
