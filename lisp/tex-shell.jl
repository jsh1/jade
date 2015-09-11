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

(defvar tex-shell-program shell-program
  "The name of the program used to start the tex-shell subprocess.")

(defvar tex-shell-program-args shell-program-args
  "The list of argumemts to give to the tex-shell subprocess.")

(defvar tex-directory "."
  "Directory for running TeX in.")

(defvar tex-run-command "tex %s"
  "Command format for running TeX")

(defvar bibtex-run-command "bibtex %s"
  "Command format for running BibTeX")

(defvar tex-dvi-print-command "dvips %s"
  "Command for printing a DVI file")

(defvar tex-dvi-view-command "xdvi %s >/dev/null 2>&1 </dev/null &"
  "Command for viewing a DVI file")

(defvar tex-show-queue-command "lpq"
  "Command for displaying the printer queue")

(defvar tex-last-dvi-file nil)

;; Returns the buffer containing the running tex-shell
(defun tex-init-shell ()
  (let
      ((buffer (get-buffer "*tex-shell*")))
    (unless buffer
      (setq buffer (open-buffer "*tex-shell*")))
    (with-buffer buffer
      (unless shell-process
	(clear-buffer)
	(kill-all-local-variables)
	(setq shell-program tex-shell-program)
	(setq shell-program-args tex-shell-program-args)
	(shell-mode)
	(call-hook 'tex-shell-hook)))
    (with-view (other-view)
      (goto-buffer buffer))
    buffer))

(defun tex-shell-command (shell-buffer fmt #!rest args)
  (with-buffer shell-buffer
    (goto (end-of-buffer))
    (let
	((command (apply format nil fmt args)))
      (insert command)
      (insert "\n")
      (write shell-process command)
      (write shell-process #\newline))))

(defun tex-kill-job ()
  (interactive)
  (let
      ((buffer (get-buffer "*tex-shell*")))
    (when buffer
      (kill-buffer buffer))))

(defun tex-recenter-output-buffer ()
  (interactive)
  (let*
      ((buffer (get-buffer "*tex-shell*"))
       (view (and buffer (get-buffer-view buffer t))))
    (cond (view
	   (with-view view
	     (goto (end-of-buffer))))
	  (buffer
	   (with-view (other-view)
	     (goto-buffer buffer)
	     (goto (end-of-buffer))))
	  (t
	   (error "No TeX-shell!")))))


;; Running shell commands

;;;###autoload
(defun tex-file (file-name)
  (interactive (list (buffer-file-name)))
  (save-some-buffers)
  (let
      ((buffer (tex-init-shell))
       (local-file (or (local-file-name file-name)
		       (error "Can't run TeX on remote files!")))
       (dir (local-file-name
	     (file-name-as-directory (expand-file-name tex-directory)))))
    (tex-shell-command buffer "cd %s" dir)
    (tex-shell-command buffer tex-run-command local-file)
    (when (string-match "^(.*)\\.tex$" local-file nil t)
      (setq tex-last-dvi-file (expand-last-match "\\1.dvi")))))

;;;###autoload
(defun bibtex-file (file-name)
  (interactive (list (buffer-file-name)))
  (save-some-buffers)
  (when (string-match "^(.*)\\.tex$" file-name)
    (setq file-name (expand-last-match "\\1")))
  (let
      ((buffer (tex-init-shell))
       (local-file (or (local-file-name file-name)
		       (error "Can't run TeX on remote files!")))
       (dir (local-file-name
	     (file-name-as-directory (expand-file-name tex-directory)))))
    (tex-shell-command buffer "cd %s" dir)
    (tex-shell-command buffer bibtex-run-command local-file)))

;;;###autoload
(defun tex-print ()
  (interactive)
  (or tex-last-dvi-file
      (error "No DVI file to print!"))
  (let
      ((buffer (tex-init-shell)))
    (tex-shell-command buffer tex-dvi-print-command tex-last-dvi-file)))

;;;###autoload
(defun tex-view ()
  (interactive)
  (or tex-last-dvi-file
      (error "No DVI file to print!"))
  (let
      ((buffer (tex-init-shell)))
    (tex-shell-command buffer tex-dvi-view-command tex-last-dvi-file)))

;;;###autoload
(defun tex-show-print-queue ()
  (interactive)
  (let
      ((buffer (tex-init-shell)))
    (tex-shell-command buffer tex-show-queue-command)))
