;;;; print.jl -- Wrapper functions to print text
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
(provide 'print)


;; Options

(defvar print-command "enscript"
  "The command to print text fed to its standard input.")

;; Note that all options *must* be preceded by a space

(defvar print-standard-options ""
  "Options always given to print-command.")

(defvar print-file-prefix " -p"
  "The option prefix to specify the output file (for `print-command').")

(defvar print-printer-prefix " -P"
  "The option to specify the destination printer for `print-command'.")

(defvar print-num-alist '((2 . " -U2") (4 . " -U4") (8 . " -U8"))
  "Options to include depending on the numeric arg specified to the
print command.")

(defvar print-mode-alist '((c-mode . " -Ec")
			   (lisp-mode . " -Eelisp")
			   (perl-mode . " -Eperl")
			   (asm-mode . " -Easm")
			   (changelog-mode . " -Echangelog")
			   (read-mail-mode . " -Email")
			   (send-mail-mode . " -Email")
			   (sh-mode . " -Esh"))
  "Alist of options to include dependent on the major-mode of the current
buffer.")


;; Code

(defun print-build-shell-command (#!optional prefix buffer
				  output-file printer)
  (let
      ((command (concat print-command
			print-standard-options
			(and (numberp prefix)
			     (cdr (assoc prefix print-num-alist)))
			(and (with-buffer buffer major-mode)
			     (cdr (assq (with-buffer buffer major-mode)
					print-mode-alist)))
			(when output-file
			  (let
			      ((local-output (or (local-file-name output-file)
						 (error "File not local: %s"
							output-file))))
			    (concat print-file-prefix local-output)))
			(and printer
			     (concat print-printer-prefix printer)))))
    (if (and prefix (not (numberp prefix)))
	(or (prompt-for-string "Print command:" command)
	    (error "Null print command"))
      command)))

;;;###autoload
(defun print-buffer (#!optional prefix)
  "Print the current buffer, using `print-command' and it's associated
options."
  (interactive "P")
  (shell-command-on-buffer (print-build-shell-command prefix
						      (current-buffer))))

;;;###autoload
(defun print-buffer-to-file (file #!optional prefix)
  "Print the current buffer, leaving the output in FILE."
  (interactive "FOutput file\nP")
  (shell-command-on-buffer (print-build-shell-command prefix
						      (current-buffer)
						      file)))

;;;###autoload
(defun print-buffer-to-printer (printer #!optional prefix)
  "Print the current buffer using PRINTER."
  (interactive "sPrinter\nP")
  (shell-command-on-buffer (print-build-shell-command prefix
						      (current-buffer)
						      nil printer)))

;;;###autoload
(defun print-area (start end #!optional prefix)
  "Print the area from START to END."
  (interactive "-m\nM\nP")
  (shell-command-on-area (print-build-shell-command prefix
						    (current-buffer))
			 start end))

;;;###autoload
(defun print-area-to-file (start end file #!optional prefix)
  "Print the area from START to END leaving output in FILE."
  (interactive "-m\nM\nFOutput file\nP")
  (shell-command-on-area (print-build-shell-command prefix
						    (current-buffer)
						    file)
			 start end))

;;;###autoload
(defun print-area-to-printer (start end printer #!optional prefix)
  "Print the area from START to END using PRINTER."
  (interactive "-m\nM\nsPrinter\nP")
  (shell-command-on-area (print-build-shell-command prefix
						    (current-buffer)
						    nil printer)
			 start end))
