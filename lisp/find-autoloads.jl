;;;; find-autoloads.jl -- Rebuild the autoload.jl file
;;;  Copyright (C) 1994 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'find-autoloads)

;;; Code to manipulate the sequence of autoload functions in autoload.jl
;;; Basically you just do `ESC x add-autoloads' to add all marked functions
;;; and macros to the list and `ESC x remove-autoloads' to take them out.

(defun autoload-insert-entry (str)
  (if (search-forward str (start-of-buffer))
      (progn
	(delete-area (match-start) (end-of-line (match-start)))
	(goto (match-start)))
    (unless (re-search-backward "^;;; ::autoload-end::" (end-of-buffer))
      (error "autoload.jl file malformed"))
    (goto (end-of-line (forward-line -1 (match-start))))
    (insert "\n"))
  (insert str))

(defun autoload-remove-entry (str)
  (when (search-forward str (start-of-buffer))
    (goto (match-start))
    (delete-area (match-start)
		 (start-of-line (forward-line 1 (match-start))))))

(defun autoload-do-magic (buf line-fun)
  (let
      (aload-buf)
    (when (setq aload-buf (open-file (file-name-concat lisp-lib-dir
						       "autoload.jl")))
      (goto-buffer aload-buf)
      (let
	  ((pos (start-of-buffer))
	   form
	   (short-file-name (and (string-match "^(.+)\\.jl$"
					       (file-name-nondirectory
						(buffer-file-name buf)))
				 (expand-last-match "\\1")))
	   (count 0))
	(while (setq pos (re-search-forward "^;;;###autoload[\t ]*(.*)$"
					   pos buf))
	  (setq form (expand-last-match "\\1"))
	  (when (and form (not (equal "" form)))
	    (funcall line-fun form)
	    (setq count (1+ count))
	    (message form t))
	  (setq pos (forward-line 1 pos))
	  (when (and (looking-at "^\\(def(macro|un) " pos buf)
		     (setq form (read (cons buf pos)))
		     (memq (car form) '(defun defmacro)))
	    (setq form (format nil (if (assq 'interactive form)
				       ;; Can be called as a command
				       "(autoload '%s %S t)"
				     "(autoload '%s %S)")
			       (nth 1 form)
			       short-file-name))
	    (when (funcall line-fun form)
	      (setq count (1+ count))
	      (message form t))))
	count))))

;;;###autoload
(defun add-autoloads (&optional buffer)
  "Add all functions, macros or variables in the BUFFER marked by the magic
rune `;;;###autoload' to the `autoload.jl' file."
  (interactive)
  (message (format nil "Found %d autoload definition(s)"
		   (autoload-do-magic (unless buffer (current-buffer))
				      #'autoload-insert-entry))))

;;;###autoload
(defun remove-autoloads (&optional buffer)
  "Removes all autoload definitions in the buffer BUFFER which are marked by
the string `;;;###autoload' from the `autoload.jl' file."
  (interactive)
  (message (format nil "Removed %d autoload definition(s)"
		   (autoload-do-magic (unless buffer (current-buffer))
				      #'autoload-remove-entry))))
