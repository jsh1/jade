;;;; dired.jl -- Directory editing via summary mode
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

(require 'summary)
(provide 'dired)


;; Variables

(defvar dired-keymap (copy-sequence summary-keymap))
(bind-keys dired-keymap
  "*" 'summary-mark-item
  "#" 'dired-delete-autosaves
  "~" 'dired-delete-backups
  "&" 'dired-delete-garbage
  "%" 'dired-delete-by-regexp
  "f" 'dired-find-file
  "o" 'dired-find-file-other-view
  "Ctrl-o" 'dired-display-file
  "g" 'summary-update)

(defvar dired-garbage-files "\\.(orig|rej|aux|log)$"
  "Regular expression matching files under dired that should be marked for
deletion by the `&' command.")

(defvar dired-cursor-column 45)

(defvar dired-delete-cache nil)
(make-variable-buffer-local 'dired-delete-cache)

(defvar dired-functions '((print . dired-print)
			  (after-move . (lambda ()
					  (goto-glyph
					   (pos dired-cursor-column nil))))
			  (list . dired-list)
			  (after-marking . (lambda () (summary-next-item 1)))
			  (delete . dired-delete)
			  (execute-end . dired-execute-end)
			  (select . dired-find-file)
			  (on-quit . bury-buffer))
  "Function vector for Dired mode.")


;; Basics

;;;###autoload
(defun dired (directory)
  (interactive "DDirectory:")
  (setq directory (directory-file-name directory))
  (let
      ((buffer (or (get-file-buffer directory)
		   (open-buffer (file-name-nondirectory directory))))
       (inhibit-read-only t))
    (goto-buffer buffer)
    (if (eq major-mode 'dired-mode)
	(summary-update)
      (format buffer "[Dired] %s:\n\n" directory)
      (set-buffer-file-name buffer directory)
      (summary-mode "Dired" dired-functions dired-keymap)
      (setq summary-assoc-item-function 'assoc
	    major-mode 'dired-mode))))

(defun dired-mode ()
  "Dired mode:

Major mode for viewing and manipulating directories in the local filing
system. The standard summary interface (see the function `summary-mode')
is used with the following commands that are specific to Dired:

  `#'			Mark all autosave files for deletion
  `~'			Mark all backup files for deletion
  `&'			Mark unwanted files for deletion (see the variable
		 	`dired-garbage-files' for the definition of unwanted)
  `% REGEXP RET'	Mark all files whose names match REGEXP for deletion
  `f', `RET'		Edit the current item in the current view
  `o'			Edit the current item in the other view
  `Ctrl-o'		Display the current item in the other view, but
			 leave the cursor in the Dired view
  `g', `Ctrl-l'		Refresh the directory listing\n")

(defun dired-list ()
  (sort (directory-files (buffer-file-name))))

(defun dired-print (item)
  (let*
      ((name (file-name-concat (buffer-file-name) item))
       (symlink (and (file-symlink-p name)
		     (or (file-exists-p name) 'broken))))
    (format (current-buffer) "%c%c %s %2d %8d  "
	    (if (memq 'delete (summary-get-pending-ops item)) ?D ? )
	    (if (summary-item-marked-p item) ?* ? )
	    (if symlink "lrwxrwxrwx"
	      (file-modes-as-string (file-modes name)))
	    (if (eq symlink 'broken) 1 (file-nlinks name))
	    (if (eq symlink 'broken) 0 (file-size name)))
    (if (eq symlink 'broken)
	(insert "[broken symlink]  ")
      (insert (current-time-string (file-modtime name) "%Y-%m-%d %R  ")))
    (indent-to dired-cursor-column)
    (insert item)))

(defun dired-delete (item)
  (setq dired-delete-cache (cons item dired-delete-cache)))

(defun dired-execute-end ()
  (map-y-or-n-p "Really delete file `%s'?"
		(prog1 dired-delete-cache
		  (setq dired-delete-cache nil))
		#'(lambda (f)
		    (delete-file
		     (file-name-concat (buffer-file-name) f)))))

;; Commands

(defun dired-find-file (item)
  (interactive (list (summary-current-item)))
  (find-file (file-name-concat (buffer-file-name) item)))

(defun dired-find-file-other-view (item)
  (interactive (list (summary-current-item)))
  (let
      ((name (file-name-concat (buffer-file-name) item)))
    (goto-other-view)
    (find-file name)))

(defun dired-display-file (item)
  (interactive (list (summary-current-item)))
  (let
      ((name (file-name-concat (buffer-file-name) item)))
    (with-view (other-view)
      (find-file name))))

(defun dired-delete-by-regexp (re)
  (interactive "sRegexp:")
  (mapc #'(lambda (x)
	    (when (string-match re x)
	      (summary-mark-delete x)))
	summary-items))

(defun dired-delete-autosaves ()
  (interactive)
  (dired-delete-by-regexp "^#.*#$"))

(defun dired-delete-backups ()
  (interactive)
  (dired-delete-by-regexp "~$"))

(defun dired-delete-garbage ()
  (interactive)
  (dired-delete-by-regexp dired-garbage-files))
