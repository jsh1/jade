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

(defvar dired-keymap
  (bind-keys (make-sparse-keymap summary-keymap)
    "*" 'dired-mark-executables
    "@" 'dired-mark-symlinks
    "/" 'dired-mark-directories
    "#" 'dired-delete-autosaves
    "~" 'dired-delete-backups
    "&" 'dired-delete-garbage
    "%" 'dired-%-keymap
    "f" 'dired-find-file
    "o" 'dired-find-file-other-view
    "Ctrl-o" 'dired-display-file
    "g" 'summary-update
    "C" 'dired-do-copy
    "R" 'dired-do-rename
    "+" 'dired-create-directory))

(defvar dired-%-keymap
  (bind-keys (make-sparse-keymap)
    "d" 'dired-delete-by-regexp
    "m" 'dired-mark-by-regexp))

(defvar dired-garbage-files "\\.(orig|rej|aux|log)$"
  "Regular expression matching files under dired that should be marked for
deletion by the `&' command.")

(defvar dired-directory-files directory-files
  "Function called by Dired that should return the list of files stored in
the directory named as its single argument.")
(make-variable-buffer-local 'dired-directory-files)

(defvar dired-cursor-column 45)

(defvar dired-delete-cache nil)
(make-variable-buffer-local 'dired-delete-cache)


;; Basic interface to summary-mode

;;;###autoload
(defun dired (directory #!optional directory-function)
  (interactive "DDirectory:")
  (setq directory (directory-file-name (expand-file-name directory)))
  (unless (file-directory? directory)
    (error "%S is not a directory" directory))
  (let
      ((buffer (or (get-file-buffer directory)
		   (open-buffer (file-name-nondirectory directory) t))))
    (goto-buffer buffer)
    (when directory-function
      (setq dired-directory-files directory-function))
    (set-buffer-file-name buffer directory)
    (setq *default-directory* (file-name-as-directory directory))
    (dired-mode)))

;; Put the current buffer into Dired mode, its buffer-file-name should
;; point to the directory to read
;;;###autoload
(defun dired-mode ()
  "Dired mode:\n
Major mode for viewing and manipulating directories in the local filing
system. The standard summary interface (see the function `summary-mode')
is used with commands specific to Dired added. The full list of local
bindings is:\n
\\{dired-keymap}"
  (let
      ((inhibit-read-only t))
    (if (eq? major-mode 'dired-mode)
	(summary-update)
      (format (current-buffer) "[Dired] %s:\n\n" (buffer-file-name))
      (summary-mode "Dired" dired-functions dired-keymap)
      (setq summary-assoc-item-function assoc)
      (setq major-mode 'dired-mode))))

(defun dired-list ()
  (sort! (dired-directory-files *default-directory*)))

(defun dired-print (item)
  (let*
      ((symlink (and (file-symlink? item)
		     (or (file-exists? item) 'broken))))
    (format (current-buffer) "%c%c %s %2d %8d  "
	    (if (memq 'delete (summary-get-pending-ops item)) #\D #\space)
	    (if (summary-item-marked-p item) #\* #\space)
	    (if symlink "lrwxrwxrwx" (file-modes-as-string item))
	    (if (eq? symlink 'broken) 1 (file-nlinks item))
	    (if (eq? symlink 'broken) 0 (file-size item)))
    (if (eq? symlink 'broken)
	(insert "[broken symlink]  ")
      (insert (current-time-string (file-modtime item) "%Y-%m-%d %R  ")))
    (indent-to dired-cursor-column)
    (insert item)))

(defun dired-delete (item)
  (setq dired-delete-cache (cons item dired-delete-cache)))

(defun dired-execute-end ()
  (map-y-or-n-p "Really delete file `%s'?"
		(prog1 dired-delete-cache
		  (setq dired-delete-cache nil))
		(lambda (f)
		  (if (file-directory? f)
		      (delete-directory f)
		    (delete-file f)))))
(defvar dired-functions
  (list (cons 'print dired-print)
	(cons 'after-move (lambda ()
			    (goto-glyph (pos dired-cursor-column nil))))
	(cons 'list dired-list)
	(cons 'after-marking (lambda () (summary-next-item 1)))
	(cons 'delete dired-delete)
	(cons 'execute-end dired-execute-end)
	(cons 'select find-file)
	(cons 'on-quit bury-buffer))
  "Function vector for Dired mode.")


;; Marking

(defun dired-mark-directories ()
  (interactive)
  (summary-mark-if 'file-directory?))

(defun dired-mark-by-regexp (regexp #!optional op)
  (interactive "sMark files matching regexp:")
  (summary-mark-if (lambda (f) (string-match regexp f)) op))

(defun dired-delete-by-regexp (regexp)
  (interactive "sDelete files matching regexp:")
  (dired-mark-by-regexp regexp 'delete))

(defun dired-delete-autosaves ()
  (interactive)
  (dired-delete-by-regexp "^#.*#$"))

(defun dired-delete-backups ()
  (interactive)
  (dired-delete-by-regexp "~$"))

(defun dired-delete-garbage ()
  (interactive)
  (dired-delete-by-regexp dired-garbage-files))


;; Commands

(defun dired-find-file (files root)
  (interactive (list (summary-command-items) *default-directory*))
  (mapc (lambda (f)
	  (find-file (expand-file-name f root))) files))

(defun dired-find-file-other-view (files root)
  (interactive (list (summary-command-items) *default-directory*))
  (goto-other-view)
  (dired-find-file files root))

(defun dired-display-file (files root)
  (interactive (list (summary-command-items) *default-directory*))
  (with-view (other-view)
    (dired-find-file files root)))

(defun dired-do-copy ()
  "Copy all selected files to a specified location. If more than one file
is selected, the location is a directory to copy all files into, otherwise
the location is the name of a file to copy to."
  (interactive)
  (let
      ((files (summary-command-items)))
    (if (= (length files) 1)
	(let
	    ((dest (prompt-for-file "Destination file:"
				    nil (car files) (car files))))
	  (copy-file (car files) dest)
	  (when (file-name= *default-directory* (file-name-directory dest))
	    (summary-update)))
      (let
	  ((dest (prompt-for-directory "Destination directory:" t)))
	(when (file-name= *default-directory* dest)
	  (error "Can't copy to source directory!"))
	(mapc (lambda (f)
		(copy-file f (expand-file-name f dest))) files)))))

(defun dired-do-rename ()
"Rename all selected files. If more than one file is selected, the specified
location is a directory to move all files into, otherwise the location is
the new name for the file."
  (interactive)
  (let
      ((files (summary-command-items)))
    (if (= (length files) 1)
	(rename-file (car files) (prompt-for-file "New name of file:"
						  nil nil (car files)))
      (let
	  ((dest (prompt-for-directory "Destination directory:" t)))
	(mapc (lambda (f)
		(rename-file f (expand-file-name f dest))) files)))
    (summary-update)))

(defun dired-create-directory (dir)
  "Create a directory called DIR if one doesn't already exist."
  (interactive "DDirectory to create:")
  (if (file-exists? dir)
      (or (file-directory? dir)
	  (error "File exists: %s" dir))
    (make-directory dir)
    (summary-update)))
