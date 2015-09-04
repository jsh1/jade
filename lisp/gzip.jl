;;;; gzip.jl -- Editing gzipped files
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

;; TODO:
;;	Makes no attempt to [de]compress remote files. This could
;;	be done by copying to-from the local fs, or whatever..

(provide 'gzip)


;; Configuration:

(defvar auto-compression-alist
  '(("\\.g?z$" ("gunzip" "-c") ("gzip" "-c"))
    ("\\.Z$" ("uncompress" "-c") ("compress" "-c"))
    ("\\.bz2$" ("bzip2" "-d") ("bzip2" "-f")))
  "List of (FILE-REGEXP DECOMPRESS-COMMAND COMPRESS-COMMAND) where
DECOMPRESS-COMMAND and COMPRESS-COMMAND are lists of strings, the command
names followed by their argument lists, such that they uncompress or compress
their standard input to their standard output.")

(defvar auto-compression-mode-enabled nil
  "When t, files whose suffixes match `compression-file-alist' are
automatically uncompressed when loaded, and recompressed when saved.")


;; Entry point

;;;###autoload
(defun auto-compression-mode (#!optional force-active)
  "Toggle automatic decompression and compression of files whose suffixes
match well-known suffixes."
  (interactive "P")
  (setq auto-compression-mode-enabled
	(or force-active (not auto-compression-mode-enabled)))
  (unless force-active
    (message (if auto-compression-mode-enabled
		 "Auto compression enabled"
	       "Auto compression disabled"))))


;; Hooks

;; Find the (REGEXP DECOMPRESSOR COMPRESSOR) rule for FILE-NAME. If FILE-NAME
;; isn't a local file, don't bother looking
(defun gzip-file-rule (file-name)
  (when (and auto-compression-mode-enabled
	     (local-file-name file-name))
    (assoc-regexp file-name auto-compression-alist)))

;; Uncompress FILE-NAME into the current buffer
(defun gzip-uncompress (file-name rule)
  (when (file-exists? file-name)
    (let
	((proc (make-process (current-buffer))))
      (message (concat "Uncompressing `" file-name "'") t)
      (unless (zero? (apply call-process proc file-name (nth 1 rule)))
	(signal 'file-error (list "Can't uncompress file" file-name))))))
    
;; In the read-file-hook
(defun gzip-read-file (file-name buffer)
  (let
      ((rule (gzip-file-rule file-name)))
    (when rule
      (let
	  ((old-pos (cursor-pos)))
	(with-buffer buffer
	  (gzip-uncompress file-name rule)
	  (goto old-pos)
	  (setq buffer-file-modtime (file-modtime file-name))
	  (set-buffer-file-name buffer file-name)
	  (setq *default-directory* (file-name-directory file-name)))
	t))))

;; In insert-file-hook
(defun gzip-insert-file (file-name)
  (let
      ((rule (gzip-file-rule file-name)))
    (when rule
      (gzip-uncompress file-name rule))))

;; In write-file-hook
(defun gzip-write-file (file-name buffer)
  (let
      ((rule (gzip-file-rule file-name)))
    (when rule
      (let
	  ((modes (when (file-exists? file-name) (file-modes file-name)))
	   (tmp-name (make-temp-name))
	   dst-file proc)
	(backup-file file-name)
	(when (and (with-buffer buffer
		     (write-buffer-contents tmp-name))
		   (setq dst-file (open-file file-name 'write)))
	  (unwind-protect
	      (progn
		(setq proc (make-process dst-file))
		(message (concat "Compressing `" file-name "'... ") t)
		(when (/= (apply call-process proc tmp-name (nth 2 rule)) 0)
		  (signal 'file-error
			  (list "Can't compress file" tmp-name))))
	    (close-file dst-file)
	    (delete-file tmp-name))
	  (when modes
	    (set-file-modes file-name modes))
	  t)))))

(add-hook 'read-file-hook gzip-read-file)
(add-hook 'insert-file-hook gzip-insert-file)
(add-hook 'write-file-hook gzip-write-file)
