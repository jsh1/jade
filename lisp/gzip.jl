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

(provide 'gzip)

;;; Simple hooks to read and write compressed (compress or gzip) files.
;;; Do `(require 'gzip)' to load and install it. Any files whose name
;;; ends in `.gz' or `.Z' will be (de-)compressed as necessary.

;;; TO-DO:
;;; * Info should work with compressed files
;;; * able to specify what suffixes run what (de)compressors

;; Uncompress FILE-NAME into the current buffer
(defun gzip-uncompress (file-name)
  (when (file-exists-p file-name)
    (let
	((proc (make-process (current-buffer))))
      (message (concat "Uncompressing `" file-name "'") t)
      ;; gunzip can do .Z files as well
      (unless (zerop (call-process proc nil "gunzip" "-c" file-name))
	(signal 'file-error (list "Can't gunzip file" file-name))))))
    
;; In the read-file-hook
(defun gzip-read-file (file-name buffer)
  (when (string-match "\\.(gz|Z)$" file-name)
    ;; gzipped file, decompress it into the buffer
    (let
	((old-pos (cursor-pos)))
      (with-buffer buffer
	(gzip-uncompress file-name)
	(goto old-pos)
	(unless mode-name
	  ;; so init-mode has a chance
	  (setq mode-name (and (string-match "^(.*)\\.(gz|Z)$" file-name)
			       (expand-last-match "\\1"))))
	(setq buffer-file-modtime (file-modtime file-name))
	(set-buffer-file-name buffer file-name))
      t)))

;; In insert-file-hook
(defun gzip-insert-file (file-name)
  (when (string-match "\\.(gz|Z)$" file-name)
    ;; compressed file
    (gzip-uncompress file-name)))

;; In write-file-hook
(defun gzip-write-file (file-name buffer)
  (when (string-match "\\.(gz|Z)$" file-name)
    (let
	((modes (when (file-exists-p file-name) (file-modes file-name)))
	 (tmp-name (make-temp-name))
	 (compressor (if (string-match "\\.Z$" file-name) "compress" "gzip"))
	 dst-file proc)
      (backup-file file-name)
      (when (and (with-buffer buffer
		   (write-buffer-contents tmp-name))
		 (setq dst-file (open-file file-name 'write)))
	(unwind-protect
	    (progn
	      (setq proc (make-process dst-file))
	      (message (concat "Compressing `" file-name "'... ") t)
	      (when (/= (call-process proc nil compressor "-c" tmp-name) 0)
		(signal 'file-error (list "Can't compress file"
					  tmp-name compressor))))
	  (close-file dst-file)
	  (delete-file tmp-name))
	(when modes
	  (set-file-modes file-name modes))
	t))))

(add-hook 'read-file-hook 'gzip-read-file)
(add-hook 'insert-file-hook 'gzip-insert-file)
(add-hook 'write-file-hook 'gzip-write-file)
