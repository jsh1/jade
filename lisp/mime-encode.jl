;;;; mime-encode.jl -- Main chunk of MIME encoder
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

(require 'mime-decode)
(provide 'mime-encode)

(defconst mime-encode-boundary-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")

;; some of these may be more dubious than others
(defvar mime-encode-auto-type-alist
  '(("\\.html?$" . "text/html")
    ("\\.txt?$" . "text/plain")
    ;; image types
    ("\\.jpe?g$" . "image/jpeg")
    ("\\.gif$" . "image/gif")
    ("\\.tiff$" . "image/tiff")
    ("\\.png$" . "image/png") 
    ("\\.pbm$" . "image/x-pbm")
    ("\\.pnm$" . "image/x-pnm")
    ("\\.pict$" . "image/x-pict")
    ("\\.ppm$" . "image/x-ppm")
    ("\\.xbm$" . "image/x-xbm")
    ("\\.xpm$" . "image/x-xpm")
    ("\\.xwd$" . "image/x-xwd")
    ;; video types
    ("\\.mpe?g$" . "video/mpeg")
    ;; audio types
    ("\\.au$" . "audio/basic")
    ("\\.aiff$" . "audio/x-aiff")
    ("\\.wav$" . "audio/x-wav")
    ;; application types
    ("\\.e?ps$" . "application/postscript")
    ("\\.pdf$" . "application/pdf")
    ("\\.dvi$" . "application/x-dvi")
    ("\\.gz$" . "application/x-gzip")
    ("\\.bz2$" . "application/x-bzip2")
    ("\\.tar$" . "application/x-tar")
    ("\\.tex$" . "application/x-tex")
    ("\\.texi(nfo)?$" . "application/x-texinfo"))
  "Alist of (REGEXP . CONTENT-TYPE-STRING).")

(defvar mime-encode-keymap (bind-keys (make-sparse-keymap)
			     "C-a" 'mime-encode-attach-file
			     "C-b" 'mime-encode-attach-buffer
			     "C-n" 'mime-next-part
			     "C-p" 'mime-previous-part
			     "C-d" 'mime-encode-delete-part))
(fset 'mime-encode-keymap 'keymap)
;;;###autoload (autoload-keymap 'mime-encode-keymap "mime-encode")


;; Message composition functions

(defun mime-encode-insert (content-type content-disp &optional plist)
  (and (mime-current-part t)
       (error "Can't insert in existing MIME attachments!"))
  (let
      ((start (cursor-pos))
       extent)
    (unless (listp content-type)
      (setq content-type (if (stringp content-type)
			     (condition-case nil
				 (mime-decode-content-type content-type)
			       (error
				(list 'application 'octet-stream)))
			   (list 'application 'octet-stream))))
    (cond
     ((eq major-mode 'send-mail-mode)
      (unless (memq 'mime-encode-message mail-send-hook)
	(make-local-variable 'mail-send-hook)
	(add-hook 'mail-send-hook 'mime-encode-message)))
     (t
      (error "Don't know how to hook into %s" major-mode)))
    (format (current-buffer) "[[%s: %s/%s"
	    (car content-disp) (car content-type) (nth 1 content-type))
    (when (assq 'filename (cdr content-disp))
      (format (current-buffer) " filename=%s"
	      (cdr (assq 'filename (cdr content-disp)))))
    (format (current-buffer) "]]\n")
    (setq extent (make-extent start (cursor-pos)
			      (list* 'face mime-highlight-face
				     'content-type content-type
				     'content-disp content-disp
				     plist)))
    (extent-set extent 'read-only t)))

;;;###autoload
(defun mime-encode-attach-file (filename &optional content-type inline)
  (interactive "fFile to attach:\n\nP")
  (unless content-type
    (let
	((default (or (cdr (assoc-regexp
			    filename mime-encode-auto-type-alist t))
		      "application/octet-stream")))
      (setq content-type (prompt-for-string
			  (format nil "Content type (default: %s):" default)))
      (unless (and (stringp content-type) (not (string= content-type "")))
	(setq content-type default))))
  (mime-encode-insert content-type
		      (list (if inline 'inline 'attachment)
			    (cons 'filename
				  (file-name-nondirectory filename)))
		      (list 'content-source filename)))

;;;###autoload
(defun mime-encode-attach-buffer (buffer &optional content-type inline)
  (interactive "bBuffer to attach:\n\nP")
  (let
      ((name (if (buffer-file-name buffer)
		 (file-name-nondirectory
		  (buffer-file-name buffer))
	       (buffer-name buffer))))
    (unless content-type
      (let
	  ((default (or (cdr (assoc-regexp
			      name mime-encode-auto-type-alist t))
			"application/octet-stream")))
	(setq content-type (prompt-for-string
			    (format nil "Content type (default: %s):"
				    default)))
	(unless (and (stringp content-type) (not (string= content-type "")))
	  (setq content-type default))))
    (mime-encode-insert content-type
			(list (if inline 'inline 'attachment)
			      (cons 'filename name))
			(list 'content-source buffer))))


;; Encoding composed message to MIME

(defun mime-encode-params (params)
  (mapc #'(lambda (param)
	    (insert ";")
	    (if (>= (+ (pos-col (char-to-glyph-pos))
		       (length (symbol-name (car param)))
		       (length (cdr param)) 2)
		    mail-fill-column)
		(insert "\n\t")
	      (insert " "))
	    (format (current-buffer) "%s=" (car param))
	    (if (string-match (concat ?^ mime-token-re ?$) (cdr param))
		(insert (cdr param))
	      (format (current-buffer) "\"%s\"" (cdr param))))
	params))

(defun mime-encode-content-type (content-type)
  (format (current-buffer) "Content-type: %s/%s"
	  (car content-type) (nth 1 content-type))
  (mime-encode-params (nthcdr 2 content-type))
  (insert "\n"))

(defun mime-encode-content-disp (content-disp)
  (format (current-buffer) "Content-disposition: %s" (car content-disp))
  (mime-encode-params (cdr content-disp))
  (insert "\n"))

(defun mime-encode-stream (encoding input)
  (let
      ((cell (assq encoding mime-xfer-encodings-alist)))
    (if (null cell)
	;; No encoding method, copy verbatim
	(copy-stream input (current-buffer))
      (funcall (nth 1 cell) input (current-buffer)))))

(defun mime-encode-make-boundary ()
  (let
      ((i 10)
       (chars (list ?_)))
    (while (> i 0)
      (setq chars (cons (aref mime-encode-boundary-alphabet
			      (random (length mime-encode-boundary-alphabet)))
			chars))
      (setq i (1- i)))
    (apply 'concat "jade-" chars)))

;; Encode the message in the current buffer
(defun mime-encode-message ()
  ;; First see if there actually are any attachments
  (when (catch 'foo
	  (map-extents #'(lambda (e)
			   (and (extent-get e 'content-type)
				(throw 'foo t)))
		       (start-of-buffer) (end-of-buffer)))
    ;; There are, so work through them in turn
    (let
	((inhibit-read-only t)
	 boundaries boundary-string)
      (unless (search-forward mail-header-separator (start-of-buffer))
	(error "Can't find end of headers!"))
      (goto (match-start))
      ;; Insert boilerplate headers
      (insert "MIME-Version: 1.0\n")
      (insert "Content-type: multipart/mixed; boundary=\"\"\n")
      (setq boundaries (cons (make-mark (forward-char -2)) boundaries))
      ;; Then boundary of the first message part
      (goto (forward-line))
      (insert "--\n")
      (setq boundaries (cons (make-mark (forward-char -1)) boundaries))
      (mime-encode-content-type (list 'text 'plain))
      (insert "\n")
      (let
	  (attachments next-start)
	;; First locate all attachments (can't delete extents while
	;; map-extent'ing)
	(map-extents #'(lambda (e)
			 (when (extent-get e 'content-type)
			   ;; Found an attachment
			   (setq attachments
				 (cons (list* 'start (extent-start e)
					      'end (extent-end e)
					      (extent-plist e)) attachments))
			   (set-extent-plist e nil)))
		     (start-of-buffer) (end-of-buffer))
	;; Then convert them to MIME format
	;; Working from bottom to top of the buffer to avoid screwing with
	;; the positions of the unencoded attachments
	(while attachments
	  (let*
	      ((att (car attachments))
	       (start (car (cdr (memq 'start att))))
	       (end (car (cdr (memq 'end att))))
	       (content-type (car (cdr (memq 'content-type att))))
	       (content-disp (car (cdr (memq 'content-disp att))))
	       (content-xfer-enc (if (eq (car content-type) 'text)
				     'quoted-printable 'base64))
	       (source (car (cdr (memq 'content-source att)))))
	    ;; if necessary mark the start of the following body part
	    (unless (or (>= end (end-of-buffer))
			(and next-start (>= end next-start)))
	      (goto end)
	      (insert "--\n")
	      (setq boundaries (cons (make-mark (forward-char -1)) boundaries))
	      (mime-encode-content-type (list 'text 'plain))
	      (insert "\n"))
	    (goto start)
	    (delete-area start end)
	    (unless (zerop (pos-col (cursor-pos)))
	      (insert "\n"))
	    (insert "--\n")
	    (setq boundaries (cons (make-mark (forward-char -1))
				   boundaries))
	    (mime-encode-content-type content-type)
	    (mime-encode-content-disp content-disp)
	    (format (current-buffer) "Content-transfer-encoding: %s\n"
		    content-xfer-enc)
	    (insert "\n")
	    (cond
	     ((stringp source)
	      ;; Assume SOURCE is a file name
	      (let
		  ((file (open-file source 'read)))
		(unwind-protect
		    (mime-encode-stream content-xfer-enc file)
		  (close-file file))))
	     ((bufferp source)
	      (mime-encode-stream
	       content-xfer-enc
	       (cons source (start-of-buffer source))))
	     ((streamp source)
	      (mime-encode-stream content-xfer-enc source))
	     (t
	      (error "Don't know how to access source: %s" source)))
	    (insert "\n")
	    (setq next-start start)
	    (setq attachments (cdr attachments)))))
      ;; Add the trailing boundary
      (goto (end-of-buffer))
      (unless (zerop (pos-col (cursor-pos)))
	(insert "\n"))
      (insert "----\n")
      (setq boundaries (cons (make-mark (forward-char -3)) boundaries))
      ;; Now compute the boundary string and insert it wherever required
      (while (null boundary-string)
	(setq boundary-string (mime-encode-make-boundary))
	(when (search-forward boundary-string (start-of-buffer))
	  (setq boundary-string nil)))
      (mapc #'(lambda (m)
		(insert boundary-string (mark-pos m))) boundaries))))


;; Misc commands

(defun mime-encode-delete-part (extent)
  (interactive (list (mime-current-part)))
  (extent-set extent 'read-only nil)
  (delete-area (extent-start extent) (extent-end extent)))
