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

(defvar mime-encode-auto-type-alist
  '(("\\.jpe?g$" . "image/jpeg")
    ("\\.gif$" . "image/gif")
    ("\\.tiff$" . "image/tiff")
    ("\\.e?ps$" . "application/postscript")
    ("\\.html?$" . "text/html")
    ("\\.txt?$" . "text/plain"))
  "Alist of (REGEXP . CONTENT-TYPE-STRING).")


;; Message composition functions

(defun mime-encode-insert (content-type content-disp &optional plist)
  (let
      ((start (cursor-pos)))
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
    (make-extent (forward-char 2 start) (forward-char -3 (cursor-pos))
		 (list* 'face mime-highlight-face
			'content-type content-type
			'content-disp content-disp
			plist))))

;;;###autoload
(defun mime-encode-attach-file (filename &optional content-type)
  (interactive (let*
		   ((file (prompt-for-file "File to attach:" t))
		    (default (cdr (assoc-regexp
				   file mime-encode-auto-type-alist t)))
		    (type (prompt-for-string
			   (format nil "Content type (default: %s):"
				   default))))
		 (list file (if (and type (not (string= type "")))
				type default))))
  (mime-encode-insert content-type
		      `(attachment
			(filename . ,(file-name-nondirectory filename)))
		      (list 'content-source filename)))

;;;###autoload
(defun mime-encode-attach-buffer (buffer &optional content-type)
  (interactive (let*
		   ((buffer (prompt-for-buffer "Buffer to attach:" t))
		    (name (or (file-name-nondirectory
			       (buffer-file-name buffer))
			      (buffer-name buffer)))
		    (default (cdr (assoc-regexp
				   name mime-encode-auto-type-alist t)))
		    (type (prompt-for-string
			   (format nil "Content type (default: %s):"
				   default))))
		 (list buffer (if (and type (not (string= type "")))
				  type default))))
  (mime-encode-insert content-type `(attachment (filename . ,name))
		      (list 'content-source buffer)))


;; Encoding composed message to MIME

(defun mime-encode-params (params)
  (mapc #'(lambda (param)
	    (format (current-buffer) ";\n\t%s=" (car param))
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
      (when (nth 1 cell)
	(require (nth 1 cell)))
      (funcall (nth 2 cell) input (current-buffer)))))

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
      (insert "Content-type: multipart/mixed; boundary=\"")
      (setq boundaries (cons (cursor-pos) boundaries))
      (insert "\"\n")
      ;; Then boundary of the first message part
      (goto (forward-line))
      (insert "--")
      (setq boundaries (cons (cursor-pos) boundaries))
      (insert "\n\n")
      (mime-encode-content-type (list 'text 'plain))
      (insert "\n")
      (let
	  (attachments)
	;; First locate all attachments (can't delete extents while
	;; map-extent'ing)
	(map-extents #'(lambda (e)
			 (when (extent-get e 'content-type)
			   ;; Found an attachment
			   (setq attachments
				 (cons (list*
					;; Include the [[..]] surrounding
					;; the extent
					'start
					(forward-char -2 (extent-start e))
					'end
					(forward-char 3 (extent-end e))
					(extent-plist e))
				       attachments))
			   (set-extent-plist e nil)))
		     (start-of-buffer) (end-of-buffer))
	;; Then convert them to MIME format
	;; Working from bottom to top of the buffer to avoid screwing with
	;; the positions of the unencoded attachments
	(mapc #'(lambda (att)
		  (let*
		      ((start (car (cdr (memq 'start att))))
		       (end (car (cdr (memq 'end att))))
		       (content-type (car (cdr (memq 'content-type att))))
		       (content-disp (car (cdr (memq 'content-disp att))))
		       (content-xfer-enc (if (eq (car content-type) 'text)
					     'quoted-printable 'base64))
		       (source (car (cdr (memq 'content-source att)))))
		    (goto start)
		    (delete-area start end)
		    (unless (zerop (pos-col (cursor-pos)))
		      (insert "\n"))
		    (insert "--")
		    (setq boundaries (cons (cursor-pos) boundaries))
		    (insert "\n")
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
		     ((streamp source)
		      (mime-encode-stream content-xfer-enc source))
		     (t
		      (error "Don't know how to access source: %s" source)))
		    ;; XXX: this is truly fucked up, right here
		    (insert "\n--")
		    (setq boundaries (cons (cursor-pos) boundaries))
		    (insert "\n")
		    (mime-encode-content-type (list 'text 'plain))
		    (insert "\n")))
	      attachments))
      ;; Add the trailing boundary
      (goto (end-of-buffer))
      (unless (zerop (pos-col (cursor-pos)))
	(insert "\n"))
      (insert "--")
      (setq boundaries (cons (cursor-pos) boundaries))
      (insert "--\n")
      ;; Now compute the boundary string and insert it wherever required
      (while (null boundary-string)
	(setq boundary-string (mime-encode-make-boundary))
	(when (search-forward boundary-string (start-of-buffer))
	  (setq boundary-string nil)))
      (mapc #'(lambda (p)
		(insert boundary-string p)) boundaries))))
