;;;; mime-decode.jl -- Main chunk of MIME decoder
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

(require 'maildefs)
(require 'mail-headers)
(provide 'mime-decode)

(defvar mime-xfer-encodings-alist
  '((base64 mime-base64 mime-encode-base64 mime-decode-base64)
    (quoted-printable mime-quoted-printable
		      mime-encode-quoted-printable
		      mime-decode-quoted-printable))
  "Alist of (ENCODING FEATURE ENCODER DECODER) where ENCODER and DECODER are
functions that operate as filters on their argument streams.")

(defvar mime-viewer-alist
  '((image . "xview %s >/dev/null 2>&1 </dev/null &"))
  "Alist of (TYPE . COMMAND)")

(defface mime-highlight-face "Face to highlight MIME non-inline components."
  (set-face-attribute mime-highlight-face 'background "turquoise")
  (set-face-attribute mime-highlight-face 'underline t))


;; Parsing Content- headers

;; Returns (TYPE SUBTYPE [(PARAM . VALUE-STRING) ... ])
(defun mime-decode-content-type (text)
  (let
      (type subtype point params)
    (unless (string-looking-at (concat "[ \t\n]*([a-zA-Z0-9-]+)[ \t\n]*/"
				       "[ \t\n]*([a-zA-Z0-9-]+)[ \t\n]*") text)
      (error "Malformed Content-type header"))
    (setq type (intern (translate-string (expand-last-match "\\1")
					 downcase-table)))
    (setq subtype (intern (translate-string (expand-last-match "\\2")
					    downcase-table)))
    (setq point (match-end))
    (while (string-looking-at (concat ";[ \t\n]*([a-zA-Z0-9-]+)[ \t\n]*="
				      "[ \t\n]*") text point)
      (let
	  ((param-name (intern (translate-string (expand-last-match "\\1")
						 downcase-table)))
	   param-body)
	(setq point (match-end))
	(when (string-looking-at (if (= (aref text point) ?\")
				     "\"([^\"]*)\"[ \t\n]*"
				   "([a-zA-Z0-9-]+)")
				 text point)
	  (setq param-body (expand-last-match "\\1"))
	  (setq point (match-end)))
	(setq params (cons (cons param-name param-body) (nreverse params)))))
    (list* type subtype params)))

;; Returns a symbol
(defun mime-decode-content-xfer-enc (text)
  (unless (string-looking-at "[ \t\n]*([a-zA-Z0-9-]+)" text)
    (error "Malformed Content-transfer-encoding header"))
  (intern (translate-string (expand-last-match "\\1") downcase-table)))


;; Decoder-engine

;; Decode from the start of SRC-BUFFER to the cursor in the current buffer.
;; CONTENT-TYPE is the parsed content type of the message. CONTENT-XFER-ENC
;; likewise for the encoding.
(defun mime-decode (src-buffer content-type content-xfer-enc)
  (let
      ((start (cursor-pos))
       tem)
    ;; Switch on content types
    (cond
     ((eq (car content-type) 'text)
      ;; Some sort of text
      (insert "\n")
      (if (setq tem (assq content-xfer-enc mime-xfer-encodings-alist))
	  (progn
	    (require (nth 1 tem))
	    (funcall (nth 3 tem) src-buffer (current-buffer)))
	;; No decoder, so just copy
	(insert (copy-area (start-of-buffer src-buffer)
			   (end-of-buffer src-buffer)
			   src-buffer)))
;      (when (eq (nth 1 content-type) 'richtext)
;	(mime-decode-richtext start (cursor-pos)))
      )
     ((eq (car content-type) 'message)
      ;; Mail/news message
      (if (eq (nth 1 content-type) 'external-body)
	  ;; XXX: FIXME
	  (error "message/external-body as yet unsupported")
	(insert "\n")
	(if (setq tem (assq content-xfer-enc mime-xfer-encodings-alist))
	    (progn
	      (require (nth 1 tem))
	      (funcall (nth 3 tem) src-buffer (current-buffer)))
	  ;; No decoder, so just copy
	  (insert (copy-area (start-of-buffer src-buffer)
			     (end-of-buffer src-buffer)
			     src-buffer)))))
     ((eq (car content-type) 'multipart)
      ;; Recursive embedded message
      (let
	  ((boundary (cdr (assq 'boundary (nthcdr 2 content-type))))
	   (actual-src src-buffer)
	   parts start end last)
	(unless (stringp boundary)
	  (error "No boundary parameter in multipart message"))
	(setq boundary (concat "--" boundary))
	(when (setq tem (assq content-xfer-enc mime-xfer-encodings-alist))
	  ;; message is encoded, so decode it to a temporary buffer
	  (setq actual-src (make-buffer "*mime-decode-temp*"))
	  (require (nth 1 tem))
	  (funcall (nth 3 tem) src-buffer actual-src))
	(setq start (search-forward boundary
				    (start-of-buffer actual-src)
				    actual-src))
	(unless start
	  (error "Can't find first boundary in multipart message"))
	(setq start (forward-line 1 start))
	(while (and (not last)
		    (setq end (search-forward boundary start actual-src)))
	  (let
	      (local-type local-enc)
	    (when (looking-at "--" (match-end) actual-src)
	      (setq last t))
	    (with-buffer actual-src
	      (save-restriction
		(restrict-buffer start (forward-char -1 end))
		;; parse headers of this part
		(setq tem (start-of-buffer))
		(while (not (looking-at "^[ \t]*$" tem))
		  (let
		      ((hdr-end (mail-unfold-header tem)))
		    (cond
		     ((looking-at "content-type[ \t\n]*:[ \t\n]*" tem nil t)
		      (setq local-type (mime-decode-content-type
					(copy-area (match-end) hdr-end))))
		     ((looking-at "content-transfer-encoding[ \t\n]*:[ \t\n]*"
				  tem nil t)
		      (setq local-enc (mime-decode-content-xfer-enc
				       (copy-area (match-end) hdr-end)))))
		    (setq tem hdr-end)))))
	    (setq parts (cons (list* (forward-line 1 tem) end
				     local-enc local-type) parts))
	    (setq start (forward-line 1 end))))
	;; PARTS is a list of (START END ENCODING TYPE...)
	;; (in reverse order compared to the message)
	(if (eq (nth 1 content-type) 'alternative)
	    ;; Need to choose which of the alternative encodings
	    ;; to display. They should have been in the order
	    ;; most specific to least specific. For now just
	    ;; choose the last
	    ;; XXX: FIXME
	    (error "multipart/alternative messages unsupported as yet")
	  ;; Display all parts of the message
	  (setq parts (nreverse parts))
	  (while parts
	    (let
		((start (nth 0 (car parts)))
		 (end (nth 1 (car parts)))
		 (enc (nth 2 (car parts)))
		 (type (nthcdr 3 (car parts)))
		 (dest (current-buffer)))
	      (unless type
		;; implicit type of part
		(setq type (if (eq (nth 1 content-type) 'digest)
			       (list 'message 'rfc822)
			     (list 'text 'plain))))
	      (with-buffer actual-src
		(save-restriction
		  (restrict-buffer start (forward-char -1 end))
		  (with-buffer dest
		    ;; Recursively decode each part
		    (mime-decode actual-src type enc))))
	      (setq parts (cdr parts)))))))
     ;; end of multipart
     (t
      ;; non-inline-displayable types (application, audio, image, video, ...)
      ;; XXX: for now, just output some comment...
      (format (current-buffer) "\n\t%s/%s params: %s (%s encoding)\n\n"
	      (car content-type) (nth 1 content-type) (nthcdr 2 content-type)
	      content-xfer-enc)
      (setq tem (forward-char 2 start))
      (make-extent tem (end-of-line tem)
		   (list 'face mime-highlight-face
			 'content-type content-type
			 'content-xfer-enc content-xfer-enc
			 'buffer src-buffer
			 'start (start-of-buffer src-buffer)
			 'end (end-of-buffer src-buffer)))))))


;; Manipulating non-inlined parts of messages

(defun mime-save-part (extent &optional file-name)
  (interactive (list (let
			 ((e (get-extent)))
		       (while (and e (not (extent-get e 'content-type)))
			 (setq e (extent-parent e)))
		       (unless e
			 (error "No part to save here"))
		       e)))
  (unless file-name
    ;; FIXME: should give a default
    (setq file-name (prompt-for-file "Save to file:")))
  (with-buffer (extent-get extent 'buffer)
    (save-restriction
      (unrestrict-buffer)
      (let*
	  ((content-xfer-enc (extent-get extent 'content-xfer-enc))
	   (tem (assq content-xfer-enc mime-xfer-encodings-alist)))
	(if tem
	    ;; Use a decoder
	    (progn
	      (require (nth 1 tem))
	      (restrict-buffer (extent-get extent 'start)
			       (extent-get extent 'end))
	      (let
		  ((file (open-file file-name 'write)))
		(unwind-protect
		    (funcall (nth 3 tem)
			     (cons (current-buffer) (start-of-buffer)) file)
		  (close-file file))))
	  ;; otherwise just save normally
	  (write-buffer-contents file-name
				 (extent-get extent 'start)
				 (extent-get extent 'end)))))))

(defun mime-decode-part (extent)
  (interactive (list (let
			 ((e (get-extent)))
		       (while (and e (not (extent-get e 'content-type)))
			 (setq e (extent-parent e)))
		       (unless e
			 (error "No part to decode here"))
		       e)))
  (let*
      ((content-type (extent-get extent 'content-type))
       (content-xfer-enc (extent-get extent 'content-xfer-enc))
       (viewer (cdr (assq (car content-type) mime-viewer-alist))))
    (cond
     ((functionp viewer)
      (funcall viewer extent))
     ((stringp viewer)
      (let*
	  ((file (make-temp-name))
	   (args (cons file nil)))
	(mime-save-part extent file)
	(rplacd args args)
	(message "Calling external viewer..." t)
	(shell-command (apply 'format nil viewer args))))
     (t
      (mime-save-part extent)))))


;; Moving through MIME parts in buffers

(defun mime-next-part (count)
  (interactive "p")
  (while (> count 0)
    (let
	((start (cursor-pos))
	 tem)
      (setq tem (get-extent))
      (while (and tem (not (extent-get tem 'content-type)))
	(setq tem (extent-parent tem)))
      (when tem
	(setq start (extent-end tem)))
      (goto (catch 'foo
	      (map-extents #'(lambda (e)
			       (when (extent-get e 'content-type)
				 (throw 'foo (extent-start e))))
			   start (end-of-buffer)))))
    (setq count (1- count)))
  (while (< count 0)
    (let
	((end (cursor-pos))
	 tem)
      (setq tem (get-extent))
      (while (and tem (not (extent-get tem 'content-type)))
	(setq tem (extent-parent tem)))
      (when tem
	(setq end (forward-char -1 (extent-start tem))))
      (goto (catch 'foo
	      (map-extents #'(lambda (e)
			       (when (extent-get e 'content-type)
				 (throw 'foo (extent-start e))))
			   (start-of-buffer) end))))
    (setq count (1+ count))))

(defun mime-previous-part (count)
  (interactive "p")
  (mime-next-part (- count)))
