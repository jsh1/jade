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

;; TODO:
;;
;; * When running an external viewer there's no way to safely delete the
;;   temporary file, the viewer must do it itself. The viewer process
;;   should be run asynchronously..
;; * Various MIME types aren't supported: messages with external-body's
;; * Prettify text/enriched and text/richtext as much as possible
;; * Do something with text/html
;; * Replace the mime-viewer-alist with something that has finer
;;   granularity, i.e. by subtype (look in .mailcap?)
;; * Should process embedded messages if they're MIME

(require 'maildefs)
(require 'mail-headers)
(provide 'mime-decode)


;; Configuration

(defvar mime-decode-mark-inlines nil
  "When non-nil multipart pieces of messages that are displayed inline are
still given a highlighted header.")

(defvar mime-xfer-encodings-alist
  '((base64 mime-base64 mime-encode-base64 mime-decode-base64)
    (quoted-printable mime-quoted-printable
		      mime-encode-quoted-printable
		      mime-decode-quoted-printable))
  "Alist of (ENCODING FEATURE ENCODER DECODER) where ENCODER and DECODER are
functions that operate as filters on their argument streams.")

(defvar mime-viewer-alist
  '((image . "( xview %s ; rm -f %s ) >/dev/null 2>&1 </dev/null &"))
  "Alist of (TYPE . COMMAND)")

(defface mime-highlight-face "Face to highlight MIME stubs."
  (set-face-attribute mime-highlight-face 'background "turquoise")
  (set-face-attribute mime-highlight-face 'underline t))

(defconst mime-token-re "[^][()<>@,;:\\\"/?=\001-\037 \t\177]+")


;; Parsing Content- headers

(defun mime-decode-params (text &optional point)
  (let
      (params)
    (while (string-looking-at (concat ";[ \t\n]*(" mime-token-re
				      ")[ \t\n]*=" "[ \t\n]*")
			      text point)
      (let
	  ((param-name (intern (translate-string (expand-last-match "\\1")
						 downcase-table)))
	   param-body)
	(setq point (match-end))
	(when (string-looking-at (if (= (aref text point) ?\")
				     "\"([^\"]*)\"[ \t\n]*"
				   (concat "(" mime-token-re ")"))
				 text point)
	  (setq param-body (expand-last-match "\\1"))
	  (setq point (match-end)))
	(setq params (cons (cons param-name param-body) params))))
    (nreverse params)))

;; Returns (TYPE SUBTYPE [(PARAM . VALUE-STRING) ... ])
(defun mime-decode-content-type (text)
  (let
      (type subtype params)
    (unless (string-looking-at (concat "[ \t\n]*(" mime-token-re
				       ")[ \t\n]*(/[ \t\n]*(" mime-token-re
				       ")[ \t\n]*)?") text)
      (error "Malformed Content-type header"))
    (setq type (intern (translate-string (expand-last-match "\\1")
					 downcase-table)))
    (when (match-start 2)
      (setq subtype (intern (translate-string (expand-last-match "\\3")
					      downcase-table))))
    (list* type subtype (mime-decode-params text (match-end)))))

;; Returns a symbol
(defun mime-decode-content-xfer-enc (text)
  (unless (string-looking-at (concat "[ \t\n]*(" mime-token-re ")") text)
    (error "Malformed Content-transfer-encoding header"))
  (intern (translate-string (expand-last-match "\\1") downcase-table)))

;; Returns (inline PARAMS...) or (attachment PARAMS...) where the only
;; thing possible in the PARAMS alist is a filename
(defun mime-decode-content-disp (text)
  (unless (string-looking-at (concat "[ \t\n]*(" mime-token-re
				     ")[ \t\n]*") text)
    (error "Malformed Content-disposition header"))
  (let
      ((type (intern (translate-string (expand-last-match "\\1")
				       downcase-table)))
       (params (when (and (> (length text) (match-end))
			  (= (aref text (match-end)) ?\;))
		 (mime-decode-params text (match-end)))))
    (cons type params)))


;; Decoder-engine

;; For the mime part defined by the arguments, insert an extent marking
;; its presence. The extent will contain all the arguments in its plist
(defun mime-decode-insert-stub (buffer start end
				&optional content-type content-xfer-enc
				content-disp)
 (let
     ((out-start (cursor-pos))
      (filename (cdr (or (assq 'filename (cdr content-disp))
			 ;; seems some mailers (sun dtmail) put the filename
			 ;; in the content-type parameters!?
			 (assq 'name (nthcdr 2 content-type)))))
      tem)
   (insert "[[ ")
   (when content-disp
     (format (current-buffer) "%s: " (car content-disp)))
   (when filename
     (format (current-buffer) "filename=%s " filename))
   (format (current-buffer) "%s/%s " (car content-type) (nth 1 content-type))
;  (when (nth 2 content-type)
;    (format (current-buffer) "params: %S " (nth 2 content-type)))
   (format (current-buffer) "(%s encoding) " content-xfer-enc)
   (insert "]]\n")
   (setq tem (forward-char 2 out-start))
   (make-extent tem (forward-char -2 (end-of-line tem))
		(list 'face mime-highlight-face
		      'content-type content-type
		      'content-xfer-enc content-xfer-enc
		      'content-disp content-disp
		      'buffer buffer
		      'start start
		      'end end))))

;; Decode all of SRC-BUFFER to the stream OUTPUT, according to the mime
;; content-transfer-encoding ENCODING (a symbol)
(defun mime-decode-buffer (encoding src-buffer output)
  (let
      ((tem (assq encoding mime-xfer-encodings-alist)))
    (if (null tem)
	;; Just copy verbatim
	(write output (copy-area (start-of-buffer src-buffer)
				 (end-of-buffer src-buffer)
				 src-buffer))
      ;; apply the decoder
      (when (nth 1 tem)
	(require (nth 1 tem)))
      (funcall (nth 3 tem)
	       (cons src-buffer (start-of-buffer src-buffer)) output))))

;; Decode from the start of SRC-BUFFER to the cursor in the current buffer.
;; CONTENT-TYPE is the parsed content type of the message. CONTENT-XFER-ENC
;; likewise for the encoding.
(defun mime-decode (src-buffer content-type
		    &optional content-xfer-enc content-disp in-multipart)
  (let
      (tem)
    ;; Switch on content types
    (cond
     ((eq (car content-type) 'text)
      ;; Some sort of text
      (when (or (and content-disp (eq (car content-disp) 'attachment))
		(and in-multipart mime-decode-mark-inlines))
	(mime-decode-insert-stub src-buffer
				 (start-of-buffer src-buffer)
				 (end-of-buffer src-buffer)
				 content-type content-xfer-enc content-disp))
      (when (or (null content-disp)
		(eq (car content-disp) 'inline))
	(mime-decode-buffer content-xfer-enc src-buffer (current-buffer))
	(insert "\n")))
     ((eq (car content-type) 'message)
      ;; Mail/news message
      (when (or (and content-disp (eq (car content-disp) 'attachment))
		(and in-multipart mime-decode-mark-inlines))
	(mime-decode-insert-stub src-buffer
				 (start-of-buffer src-buffer)
				 (end-of-buffer src-buffer)
				 content-type content-xfer-enc content-disp))
      (when (or (null content-disp)
		(eq (car content-disp) 'inline))
	(if (eq (nth 1 content-type) 'external-body)
	    ;; XXX: FIXME
	    (error "message/external-body as yet unsupported")
	  (mime-decode-buffer content-xfer-enc src-buffer (current-buffer))
	  (insert "\n"))))
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
	  (mime-decode-buffer content-xfer-enc src-buffer actual-src))
	(setq start (search-forward boundary
				    (start-of-buffer actual-src)
				    actual-src))
	(unless start
	  (error "Can't find first boundary in multipart message"))
	(setq start (forward-line 1 start))
	(while (and (not last)
		    (setq end (search-forward boundary start actual-src)))
	  (let
	      (local-type local-enc local-disp)
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
				       (copy-area (match-end) hdr-end))))
		     ((looking-at "content-disposition[ \t\n]*:[ \t\n]*"
				  tem nil t)
		      (setq local-disp (mime-decode-content-disp
					(copy-area (match-end) hdr-end)))))
		    (setq tem hdr-end)))))
	    (setq parts (cons (list (forward-line 1 tem) end
				     local-enc local-type local-disp) parts))
	    (setq start (forward-line 1 end))))
	;; PARTS is a list of (START END ENCODING TYPE DISP)
	;; (in reverse order compared to the message)
	(if (eq (nth 1 content-type) 'alternative)
	    ;; Need to choose which of the alternative encodings
	    ;; to display. They should have been in the order
	    ;; most specific to least specific
	    (setq parts
		  (catch 'foo
		    (mapc #'(lambda (part)
			      (let
				  ((type (nth 3 part)))
				(when (or (eq (car type) 'multipart)
					  (eq (car type) 'message)
					  (and (eq (car type) 'text)
					       (memq (nth 1 type)
						     '(plain enriched
						       richtext))))
				  (throw 'foo (list part))))) parts)
		    nil))
	  (setq parts (nreverse parts)))
	(while parts
	  (let
	      ((start (nth 0 (car parts)))
	       (end (nth 1 (car parts)))
	       (enc (nth 2 (car parts)))
	       (type (nth 3 (car parts)))
	       (disp (nth 4 (car parts)))
	       (dest (current-buffer)))
	    (when (> end start)
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
		    (mime-decode actual-src type enc disp t)))))
	    (setq parts (cdr parts))))))
     ;; end of multipart
     (t
      ;; non-inline-displayable types (application, audio, image, video, ...)
      (unless in-multipart
	(insert "\n"))
      (mime-decode-insert-stub src-buffer
			       (start-of-buffer src-buffer)
			       (end-of-buffer src-buffer)
			       content-type content-xfer-enc content-disp)))))


;; Manipulating embedded parts of messages

(defun mime-current-part (&optional no-error)
  (let
      ((e (get-extent)))
    (while (and e (not (extent-get e 'content-type)))
      (setq e (extent-parent e)))
    (unless (or e no-error)
      (error "No MIME part here!"))
    e))

(defun mime-save-part (extent &optional file-name)
  "Save the MIME part of the message marked by EXTENT to the file FILE-NAME.
When called interactively, the MIME part under the cursor is used, and
FILE-NAME is prompted for."
  (interactive (list (mime-current-part)))
  (unless file-name
    (setq file-name (prompt-for-file
		     "Save attachment to file:" nil
		     (cdr (or (assq 'filename (cdr (extent-get extent
							       'content-disp)))
			      (assq 'name (cdr (extent-get
						extent 'content-type))))))))
  (with-buffer (extent-get extent 'buffer)
    (save-restriction
      (unrestrict-buffer)
      (let*
	  ((content-xfer-enc (extent-get extent 'content-xfer-enc))
	   (tem (assq content-xfer-enc mime-xfer-encodings-alist)))
	(if tem
	    ;; Use a decoder
	    (let
		((file (open-file file-name 'write)))
	      (unwind-protect
		  (progn
		    (restrict-buffer (extent-get extent 'start)
				     (extent-get extent 'end))
		    (goto (start-of-buffer))
		    (mime-decode-buffer content-xfer-enc
					(current-buffer) file))
		(close-file file)))
	  ;; otherwise just save normally
	  (write-buffer-contents file-name
				 (extent-get extent 'start)
				 (extent-get extent 'end)))))))

(defun mime-decode-part (extent)
  "Attempt to view the MIME part of the message marked by EXTENT. If there is
no known viewing method, use mime-save-part to save it to a file. When called
interactively the MIME part under the cursor is used."
  (interactive (list (mime-current-part)))
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

(defun mime-display-part (extent)
  "Display the unencoded form of the current MIME part in a temporary buffer."
  (interactive (list (mime-current-part)))
  (let
      ((buffer (make-buffer (or (cdr (assq 'filename
					   (cdr (extent-get extent
							    'content-disp))))
				(cdr (assq 'name
					   (cdr (extent-get extent
							    'content-type))))
				"*decoded-mime-part*"))))
    (with-buffer (extent-get extent 'buffer)
      (save-restriction
	(unrestrict-buffer)
	(let*
	    ((content-xfer-enc (extent-get extent 'content-xfer-enc))
	     (tem (assq content-xfer-enc mime-xfer-encodings-alist)))
	  (restrict-buffer (extent-get extent 'start)
			   (extent-get extent 'end))
	  (goto (start-of-buffer))
	  (mime-decode-buffer content-xfer-enc (current-buffer) buffer))))
    (with-view (other-view)
      (goto-buffer buffer)
      (goto (start-of-buffer)))))


;; Moving through MIME parts in buffers

(defun mime-next-part (count)
  "Move to the COUNT'th next MIME part in the current message."
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
      (goto (let
		(last)
	      (map-extents #'(lambda (e)
			       (when (extent-get e 'content-type)
				 (setq last (extent-start e))))
			   (start-of-buffer) end)
	      last)))
    (setq count (1+ count))))

(defun mime-previous-part (count)
  "Move to the COUNT'th previous MIME part in the current message."
  (interactive "p")
  (mime-next-part (- count)))
