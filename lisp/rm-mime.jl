;;;; rm-mime.jl -- Interface MIME decoder to mail reader
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

;; Commentary:
;;
;; The easiest way to enable MIME decoding for messages that need it is
;; to put the form:
;;
;;	(eval-after-load "read-mail" '(require 'rm-mime))
;;
;; in your .jaderc file (or the system-wide site-init.jl).

(require 'read-mail)
(require 'mail-headers)
(provide 'rm-mime)

;; Called from the rm-display-message-hook
(defun rm-mime-display-msg-function (msg folder)
  (when (rm-get-msg-header msg "MIME-Version")
    ;; A MIME message, look for content-type, content-transfer-encoding,
    ;; and content-disposition headers
    (let
	((content-type (rm-get-msg-header msg "Content-Type"))
	 (content-xfer-enc (rm-get-msg-header
			    msg "Content-Transfer-Encoding"))
	 (content-disp (rm-get-msg-header msg "Content-Disposition")))
      (require 'mime-decode)
      (setq content-type (if content-type
			     (mime-decode-content-type content-type)
			   (list 'text 'plain)))
      (setq content-xfer-enc (and content-xfer-enc
				  (mime-decode-content-xfer-enc
				   content-xfer-enc)))
      (setq content-disp (and content-disp
			      (mime-decode-content-disp content-disp)))
      (unless (and (null (assq content-xfer-enc mime-xfer-encodings-alist))
		   (eq (car content-type) 'text)
		   (eq (nth 1 content-type) 'plain))
	;; Something nonstandard, decode it to a temporary buffer
	;; On return from the hook read-mail will notice the new current
	;; buffer and assume that it contains an updated version of the
	;; message
	(let
	    ((dest (make-buffer "*decoded-mime-message*")))
	  ;; First copy all visible headers
	  (insert (copy-area (restriction-start)
			     (rm-message-body msg)) nil dest)
	  (save-restriction
	    (restrict-buffer (rm-message-body msg)
			     (rm-message-end msg))
	    (goto-buffer dest)
	    ;; Re-highlight the copied headers
	    (let
		((tem (start-of-buffer))
		 (top (cursor-pos)))
	      (while (re-search-forward mail-highlighted-headers tem nil t)
		(when (looking-at "^[^:]+:[\t ]*" (match-start))
		  (let
		      ((end (match-end)))
		    (setq tem (forward-char -1 (or (mail-unfold-header
						    (match-end))
						   (end-of-buffer))))
		    (make-extent end tem (list 'face mail-highlight-face)))))
	      (mime-decode (mark-file (rm-get-msg-field msg rm-msg-mark))
			   content-type content-xfer-enc content-disp)
	      (goto top)
	      (center-display nil -1))))))))

(add-hook 'rm-display-message-hook 'rm-mime-display-msg-function)

;; Extra bindings in read-mail folders
(bind-keys rm-keymap
  "RET" 'mime-decode-part
  "C-w" 'mime-save-part
  "TAB" 'mime-next-part
  "M-TAB" 'mime-previous-part)
