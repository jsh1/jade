;;;; rm-msg-links.jl -- "Hypertext" links in message headers
;;;  Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
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

(require 'read-mail)
(provide 'rm-msg-links)

(defvar rm-msg-links-alist nil
  "Alist of (HEADER-RE . LINK-FUNCTION).")

(defvar rm-msg-links-face underline-face)

;; Called from the rm-display-message-hook
(defun rm-make-message-links (msg folder)
  (mapc (lambda (cell)
	  (let ((point (start-of-buffer)))
	    (while (re-search-forward (car cell) point nil t)
	      (setq point (forward-line 1 (match-start)))
	      ((cdr cell) (match-start) msg folder))))
	rm-msg-links-alist))

(defun rm-msg-links-make-extent (point)
  (looking-at (concat mail-header-name "[ \t]*") point)
  (make-extent (match-end) (forward-char
			    -1 (mail-unfold-header (match-end)))
	       (list 'face rm-msg-links-face
		     'mouse-face active-face)))

(defun rm-msg-links-addresses (point msg folder)
  (declare (unused msg))
  (save-restriction
    (restrict-buffer point (forward-char -1 (or (mail-unfold-header point)
						(end-of-buffer))))
    (when (looking-at (concat mail-header-name "[\t ]*") point)
      (setq point (match-end)))
    (let
	(tem addr extent)
      (while (setq tem (mail-parse-group point))
	;; address from POINT to (cdr TEM)
	(setq addr (mail-parse-address (apply concat (car tem))))
	(setq extent (make-extent point (cdr tem)
				  (list 'face rm-msg-links-face
					'mouse-face active-face)))
	(let
	    ((menus `(("Restrict to this address"
		       ,(lambda ()
			  (rm-restrict-to-address
			   (quote-regexp (car addr)) folder)))
		      ("Restrict to sent msgs"
		       ,(lambda ()
			  (rm-restrict-to-sender
			   (quote-regexp (car addr)) folder)))
		      ("Restrict to received msgs"
		       ,(lambda ()
			  (rm-restrict-to-recipient
			   (quote-regexp (car addr)) folder)))
		      ("Mail this address"
		       ,(lambda ()
			  (mail-setup (mail-format-address (car addr)
							   (cdr addr)))))
		      ("Add to address book"
		       ,(lambda ()
			  (add-mail-address (car addr) (cdr addr)))))))
	  (extent-put extent 'popup-menus menus))
	(setq point (if (looking-at "[\t\n ]*,[\t\n ]*" (cdr tem))
			(match-end)
		      (cdr tem)))))))
    
(defun rm-msg-links-subject (point msg folder)
  (let
      ((subject (rm-get-actual-subject msg))
       (extent (rm-msg-links-make-extent point)))
    (extent-put extent 'popup-menus
		`(("Restrict to this subject"
		   ,(lambda ()
		      (rm-restrict-to-subject
		       (quote-regexp subject) folder)))
		  ("Kill this subject"
		   ,(lambda ()
		      (rm-kill-subject)))))))

(defun rm-msg-links-date (point msg folder)
  (let
      ((date (rm-get-msg-header msg "Date"))
       (extent (rm-msg-links-make-extent point)))
    ;;; TODO: ranges of dates
    (extent-put extent 'popup-menus
		`(("Restrict to before this date"
		   ,(lambda ()
		      (rm-change-rule folder
				      (rule-lambda
				       () (list 'sent-before date)))))
		  ("Restrict to after this date"
		   ,(lambda ()
		      (rm-change-rule folder
				      (rule-lambda
				       () (list 'sent-after date)))))))))

(defun rm-msg-links-message-id (point msg folder)
  (declare (unused msg))
  (save-restriction
    (restrict-buffer point (forward-char -1 (or (mail-unfold-header point)
						(end-of-buffer))))
    (when (looking-at (concat mail-header-name "[\t ]*") point)
      (setq point (match-end)))
    (while (re-search-forward mail-message-id-re point)
      (setq point (match-end))
      (let*
	  ((id (copy-area (match-start) point))
	   (quoted-id (quote-regexp id))
	   (extent (make-extent (match-start) point
				(list 'face rm-msg-links-face
				      'mouse-face active-face))))
	(extent-put extent 'popup-menus
		    `(("Restrict to this id"
		       ,(lambda ()
			  (rm-restrict-to-message-id quoted-id folder)))
		      ("Display this message"
		       ,(lambda ()
			  (rm-display-message
			   folder
			   (or (rm-find-message-by-id folder id)
			       (error "No message %s" id)))))))))))

;; Add the function at the end of the hook, to guarantee it's after
;; the mime-decoder
(add-hook 'rm-display-message-hook rm-make-message-links t)

(setq rm-msg-links-alist
      (list (cons "^(resent-)?(from|to|cc|bcc|reply-to|sender):"
		  rm-msg-links-addresses)
	    (cons "^subject:"
		  rm-msg-links-subject)
	    (cons "^date:"
		  rm-msg-links-date)
	    (cons "^(message-id|references|in-reply-to):"
		  rm-msg-links-message-id)))
