;;;; send-mail.jl -- Package for sending mail messages
;;;  Copyright (C) 1997 John Harper <john@dcs.warwick.ac.uk>
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
(provide 'send-mail)

;;;; Code

(defvar mail-buffer-in-use nil)

;; List of (FUNCTION . ARGS) to call when message is finally sent.
(defvar mail-send-actions nil)

;;;###autoload
(defun mail-setup (to &optional subject in-reply-to cc references actions)
  "Initialises a buffer in which a mail message may be composed, prior to
being sent."
  (let
      ((buffer (or (get-buffer "*mail*")
		   (make-buffer "*mail*"))))
    (clear-buffer buffer)
    (set-buffer-special buffer nil)
    (goto-buffer buffer)
    (when mail-buffer-in-use
      (if (y-or-n-p "Okay to lose contents of *mail* buffer?")
	  (clear-buffer)
	(error "Mail buffer in use")))
    (set-buffer-read-only buffer nil)
    (setq mail-buffer-in-use t
	  mail-send-actions actions)
    (format buffer "To: %s\n" to)
    (when cc
      (format buffer "CC: %s\n" cc))
    (when in-reply-to
      (format buffer "In-reply-to: %s\n" in-reply-to))
    (when references
      (format buffer "References: %s\n" references))
    (format buffer "Subject: %s\n" (or subject ""))
    (when mail-default-headers
      (insert mail-default-headers)
      (when (/= (pos-col (cursor-pos)) 0)
	(insert "\n")))
    (when mail-default-reply-to
      (format buffer "Reply-to: %s\n" mail-default-reply-to))
    (when mail-self-blind
      (format buffer "BCC: %s\n" user-mail-address))
    (when mail-archive-file-name
      (format buffer "FCC: %s\n" mail-archive-file-name))
    (insert mail-header-separator)
    (insert "\n")
    (when mail-signature
      (let
	  ((old (cursor-pos)))
	(insert "\n\n--\n")
	(if (eq mail-signature t)
	    (when (and mail-signature-file
		       (file-exists-p (expand-file-name mail-signature-file)))
	      (insert-file (expand-file-name mail-signature-file)))
	  (insert mail-signature))
	(when (/= (pos-col (cursor-pos)) 0)
	  (insert "\n"))
	(goto-char old)))
    (set-buffer-modified buffer nil)
    (setq buffer-undo-list nil)
    (mail-mode)))


;; Mail mode

(defvar mail-c-keymap (make-keylist))
(bind-keys mail-c-keymap
  "Ctrl-c" 'mail-send-and-exit
  "Ctrl-s" 'mail-send
  "Ctrl-f" '(setq next-keymap-path '(mail-c-f-keymap))
  "Ctrl-t" 'mail-go-text
  "Ctrl-w" 'mail-signature
  "Ctrl-y" 'mail-yank-original
  "Ctrl-q" 'mail-fill-yanked-message)
(defvar mail-c-f-keymap (make-keylist))
(bind-keys mail-c-f-keymap
  "Ctrl-t" 'mail-go-to
  "Ctrl-s" 'mail-go-subject
  "Ctrl-c" 'mail-go-cc
  "Ctrl-b" 'mail-go-bcc
  "Ctrl-f" 'mail-go-fcc)

(defun mail-mode ()
  "Mail Mode:\n
Major mode for composing and sending mail messages."
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "Mail"
	major-mode 'mail-mode
	major-mode-kill 'mail-mode-kill
	ctrl-c-keymap mail-c-keymap)
  ;; Need to turn on autosaving and associate the buffer with a
  ;; temporary file...
  (eval-hook 'mail-setup-hook))

(defun mail-mode-kill ()
  (setq major-mode nil
	major-mode-kill nil
	ctrl-c-keymap nil))

(defun mail-go-text ()
  "Put the cursor at the start of the message body."
  (interactive)
  (if (find-next-regexp (concat ?^ (regexp-quote mail-header-separator) ?$)
			(buffer-start))
      (goto-char (next-line 1 (match-start)))
    (error "No mail-header-separator in message")))

(defun mail-find-header (header)
  (if (find-next-regexp (concat ?^ header "[\t ]*:[\t ]*")
			(buffer-start) nil t)
      (goto-char (match-end))
    (unless (find-next-regexp
	     (concat ?^ (regexp-quote mail-header-separator) ?$)
	     (buffer-start))
      (error "Can't find header separator"))
    (goto-char (match-start))
    (insert (concat header ": \n"))
    (goto-prev-char)))
    
(defun mail-go-to ()
  "Move to the message's To: header."
  (interactive)
  (mail-find-header "To"))

(defun mail-go-subject ()
  "Move to the message's Subject: header."
  (interactive)
  (mail-find-header "Subject"))

(defun mail-go-cc ()
  "Move to the message's CC: header."
  (interactive)
  (mail-find-header "CC"))

(defun mail-go-bcc ()
  "Move to the message's BCC: header."
  (interactive)
  (mail-find-header "BCC"))

(defun mail-go-fcc ()
  "Move to the message's FCC: header."
  (interactive)
  (mail-find-header "FCC"))

(defun mail-signature ()
  "Insert the contents of the mail-signature-file as the message's signature."
  (interactive)
  (if (and mail-signature-file
	   (file-exists-p (expand-file-name mail-signature-file)))
      (let
	  ((pos (find-prev-string "\n\n--\n" (buffer-end)))
	   (old-pos (cursor-pos)))
	(if pos
	    (delete-area pos (buffer-end))
	  (goto-buffer-end)
	  (insert "\n\n--\n"))
	(insert-file (expand-file-name mail-signature-file))
	(when (/= (pos-col (cursor-pos)) 0)
	  (insert "\n"))
	(when (<= old-pos (buffer-end))
	  (goto-char old-pos)))
    (error "No signature file to insert")))

(defun mail-send ()
  "Send the mail message in the current buffer."
  (interactive)
  (when (or (buffer-read-only-p)
	    (and (string= (buffer-name) "*mail*") (not mail-buffer-in-use)))
    (unless (y-or-n-p "Really send this buffer as a mail message?")
      (error "Quit")))
  (message "Sending..." t)
  (eval-hook mail-send-hook)
  (funcall mail-send-function)
  (format t "done")
  (set-buffer-read-only (current-buffer) t)
  (set-buffer-modified (current-buffer) nil)
  (while mail-send-actions
    (apply (car (car mail-send-actions)) (cdr (car mail-send-actions)))
    (setq mail-send-actions (cdr mail-send-actions)))
  (setq mail-buffer-in-use nil)
  t)

(defun mail-send-and-exit ()
  "Send the mail message in the current buffer and bury the buffer."
  (interactive)
  (when (mail-send)
    (bury-buffer)))

(defun sendmail-send-message ()
  "Use sendmail to send the message in the current buffer."
  (let
      ((header-end (find-next-regexp
		    (concat ?^ (regexp-quote mail-header-separator) ?$)
		    (buffer-start) nil t))
       (resent-addresses '())
       tem)
    (unless (find-next-regexp (concat ?^
				      (regexp-quote mail-header-separator ?$)
				      ?$)
			      (buffer-start))
      (error "Can't find header-separator string"))
    ;; Delete the header separator and restrict to the headers
    (delete-area (match-start) (match-end))
    (restrict-buffer (buffer-start) (prev-line 1 (match-start)))

    ;; First, insert From: unless it's already there.
    (if (find-next-regexp "^From[\t ]*:[\t ]*" (buffer-start) nil t)
	(goto-char (match-end))
      (goto-char (buffer-start))
      (insert "From: \n")
      (goto-prev-char))
    (when (looking-at "[\t ]*$")
      ;; Need to handle quoting full name a la RFC-822
      (cond
       ((eq mail-address-style 'angles)
	(insert (concat (user-full-name) " <" user-mail-address ?\>)))
       ((eq mail-address-style 'parens)
	(insert (concat user-mail-address " \(" (user-full-name) ?\))))
       (t
	(insert user-mail-address))))

    ;; Remove blank lines
    (setq tem (buffer-start))
    (while (find-next-regexp "^\n" tem)
      (delete-area (match-start) (match-end))
      (setq tem (match-start)))

    ;; Remove blank headers
    (setq tem (buffer-start))
    (while (and tem (find-next-regexp (concat mail-header-name
					      "([\t ]*\n)+[^ \t]")
				      tem nil t))
      (setq tem (mail-delete-header (match-start))))

    ;; Handle any FCC fields
    ;; TODO: make sure that ^From is changed to >From
    (setq tem (buffer-start))
    (while (and tem
		(find-next-regexp "^FCC[\t ]*:[\t ]*([^\t\n\f ]+)" tem nil t))
      (let
	  ((filename (copy-area (match-start 1) (match-end 1)))
	   file)
	(setq tem (mail-delete-header (match-start)))
	(unwind-protect
	    (progn
	      (setq file (open (expand-file-name filename) "a"))
	      (unless (zerop (file-size filename))
		(write file "\n\n"))
	      ;; Need timezone as well
	      (format file "From %s %s\n" (user-login-name)
		      (current-time-string))
	      (write-buffer file))
	  (close file))))

    ;; Handle Resent-X headers. Build a list of addresses the message
    ;; should be sent to and specify them on the command line, instead of
    ;; letting sendmail pick them out of the text
    (setq tem (buffer-start))
    (while (and tem (find-next-regexp "^Resent-(To|CC|BCC)[\t ]*:[\t ]*"
				      tem nil t))
      (setq tem (match-start)
	    resent-addresses (nconc resent-addresses (mail-parse-list tem)))
      (if (looking-at "^Resent-BCC" tem nil t)
	  ;; Delete Resent-BCC ourselves
	  (delete-area tem (mail-unfold-header tem))
	(setq tem (mail-unfold-header tem))))

    (unrestrict-buffer)

    ;; Now call sendmail to do it's stuff..
    (let*
	((temp-buffer (make-buffer "*sendmail-output*"))
	 (proc (make-process temp-buffer)))
      (apply 'call-process-area proc (buffer-start) (buffer-end) nil
	     (nconc (list (unless sendmail-program "/usr/lib/sendmail")
			  ;; Dot doesn't specify end-of-message
			  "-oi"
			  ;; From the user
			  "-f" (user-login-name)
			  ;; report errors by mail and deliver
			  ;; in the background
			  "-oem" "-odb")
		    ;; If we had Resent-X headers specify the addresses
		    ;; explicitly, otherwise tell sendmail to find the
		    ;; addresses itself
		    (or resent-addresses '("-t"))))
      (unless (zerop (process-exit-value proc))
	;; Errors. Display the buffer they were output to and throw
	;; an exception
	(with-view (other-view)
	  (goto-buffer temp-buffer)
	  (set-buffer-special temp-buffer t)
	  (goto-buffer-start))
	(error "sendmail couldn't send message"))
      ;; No errors
      (if (boundp 'sendmail-debug)
	  (with-view (other-view)
	    (goto-buffer temp-buffer)
	    (set-buffer-special temp-buffer t)
	    (goto-buffer-start))
	(destroy-buffer temp-buffer)))))
