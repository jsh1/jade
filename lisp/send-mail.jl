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
(require 'mail-headers)
(provide 'send-mail)

;;; Configuration:

(defvar send-mail-show-output nil
  "When non-nil all output from sendmail is displayed, even if the return
code states that the message was sent successfully.")

(defvar mail-send-hook nil
  "Hook called immediately prior to sending the mail message that has been
composed in the current buffer.")

(defvar mail-drafts-directory (expand-file-name "drafts" mail-folder-dir)
  "The directory used to store partially composed, but not yet sent, mail
messages.")

;;; Code:

;; List of (FUNCTION . ARGS) to call when message is finally sent.
(defvar send-mail-actions nil)
(make-variable-buffer-local 'send-mail-actions)

;;;###autoload
(defun mail-setup (&optional to subject in-reply-to cc references actions)
  "Initialises a buffer in which a mail message may be composed, prior to
being sent."
  (interactive)
  (send-mail-find-mail-buffer)
  (setq send-mail-actions actions)
  (insert "To: ")
  (cond
   ((stringp to)
    (insert to))
   ((consp to)
    (mail-insert-list to)))
  (insert "\n")
  (cond
   ((stringp cc)
    (format (current-buffer) "CC: %s\n" cc))
   ((consp cc)
    (insert "CC: ")
    (mail-insert-list cc)
    (insert "\n")))
  (format (current-buffer) "Subject: %s\n" (or subject ""))
  (when in-reply-to
    (format (current-buffer) "In-reply-to: %s\n" in-reply-to))
  (when references
    (insert "References: ")
    (mail-insert-list references t)
    (insert "\n"))
  (when mail-default-headers
    (insert mail-default-headers)
    (when (/= (pos-col (cursor-pos)) 0)
      (insert "\n")))
  (when mail-default-reply-to
    (format (current-buffer) "Reply-to: %s\n" mail-default-reply-to))
  (when mail-self-blind
    (catch 'no-bcc
      (when (stringp mail-self-blind)
	;; a regexp, only bcc if no recipient addresses match mail-self-blind
	(mapc (lambda (addr)
		(when (string-match mail-self-blind
				    (if (stringp addr) addr (car addr)))
		  (throw 'no-bcc t)))
	      (append (if (listp to) to (list to))
		      (if (listp cc) cc (list cc)))))
      ;; okay, so insert the bcc line
      (format (current-buffer) "BCC: %s\n" user-mail-address)))
  (when mail-archive-file-name
    (format (current-buffer) "FCC: %s\n" mail-archive-file-name))
  (goto (send-mail-insert-separator))
  (when mail-signature
    (let
	((old (cursor-pos)))
      (insert "\n\n-- \n")
      (if (eq mail-signature t)
	  (when (and mail-signature-file
		     (file-exists-p mail-signature-file))
	    (insert-file mail-signature-file))
	(insert mail-signature))
      (when (/= (pos-col (cursor-pos)) 0)
	(insert "\n"))
      (goto old)))
  (set-buffer-modified (current-buffer) nil)
  (set-buffer-undo-list nil)
  (send-mail-mode)
  (cond ((null to)
	 (send-mail-go-to))
	((null subject)
	 (send-mail-go-subject))
	(t
	 (send-mail-go-text))))


;; Mail mode

(defvar send-mail-c-keymap
  (bind-keys (make-sparse-keymap)
    "Ctrl-c" 'send-mail-send-and-exit
    "Ctrl-s" 'send-mail-send
    "Ctrl-d" 'send-mail-defer
    "Ctrl-f" 'send-mail-c-f-keymap
    "Ctrl-t" 'send-mail-go-text
    "Ctrl-w" 'send-mail-signature
    "Ctrl-m" 'mime-encode-keymap
    "Ctrl-y" 'mail-yank-original
    "Ctrl-q" 'mail-fill-yanked-message))

(defvar send-mail-c-f-keymap
  (bind-keys (make-sparse-keymap)
    "Ctrl-t" 'send-mail-go-to
    "Ctrl-s" 'send-mail-go-subject
    "Ctrl-c" 'send-mail-go-cc
    "Ctrl-b" 'send-mail-go-bcc
    "Ctrl-f" 'send-mail-go-fcc))

(defun send-mail-mode ()
  "Mail Mode:\n
Major mode for composing and sending mail messages. Local bindings are:\n
\\{send-mail-c-keymap,Ctrl-c}"
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "Mail"
	major-mode 'send-mail-mode
	major-mode-kill send-mail-mode-kill
	paragraph-separate (concat "^([\t\f ]|"
				   (quote-regexp mail-yank-prefix)
				   ?| (quote-regexp mail-header-separator)
				   ")*\n")
	paragraph-start (concat "^([\t\f ]|"
				(quote-regexp mail-yank-prefix)
				?| (quote-regexp mail-header-separator) ")*$")
	local-ctrl-c-keymap send-mail-c-keymap)
  ;; Need to turn on autosaving and associate the buffer with a
  ;; temporary file...
  (call-hook 'text-mode-hook)
  (call-hook 'mail-setup-hook))

(defun send-mail-mode-kill ()
  (setq major-mode nil
	major-mode-kill nil
	local-ctrl-c-keymap nil))

(defun send-mail-go-text ()
  "Put the cursor at the start of the message body."
  (interactive)
  (if (re-search-forward (concat ?^ (quote-regexp mail-header-separator) ?$)
			(start-of-buffer))
      (goto (forward-line 1 (match-start)))
    (error "No mail-header-separator in message")))

(defun send-mail-find-header (header)
  (if (re-search-forward (concat ?^ header "[\t ]*:[\t ]*")
			(start-of-buffer) nil t)
      (goto (match-end))
    (unless (re-search-forward
	     (concat ?^ (quote-regexp mail-header-separator) ?$)
	     (start-of-buffer))
      (error "Can't find header separator"))
    (goto (forward-char -1 (match-start)))
    (insert (concat ?\n header ": "))))
    
(defun send-mail-go-to ()
  "Move to the message's To: header."
  (interactive)
  (send-mail-find-header "To"))

(defun send-mail-go-subject ()
  "Move to the message's Subject: header."
  (interactive)
  (send-mail-find-header "Subject"))

(defun send-mail-go-cc ()
  "Move to the message's CC: header."
  (interactive)
  (send-mail-find-header "CC"))

(defun send-mail-go-bcc ()
  "Move to the message's BCC: header."
  (interactive)
  (send-mail-find-header "BCC"))

(defun send-mail-go-fcc ()
  "Move to the message's FCC: header."
  (interactive)
  (send-mail-find-header "FCC"))

(defun send-mail-signature ()
  "Insert the contents of the mail-signature-file as the message's signature."
  (interactive)
  (if (and mail-signature-file (file-exists-p mail-signature-file))
      (let
	  ((p (search-backward "\n\n-- \n" (end-of-buffer)))
	   (old-pos (cursor-pos)))
	(if p
	    (progn
	      (goto (match-end))
	      (delete-area (cursor-pos) (end-of-buffer)))
	  (goto (end-of-buffer))
	  (insert "\n\n-- \n"))
	(insert-file mail-signature-file)
	(when (/= (pos-col (cursor-pos)) 0)
	  (insert "\n"))
	(when (<= old-pos (end-of-buffer))
	  (goto old-pos)))
    (error "No signature file to insert")))

;; returns the beginning of the separator line
(defun send-mail-delete-separator ()
    (unless (re-search-forward (concat ?^
				      (quote-regexp mail-header-separator ?$)
				      ?$)
			      (start-of-buffer))
      (error "Can't find header-separator string"))
    ;; Delete the header separator
    (let
	((inhibit-read-only t)
	 (start (match-start))
	 (end (match-end)))
      (when (get-extent start)
	(delete-extent (get-extent start)))
      (delete-area start end)
      start))

(defun send-mail-find-mail-buffer ()
  (let
      (buffer)
    (if (setq buffer (get-buffer "*mail*"))
	(if (or (buffer-read-only-p buffer)
		(progn
		  (goto-buffer buffer)
		  (y-or-n-p "Okay to lose contents of *mail* buffer?")))
	    (progn
	      (set-buffer-read-only buffer nil)
	      (clear-buffer buffer)
	      (kill-all-local-variables buffer))
	  (error "Mail buffer in use"))
      (setq buffer (open-buffer "*mail*")))
    (goto-buffer buffer)))

;; add the separator into an existing message, returns the start of
;; the following line
(defun send-mail-insert-separator ()
  (let
      ((start (or (re-search-forward "^\s*$" (start-of-buffer))
		  (end-of-buffer)))
       end)
    (unless (zerop (pos-col start))
      (setq start (insert "\n" start)))
    (setq end (insert "\n" (insert mail-header-separator start)))
    ;; make the separator read-only
    (extent-set (make-extent start end) 'read-only t)
    end))

(defun send-mail-send ()
  "Send the mail message in the current buffer."
  (interactive)
  (when (or (not (string= (buffer-name) "*mail*"))
	    (buffer-read-only-p))
    (unless (y-or-n-p "Really send this buffer as a mail message?")
      (error "Quit")))
  (message "Sending..." t)
  (call-hook 'mail-send-hook)
  (funcall mail-send-function)
  (format t "done")
  (set-buffer-read-only (current-buffer) t)
  (set-buffer-modified (current-buffer) nil)
  (while send-mail-actions
    (apply (car (car send-mail-actions)) (cdr (car send-mail-actions)))
    (setq send-mail-actions (cdr send-mail-actions)))
  t)

(defun send-mail-send-and-exit ()
  "Send the mail message in the current buffer and bury the buffer."
  (interactive)
  (when (send-mail-send)
    (bury-buffer)))

(defun sendmail-send-message ()
  "Use sendmail to send the message in the current buffer."
  (let
      ((resent-addresses '())
       tem)
    ;; delete the separator
    (setq tem (send-mail-delete-separator))

    ;; restrict to the headers
    (restrict-buffer (start-of-buffer) (forward-line -1 tem))

    ;; First, insert From: unless it's already there.
    (if (re-search-forward "^From[\t ]*:[\t ]*" (start-of-buffer) nil t)
	(goto (match-end))
      (goto (start-of-buffer))
      (insert "From: \n")
      (goto (forward-char -1)))
    (when (looking-at "[\t ]*$")
      (insert (mail-format-address user-mail-address (user-full-name))))

    ;; Remove blank lines
    (setq tem (start-of-buffer))
    (while (re-search-forward "^\n" tem)
      (delete-area (match-start) (match-end))
      (setq tem (match-start)))

    ;; Remove blank headers
    (setq tem (start-of-buffer))
    (while (and tem (re-search-forward (concat mail-header-name
					      "([\t ]*\n)+[^ \t]")
				      tem nil t))
      (setq tem (mail-delete-header (match-start))))

    ;; Handle any FCC fields
    (setq tem (start-of-buffer))
    (while (and tem
		(re-search-forward "^FCC[\t ]*:[\t ]*([^\t\n\f ]+)" tem nil t))
      (let
	  ((filename (copy-area (match-start 1) (match-end 1)))
	   file)
	(setq tem (mail-delete-header (match-start)))
	(let
	    ((header-end (restriction-end))
	     tem quoted-froms)
	  ;; Mangle all ^From_ strings to >From_
	  (unrestrict-buffer)
	  (setq tem (start-of-buffer))
	  (while (re-search-forward "^From " tem)
	    (setq quoted-froms (cons (match-start) quoted-froms))
	    (insert ">" (match-start))
	    (setq tem (match-end)))
	  (unwind-protect
	      (progn
		(setq file (open-file filename 'append))
		(unless (zerop (file-size filename))
		  (write file "\n\n"))
		;; Need timezone as well
		(format file "From %s %s\n" (user-login-name)
			(current-time-string))
		(write-buffer-contents file))
	    (close-file file)
	    ;; Undo any ^From_ mangling
	    (mapc #'(lambda (p)
		      (delete-area p (forward-char 1 p))) quoted-froms)
	    (restrict-buffer (start-of-buffer) header-end)))))

    ;; Handle Resent-X headers. Build a list of addresses the message
    ;; should be sent to and specify them on the command line, instead of
    ;; letting sendmail pick them out of the text
    (setq tem (start-of-buffer))
    (while (and tem (re-search-forward "^Resent-(To|CC|BCC)[\t ]*:[\t ]*"
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
      (apply call-process-area proc (start-of-buffer) (end-of-buffer) nil
	     (nconc (list (or sendmail-program "/usr/lib/sendmail")
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
      ;; Reinsert the header-separator
      (when (re-search-forward "^[\t ]*$" (start-of-buffer))
	(let
	    ((inhibit-read-only t))
	  (insert mail-header-separator (match-start))))
      (unless (zerop (process-exit-value proc))
	;; Errors. Display the buffer they were output to and throw
	;; an exception
	(with-view (other-view)
	  (goto-buffer temp-buffer)
	  (goto (start-of-buffer)))
	(error "sendmail couldn't send message"))
      ;; No errors
      (when send-mail-show-output
	(with-view (other-view)
	  (goto-buffer temp-buffer)
	  (goto (start-of-buffer))
	  (shrink-view-if-larger-than-buffer))))))


;; storing/reloading messages

(defun send-mail-defer (filename)
  "Save the message currently being composed to the file named FILENAME. The
`M-x send-mail-restore' command may be used to restore the message so that
it can be completed and sent out."
  (interactive
   (list (prompt-for-file "Save message to file:" nil mail-drafts-directory)))
  (or filename (error "No file specified"))
  (when (or (not (string= (buffer-name) "*mail*"))
	    (buffer-read-only-p))
    (unless (y-or-n-p "Is this buffer really a mail message?")
      (error "Quit")))
  (send-mail-delete-separator)
  (write-buffer-contents filename)
  (send-mail-insert-separator)
  (set-buffer-modified (current-buffer) nil)
  (bury-buffer))

;;;###autoload
(defun send-mail-restore (filename &optional dont-delete)
  "Continue composing a message whose current contents are stored in the
file called FILENAME. Unless the DONT-DELETE parameter is non-nil the
file is deleted after being read into the message editor. When called
interactively the DONT-DELETE value is taken from the raw prefix argument."
  (interactive
   (let
       ((arg current-prefix-arg))
     (list (prompt-for-file "Saved message:" t mail-drafts-directory) arg)))
  (send-mail-find-mail-buffer)
  (read-file-contents filename)
  (send-mail-insert-separator)
  (set-buffer-undo-list nil)
  (send-mail-mode)
  (unless dont-delete
    (delete-file filename)))
