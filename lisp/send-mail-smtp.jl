;;;; send-mail-smtp.jl -- sending mail messages via SMTP

;;;  Copyright (C) 2001 John Harper <jsh@pixelslut.com>

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
(provide 'send-mail-smtp)

(defvar mail-smtp-server nil
  "The name of the host to use as an SMTP server.")

(defvar mail-smtp-port 25
  "The port number of the SMTP server. Normally port 25.")

(defvar mail-smtp-return-address user-mail-address
  "The mail address used as the return address when sending messages via
SMTP directly.")

;; put this in the send-mail-send-it-function variable
(define (smtp-send-it normal-addresses resent-addresses temp-buffer)

  (require 'mail-headers)
  (require 'rep.io.sockets)

  (unless mail-smtp-server
    (error "You need to set the `mail-smtp-server' variable!"))

  (let ((pending-output nil)
	(got-result nil)
	(error-occurred nil)
	socket)

    ;; Receives output from the SMTP server. Looks for result strings
    (define (smtp-output output)
      (write temp-buffer output)
      (when pending-output
	(setq output (concat pending-output output))
	(setq pending-output nil))
      (let ((point 0))
	(while (string-match "\n" output point)
	  (let ((line (substring output point (match-start))))
	    (setq point (match-end))
	    (cond ((string-looking-at "\\s*^(211|214|220|221)\\b" line)
		   ;; ignored responses
		   )
		  ((string-match "\\s*^[23]\\d\\d\\b" line)
		   ;; success
		   (setq got-result t)
		   (setq error-occurred nil))
		  ((string-match "\\s*^[45]\\d\\d\\s+(.*)\\s+$" line)
		   (setq got-result t)
		   (setq error-occurred (expand-last-match "\\2"))))))
	(setq pending-output (substring output point))))

    ;; Output a single SMTP command; waits for the result before returning
    ;; Signals an error unless the SMTP command succeeds
    (define (smtp-command fmt . args)
      (or (socket? socket)
	  (error "SMTP server closed connection"))
      (apply format socket fmt args)
      (setq got-result nil)
      (while (and (not got-result) (socket? socket))
	(and (accept-socket-output-1 socket 120)
	     (error "Timed out waiting for SMTP server"))
	(when error-occurred
	  (error "SMTP error: %s" error-occurred))))

    ;; Send the list of unparsed address strings ADDRESSES as "RCPT To:"
    ;; SMTP commands. Signals an error if any fail
    (define (send-rcpts addresses)
      (mapc (lambda (x)
	      (let ((addr (mail-parse-address x)))
		(when (car addr)
		  (smtp-command "RCPT To: <%s>\n" (car addr)))))
	    addresses))

    ;; Create the socket used to talk to the SMTP server
    (setq socket (socket-client mail-smtp-server mail-smtp-port smtp-output))

    ;; Log on
    (smtp-command "HELO %s\n" (system-name))

    ;; Start a mail message by sending our return address
    (smtp-command "MAIL From: <%s>\n" mail-smtp-return-address)

    ;; Send the recipient addresses
    (send-rcpts normal-addresses)
    (send-rcpts resent-addresses)

    ;; Then send the message itself (including headers). Quote `.' at the
    ;; start of a line as `..', as per the SMTP standard
    (smtp-command "DATA\n")
    (let ((point (start-of-buffer))
	  (seen-date nil)
	  (seen-message-id)
	  (in-header t))
      (while (< point (end-of-buffer))
	(when in-header
	  (cond ((looking-at "Date[ \t]*:" point nil t)
		 (setq seen-date t))
		((looking-at "Message-ID[ \t]*:" point nil t)
		 (setq seen-message-id t))
		((looking-at "^[ \t]*$" point)
		 (setq in-header nil)
		 (unless seen-date
		   (format socket "Date: %s\n"
			   (current-time-string
			    nil "%a, %d %b %Y %H:%M:%S %z")))
		 (unless seen-message-id
		   (format socket "Message-ID: %s\n" (make-message-id))))))
	(if (and in-header (looking-at "^(Resent-)BCC[ \t]*:" point nil t))
	    ;; skip bcc headers
	    (setq point (mail-unfold-header point))
	  (let* ((next (forward-line 1 point))
		 (line (copy-area point next)))
	    (when (eqv? (aref line 0) #\.)
	      (write socket #\.))
	    (write socket line)
	    (setq point next)))))
    (smtp-command ".\n")

    ;; Finished, log off and shutdown the connection
    (smtp-command "QUIT\n")
    (close-socket socket)))
