;;;; rm-mail-dir.jl -- Interface mail-dir.jl with read-mail.jl
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

;;; Hooks to allow messages in the read-mail mode be scanned for
;;; email addresses by mail-dir.
;;;
;;; To enable this code, add the following form to your .jaderc:
;;;
;;;   (eval-after-load "read-mail" '(require 'rm-mail-dir))

(require 'mail-dir)
(require 'read-mail)
(provide 'rm-mail-dir)

;; Prevent warnings
(eval-when-compile (require 'rm-summary))

(defvar rm-mail-dir-auto-scan-hook (list #'(lambda () t))
  "Hook called before a mail message is automatically scanned for addresses
to add to the address book. If any function in this hook returns nil, the
message isn't scanned.")

;; This function is added to read-mail.jl's read-mail-display-message-hook
(defun rm-mail-dir-scanner (rm-message folder #!optional all-addresses force)
  (call-hook 'rm-mail-dir-auto-scan-hook (list rm-message) 'and)
  (mapc #'(lambda (cell)
	    (when (and (car cell) (or (cdr cell) force))
	      (mail-dir-scan-function
	       (car cell) (or (cdr cell)
			      (prompt-for-mail-full-name
			       (concat "Name of <" (car cell) ">:") t)))))
	(rm-get-senders rm-message))
  (when all-addresses
    (mapc #'(lambda (cell)
	      (when (and (car cell) (or (cdr cell) force))
		(mail-dir-scan-function
		 (car cell) (or (cdr cell)
				(prompt-for-mail-full-name
				 (concat "Name of <" (car cell) ">:") t)))))
	  (rm-get-recipients rm-message)))
  t)
(add-hook 'rm-display-message-hook rm-mail-dir-scanner)

(defun rm-mail-dir-scan-messages (#!optional all-addresses)
  "Add the senders of the selected message(s) to the mail directory.

If ALL-ADDRESSES is non-nil, add all recipients as well. When called
interactively a prefix argument denotes ALL-ADDRESSES."
  (interactive "P")
  (let*
      ((mail-dir-scan-messages t)
       (mail-dir-prompt-when-scanning nil)
       (folder (rm-current-folder))
       (messages (rm-command-items folder)))
    (mapc #'(lambda (m)
	      (rm-mail-dir-scanner m folder all-addresses t)) messages)))

;; Bind to read-mail keymap
(bind-keys rm-keymap
  "Ctrl-k" 'rm-mail-dir-scan-messages)

;; Similar for rm-summary keymap
(lazy-bind-keys rm-summary rm-summary-keymap
  "Ctrl-k" '(rm-command-with-folder 'rm-mail-dir-scan-messages))
