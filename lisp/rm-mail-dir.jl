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

;; This function is added to read-mail.jl's read-mail-display-message-hook
(defun rm-mail-dir-scanner (rm-message folder &optional all-addresses)
  (mapc #'(lambda (cell)
	    (when (and (car cell) (cdr cell))
	      (mail-dir-scan-function (car cell) (cdr cell))))
	(rm-get-senders rm-message))
  (when all-addresses
    (mapc #'(lambda (cell)
	      (when (and (car cell) (cdr cell))
		(mail-dir-scan-function (car cell) (cdr cell))))
	  (rm-get-recipients rm-message)))
  t)
(add-hook 'rm-display-message-hook 'rm-mail-dir-scanner)

(defun rm-mail-dir-scan-current (&optional all-addresses)
  "Add the senders of the currently displayed message to the mail directory.

If ALL-ADDRESSES is non-nil, add all recipients as well. When called
interactively a prefix argument denotes ALL-ADDRESSES."
  (interactive "P")
  (let
      ((mail-dir-scan-messages t)
       (mail-dir-prompt-when-scanning nil)
       (message (rm-get-folder-field (rm-current-folder)
				     rm-folder-current-msg)))
    (rm-mail-dir-scanner message (rm-current-folder) all-addresses)))

;; Bind to read-mail keymap
(bind-keys rm-keymap
  "Ctrl-k" 'rm-mail-dir-scan-current)

;; Similar for rm-summary keymap, but defer binding if it doesn't exist
(if (featurep 'rm-summary)
    (bind-keys rm-summary-keymap
      "Ctrl-k" '(rm-command-with-folder 'rm-mail-dir-scan-current))
  (eval-after-load "rm-summary" '(bind-keys rm-summary-keymap
				   "Ctrl-k" '(rm-command-with-folder
					      'rm-mail-dir-scan-current))))
