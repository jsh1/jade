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
(defun rm-mail-dir-scanner (rm-message)
  (let
      ((addr (rm-get-msg-field rm-message rm-msg-from-addr))
       (name (rm-get-msg-field rm-message rm-msg-from-name)))
    (when (and addr name)
      (mail-dir-scan-function addr name)))
  t)
(add-hook 'read-mail-display-message-hook 'rm-mail-dir-scanner)

(defun rm-mail-dir-scan-current ()
  "Add the sender of the currently displayed message to the mail directory."
  (interactive)
  (let
      ((mail-dir-scan-messages t)
       (mail-dir-prompt-when-scanning nil))
    (rm-mail-dir-scanner rm-current-msg)))
(bind-keys rm-keymap
  "+" 'rm-mail-dir-scan-current)
(bind-keys rm-summary-keymap
  "+" '(rm-command-with-folder 'rm-mail-dir-scan-current))
