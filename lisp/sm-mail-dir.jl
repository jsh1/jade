;;;; sm-mail-dir.jl -- Interface mail-dir.jl with send-mail.jl
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

;;; Hooks to allow messages in send-mail mode to have their aliases
;;; expanded. Also binds `Ctrl-c a' to a command to interactively insert
;;; an address, and `Ctrl-c Ctrl-a' to a command to insert an alias.
;;;
;;; To enable this code, add the following form to your .jaderc:
;;;
;;;   (eval-after-load "send-mail" '(require 'sm-mail-dir))

(require 'mail-dir)
(require 'maildefs)
(require 'send-mail)
(provide 'sm-mail-dir)

(bind-keys send-mail-c-keymap
  "Ctrl-a" 'insert-mail-address-and-name
  "a" 'insert-mail-alias
  "Ctrl-x" 'expand-mail-aliases)

;;;###autoload
(defun expand-mail-aliases ()
  "Expand all mail aliases in the `From', `To', `CC' or `BCC' headers of the
message being composed.

To make this happen automatically, add this function to the `mail-send-hook'
(i.e. add the form \"(add-hook 'expand-mail-aliases 'mail-send-hook)\" to
your `.jaderc' file."
  (interactive)
  (save-restriction
    (unrestrict-buffer)
    (unless (re-search-forward
	     (concat ?^ (quote-regexp mail-header-separator) ?$)
	     (start-of-buffer))
      (error "Can't find message body separator"))
    ;; Only work on headers
    (restrict-buffer (start-of-buffer) (match-start))
    (goto (start-of-buffer))
    (while (re-search-forward "^(Resent-|)(From|To|CC|BCC)[\t ]*:" nil nil t)
      (save-restriction
	(goto (match-start))
	(restrict-buffer (match-start)
			 (or (forward-char -1 (mail-unfold-header
					       (match-start)))
			     (end-of-buffer)))
	(when (looking-at (concat mail-header-name "[\t ]*"))
	  (goto (match-end)))
	(let
	    (item addr)
	  (while (setq item (mail-parse-group (cursor-pos)))
	    (setq addr (apply 'concat (car item)))
	    (if (assoc addr mail-alias-alist)
		(progn
		  (delete-area (cursor-pos) (cdr item))
		  (insert-mail-alias addr))
	      (goto (cdr item)))
	    (when (looking-at "[\t\n ]*,[\t\n ]*")
	      (goto (match-end)))))))))
