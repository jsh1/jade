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
  "Ctrl-a" 'insert-mail-alias
  "a" 'insert-mail-address-and-name)

(defun sm-mail-dir-expand-aliases ()
  (unrestrict-buffer)
  (when (re-search-forward (concat ?^ (quote-regexp mail-header-separator) ?$))
    (restrict-buffer (start-of-buffer) (match-start)))
  (goto (start-of-buffer))
  (while (re-search-forward "^(Resent-|)(From|To|CC|BCC)[\t ]*:" nil nil t)
    (save-restriction
      (goto (match-end))
      (restrict-buffer (match-start) (or (mail-unfold-header (match-start))
					 (end-of-buffer)))
      (when (looking-at (concat mail-header-name "[\t ]*"))
	(goto (match-end)))
      (let
	  (item)
	(while (setq item (mail-parse-group (cursor-pos)))
	  (when (assoc (car item) mail-alias-alist)
	    (delete-area (cursor-pos) (cdr item))
	    (insert-mail-alias (car item)))
	  (when (looking-at "[\t\n ]*,[\t\n ]*")
	    (goto (match-end))))))))

(when mail-dir-auto-expand-aliases
  (add-hook 'mail-send-hook 'sm-mail-dir-expand-aliases))
