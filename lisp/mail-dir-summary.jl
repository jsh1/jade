;;;; mail-dir-summary.jl -- Summary mode for the mail directory
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

(require 'summary)
(require 'mail-dir)
(provide 'mail-dir-summary)

(defvar mds-alias-buffer (make-buffer "*mail-aliases*"))
(defvar mds-address-buffer (make-buffer "*mail-addresses*"))

(defvar mds-keymap (copy-sequence summary-keymap))
(bind-keys mds-keymap
  "n" 'summary-next-item
  "p" 'summary-next-item
  "a" 'mds-add-item
  "e" 'mds-edit-name
  "b" 'mds-edit-body)

(defvar mds-alias-functions
  '((select . nop)
    (delete . (lambda (item) (remove-mail-alias (car item))))
    (print . mds-print-alias)
    (list . (lambda () mail-alias-alist))
    (after-marking . (lambda () (summary-next-item 1)))))

(defvar mds-address-functions
  '((select . nop)
    (delete . (lambda (item) (remove-mail-address (car item))))
    (print . mds-print-address)
    (list . (lambda () mail-address-alist))
    (after-marking . (lambda () (summary-next-item 1)))))

;;;###autoload
(defun list-mail-aliases ()
  "List all mail aliases in a buffer."
  (interactive)
  (goto-other-view)
  (goto-buffer mds-alias-buffer)
  (if (eq major-mode 'summary-mode)
      (summary-update)
    (insert "Mail alias summary:\n\n  Alias\t\tExpansion\n  -----\t\t---------\n")
    (summary-mode "Alias-Summary" mds-alias-functions mds-keymap)))

;;;###autoload
(defun list-mail-addresses ()
  "List all mail addresses in a buffer."
  (interactive)
  (goto-other-view)
  (goto-buffer mds-address-buffer)
  (if (eq major-mode 'summary-mode)
      (summary-update)
    (insert "Mail address summary:\n\n  Name\t\t\tAddress\n  ----\t\t\t-------\n")
    (summary-mode "Address-Summary" mds-address-functions mds-keymap)))

(defun mds-print-alias (item)
  (insert (if (memq 'delete (summary-get-pending-ops item)) "D " "  "))
  (insert (car item))
  (insert " ")
  (indent-to 16)
  (princ (cdr item) (current-buffer)))

(defun mds-print-address (item)
  (insert (if (memq 'delete (summary-get-pending-ops item)) "D " "  "))
  (insert (cdr item))
  (insert " ")
  (indent-to 24)
  (insert (car item)))

(defun mds-add-item ()
  (interactive)
  (call-command (if (eq (current-buffer) mds-address-buffer)
		    'add-mail-address
		  'add-mail-alias))
  (summary-update))

(defun mds-edit-name ()
  (interactive)
  (let*
      ((item (summary-current-item))
       (name (if (eq (current-buffer) mds-address-buffer)
		 (prompt-for-mail-full-name "New name:" t (cdr item))
	       (prompt-for-mail-alias "New alias name:" t (car item)))))
    (when name
      (if (eq (current-buffer) mds-address-buffer)
	  (rplacd item name)
	(rplaca item name))
      (setq mail-directory-modified t)
      (summary-update-item item))))

(defun mds-edit-body (&optional append)
  (interactive "P")
  (let
      ((item (summary-current-item))
       new)
    (if (eq (current-buffer) mds-address-buffer)
	(when (setq new (prompt-for-mail-address "New address:" t (car item)))
	  (rplaca item new)
	  (setq mail-directory-modified t))
      (setq new (prompt-for-address-list (concat "List of addresses"
						 (if append " to append")
						 ?:) t))
      (if append
	  (rplacd item (append (cdr item) new))
	(rplacd item new))
      (setq mail-directory-modified t))
    (summary-update-item item)))
