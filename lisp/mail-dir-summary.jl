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

;; Suppress annoying compiler warnings
(eval-when-compile (require 'send-mail))

(defvar mds-keymap
  (bind-keys (make-sparse-keymap summary-keymap)
    "n" 'summary-next-item
    "p" 'summary-next-item
    "a" 'mds-add-item
    "e" 'mds-edit
    "m" 'mds-compose
    "s" 'mds-sort-list))

(defvar mds-functions
  '((select . mds-edit)
    (delete . md-delete-record)
    (print . mds-print)
    (list . (lambda () mail-address-list))
    (after-marking . (lambda () (summary-next-item 1)))
    (on-quit . kill-current-buffer)))

;;;###autoload
(defun list-mail-dir ()
  "List the mail directory in a buffer."
  (interactive)
  (goto-other-view)
  (goto-buffer (open-buffer "*mail-dir*"))
  (if (eq major-mode 'summary-mode)
      (summary-update)
    (insert "Mail directory:\n\n  Name\t\t\tAddress\n  ----\t\t\t-------\n")
    (summary-mode "Mail-Dir" mds-functions mds-keymap)
    (setq major-mode 'mds-mode)))

(defun mds-mode ()
  "Mail directory mode:

This mode allows the mail directory to be displayed and edited. Each line
represents one record, only the name and the email addresses from the
record are displayed. Typing \\[mds-edit] or \\[summary-select-item] allows all fields in the record
to be edited.

Other local commands include:

\\{mds-keymap}")

(defun mds-print (item)
  (insert (if (memq 'delete (summary-get-pending-ops item)) "D " "  "))
  (let
      ((addresses (md-get-field item ':net))
       (names (md-get-field item ':name)))
    (when names
      (insert (car names))
      (when (cdr names)
	(insert "..."))
      (insert " "))
    (indent-to 24)
    (while addresses
      (insert (car addresses))
      (when (cdr addresses)
	(insert ", "))
      (setq addresses (cdr addresses)))))

(defun mds-add-item ()
  "Insert a new item into the list."
  (interactive)
  (call-command 'add-mail-address)
  (summary-update))

(defun mds-compose (item)
  "Compose a new mail message with the current item as the To: field (or the
CC: field if the prefix arg is set)."
  (interactive (list (summary-current-item)))
  (mail-setup)
  (if current-prefix-arg
      (send-mail-go-cc)
    (send-mail-go-to))
  (insert-mail-item (car (md-get-field item ':name)))
  (set-buffer-modified nil nil)
  (send-mail-go-subject))

(defun mds-sort-predicate (x y)
  (let
      ((name-x (md-get-field x ':name))
       (name-y (md-get-field y ':name)))
    (< (or name-x x) (or name-y y))))

(defun mds-sort-list ()
  "Sort the list of mail addresses or aliases."
  (interactive)
  (setq mail-address-list (sort mail-address-list 'mds-sort-predicate))
  (setq mail-directory-modified t)
  (summary-update))


;; Editing items

(defvar mds-edit-keymap (bind-keys (make-sparse-keymap)
			  "C-d" '(mds-edit-field "address")
			  "C-w" '(mds-edit-field "company")
			  "C-o" '(mds-edit-field "notes")
			  "C-p" '(mds-edit-field "phone")
			  "C-a" '(mds-edit-field "net-alias")
			  "C-e" '(mds-edit-field "net")
			  "C-n" '(mds-edit-field "name")
			  "C-c" 'mds-edit-commit))

(defvar mds-edit-item nil)
(make-variable-buffer-local 'mds-edit-item)

(defun mds-edit ()
  "Mail directory editor:

This mode allows individual records in the mail directory to be edited. Each
field begins at the start of a line and is formatted as the name of the
field, a colon, then the Lisp data object representing the field's contents.
Generally each field will contain a list of strings.

Type \\[mds-edit-commit] to finalise the edits. The full list of local keybindings is:

\\{mds-edit-keymap,C-c}"
  (interactive)
  (let
      ((buffer (make-buffer "*mail-dir-edit*"))
       (item (summary-current-item)))
    (goto-other-view)
    (goto-buffer buffer)
    (setq local-ctrl-c-keymap mds-edit-keymap)
    (setq major-mode 'mds-edit)
    (setq mode-name "Mail-Dir-Edit")
    ;; Copied from lisp-mode
    (setq mode-comment-header ";"
	  mode-indent-line 'lisp-indent-line
	  mode-forward-exp 'lisp-forward-sexp
	  mode-backward-exp 'lisp-backward-sexp
	  mode-symbol-regexp "[^][()?'`,@\"#; \t\f\n]+")
    (setq mds-edit-item (md-get-field item ':name))
    (unless mds-edit-item
      (error "This item has no name: %s" item))
    (format buffer "name: %S\n" mds-edit-item)
    (mapc #'(lambda (cell)
	      (unless (eq (car cell) ':name)
		(format buffer "%s: "
			(substring (symbol-name (car cell)) 1))
		(if (listp (cdr cell))
		    (progn
		      (insert "\(")
		      (let
			  ((first t)
			   (indent (pos-col (char-to-glyph-pos))))
			(mapc #'(lambda (e)
				  (unless first
				    (insert "\n")
				    (indent-to indent))
				  (prin1 e buffer)
				  (setq first nil)) (cdr cell)))
		      (insert "\)\n"))
		  (prin1 (cdr cell) buffer)
		  (insert "\n")))) item)
    (set-buffer-modified nil nil)
    (setq buffer-undo-list nil)
    (message "Type `C-c C-c' to commit edits")))

(defun mds-edit-commit ()
  (interactive)
  (or mds-edit-item
      (error "Nothing being edited in this buffer!"))
  (let
      ((record nil)
       (tem (start-of-buffer)))
    (while (re-search-forward "^([^ \t\n:]+)[ \t]*:" tem)
      (let*
	  ((field (intern (concat ?: (expand-last-match "\\1"))))
	   (stream (cons (current-buffer) (match-end)))
	   (body (read stream)))
	(setq record (cons (cons field body) record))
	(setq tem (cdr stream))))
    (or (assq ':name record)
	(error "Record has no name field: %s" record))
    (let
	((old (md-get-record ':name (car mds-edit-item))))
      (when old
	(md-delete-record old))
      (setq mail-address-list (cons record mail-address-list))
      (kill-current-buffer)
      (and (get-buffer "*mail-dir*")
	   (with-buffer (get-buffer "*mail-dir*")
	     (summary-update))))))

(defun mds-edit-field (field)
  (if (re-search-forward (concat ?^ (quote-regexp field) "[\t ]*:[\t ]*\\(?")
			 (start-of-buffer))
      (goto (match-end))
    (goto (end-of-buffer))
    (unless (zerop (pos-col (cursor-pos)))
      (insert "\n"))
    (insert field)
    (insert ": ()\n")
    (goto (forward-char -2))))
