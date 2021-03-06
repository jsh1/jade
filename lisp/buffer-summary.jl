;;;; buffer-summary.jl -- buffer-menu using summary-mode
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
(provide 'buffer-summary)

(defvar bs-keymap
  (bind-keys (make-sparse-keymap summary-keymap)
    "s" 'bs-mark-save
    "Ctrl-s" 'bs-mark-save
    "1" 'bs-select-whole-window
    "2" 'bs-select-two-views
    "o" 'bs-select-other-view
    "f" 'summary-select-item
    "~" 'bs-toggle-modified
    "%" 'bs-toggle-read-only
    "r" 'bs-revert))

(defvar bs-popup-menus '(("Select buffer" summary-select-item)
			 ("Select in other view" bs-select-other-view)
			 ("Select in whole window" bs-select-whole-window)
			 ("Select in two views" bs-select-two-views)
			 ()
			 ("Mark for saving" bs-mark-save)
			 ("Mark for deletion" summary-mark-delete)
			 ("Unmark item" summary-unmark-item)
			 ("Unmark all" summary-unmark-all)
			 ("Execute marks" summary-execute)
			 ()
			 ("Toggle modified" bs-toggle-modified)
			 ("Toggle read-only" bs-toggle-read-only)))

(defvar bs-functions nil
  "Function vector for summary-mode.")

;;;###autoload
(defun buffer-summary ()
  "Switch to the buffer-summary."
  (interactive)
  (set-current-buffer (or (get-buffer "*buffers*")
			  (make-buffer "*buffers*")))
  (if (eq? major-mode 'buffer-summary-mode)
      (summary-update)
    (insert "Buffer Summary:\n\n\tName\t\tMode\t\tFile\n\t----\t\t----\t\t----\n")
    (summary-mode "Buffer-Summary" bs-functions bs-keymap)
    (set! major-mode 'buffer-summary-mode)))

(defun buffer-summary-mode ()
  "Buffer Summary Mode:\n
This major mode is used in the `*buffers*' buffer; it provides
interactive commands for manipulating the list of buffers loaded into
the editor. It is derived from the standard summary menu (see function
`summary-mode'); all the standard summary commands are available.
Local bindings for this mode are,\n
\\{bs-keymap}.")

(defun bs-quit ()
  (set-current-buffer (car (buffer-list))))

(defun bs-print-item (item)
  (let
      ((pending-ops (summary-get-pending-ops item)))
    (format (current-buffer) "%c%c %s\t%s "
	    (if (memq 'delete pending-ops) #\D #\space)
	    (if (memq 'save pending-ops) #\S #\space)
	    (if (buffer-modified-p item)
		(if (buffer-read-only-p item) "%*" "**")
	      (if (buffer-read-only-p item) "%%" "  "))
	    (buffer-name item))
    (indent-to 24)
    ;; Print out the mode names
    (insert (or (with-buffer item mode-name) "Fundamental"))
;    (let
;	((minor-names (with-buffer item minor-mode-names)))
;      (while minor-names
;	(format (current-buffer) " %s" (car minor-names))
;	(set! minor-names (cdr minor-names))))
    (insert " ")
    (indent-to 40)
    (format (current-buffer) "%s" (or (buffer-file-name item) ""))))

(defun bs-select-item (item)
  (bs-quit)
  (goto-buffer item))

(defun bs-select-whole-window ()
  (interactive)
  (delete-other-views)
  (bs-select-item (summary-current-item)))

(defun bs-select-two-views ()
  (interactive)
  (let
      ((new-buf (summary-current-item))
       (old-buf (car (buffer-list)))
       first-view second-view)
    (bs-quit)
    (if (< (window-view-count) 3)
	(set-current-view (split-view))
      (while (> (window-view-count) 3)
	(delete-view)))
    (set! first-view (window-first-view))
    (set! second-view (next-view first-view))
    (with-view first-view
      (goto-buffer old-buf))
    (set-current-view second-view)
    (goto-buffer new-buf)))

(defun bs-select-other-view ()
  (interactive)
  (in-other-view `(goto-buffer ',(summary-current-item))))

(defun bs-toggle-modified ()
  (interactive)
  (let
      ((buf (summary-current-item)))
    (set-buffer-modified buf (not (buffer-modified-p buf)))
    (summary-update-item buf)))

(defun bs-toggle-read-only ()
  (interactive)
  (let
      ((buf (summary-current-item)))
    (with-buffer buf
      (toggle-buffer-read-only))
    (summary-update-item buf)))

(defun bs-mark-save ()
  (interactive)
  (summary-add-pending-op (summary-current-item) 'save))

(defun bs-revert ()
  (interactive)
  (mapc revert-buffer (summary-command-items))
  (summary-update))
     

;; init

(set! bs-functions (list (cons 'select bs-select-item)
			 (cons 'delete kill-buffer)
			 (cons 'print bs-print-item)
			 (cons 'list (lambda ()
				       (copy-sequence (buffer-list))))
			 (cons 'save save-file)
			 (cons 'after-marking (lambda ()
						(summary-next-item 1)))
			 (cons 'with-extent
			       (lambda (e)
				 (extent-put
				  e 'popup-menus bs-popup-menus)))
			 (cons 'on-quit bs-quit)))
