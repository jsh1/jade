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

(defvar bs-buffer (make-buffer "*buffers*"))

(defvar bs-keymap (copy-sequence summary-keymap))
(bind-keys bs-keymap
  "s" 'bs-mark-save
  "Ctrl-s" 'bs-mark-save
  "1" 'bs-select-whole-window
  "2" 'bs-select-two-views
  "o" 'bs-select-other-view
  "f" 'summary-select-item
  "~" 'bs-toggle-modified
  "-" 'bs-toggle-read-only
  "%" 'bs-toggle-read-only)

(defvar bs-functions '((select . bs-select-item)
		       (delete . kill-buffer)
		       (print . bs-print-item)
		       (list . (lambda () buffer-list))
		       (save . save-file))
  "Function vector for summary-mode.")

;;;###autoload
(defun buffer-summary ()
  "Switch to the buffer-summary."
  (interactive)
  (goto-buffer bs-buffer)
  (if (eq major-mode 'summary-mode)
      (summary-update)
    (insert "Buffer Summary:\n\n   MR\tName\t\tMode\t\tFile\n   --\t----\t\t----\t\t----\n")
    (summary-mode "Buffer-Summary" bs-functions bs-keymap)))

(defun bs-print-item (item)
  (let
      ((pending-ops (summary-get-pending-ops item)))
    (format (current-buffer) "%c%c %c%c\t%s\t"
	    (if (memq 'delete pending-ops) ?D ?\ )
	    (if (memq 'save pending-ops) ?S ?\ )
	    (if (buffer-modified-p item) ?+ ?\ )
	    (if (buffer-read-only-p item) ?- ?\ )
	    (buffer-name item))
    (indent-to 24)
    ;; Print out the mode names
    (insert (or (with-buffer item mode-name) "Generic"))
    (let
	((minor-names (with-buffer item minor-mode-names)))
      (while minor-names
	(format (current-buffer) " %s" (car minor-names))
	(setq minor-names (cdr minor-names))))
    (insert "\t")
    (indent-to 40)
    (format (current-buffer) "%s\n" (buffer-file-name item))))

(defun bs-select-item (item)
  (bury-buffer bs-buffer)
  (goto-buffer item))

(defun bs-select-whole-window ()
  (interactive)
  (close-other-views)
  (bs-select-item (summary-current-item)))

(defun bs-select-two-views ()
  (interactive)
  (let
      ((new-buf (summary-current-item))
       (old-buf (nth 1 buffer-list))
       first-view second-view)
    (bury-buffer bs-buffer)
    (if (< (window-view-count) 3)
	(open-view)
      (while (> (window-view-count) 3)
	(close-view)))
    (setq first-view (window-first-view)
	  second-view (next-view first-view))
    (with-view first-view
      (goto-buffer old-buf))
    (set-current-view second-view)
    (goto-buffer new-buf)))

(defun bs-select-other-view ()
  (interactive)
  (let
      ((buf (summary-current-item)))
    (in-other-view '(goto-buffer buf))))

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
