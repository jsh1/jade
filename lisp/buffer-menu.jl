;;;; buffer-menu.jl -- interactive buffer manipulation
;;;  Copyright (C) 1994 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'buffer-menu)


(defvar bm-buffer (make-buffer "*Buffer Menu*"))
(set-buffer-special bm-buffer t)
(set-buffer-read-only bm-buffer t)
(with-buffer bm-buffer
  (setq buffer-record-undo nil))

(defvar bm-keymap (make-keylist))
(bind-keys bm-keymap
  "d" 'bm-toggle-deletion
  "s" 'bm-toggle-save
  "Ctrl-s" 'bm-toggle-save
  "u" 'bm-unmark-line
  "x" 'bm-execute
  "1" 'bm-select-buffer-whole-window
  "2" 'bm-select-buffer-two-views
  "RET" 'bm-select-buffer
  "f" 'bm-select-buffer
  "q" 'bury-buffer
  "~" 'bm-toggle-modified
  "-" 'bm-toggle-read-only
  "%" 'bm-toggle-read-only
  "o" 'bm-select-buffer-other-view
  "Ctrl-f" 'bm-next
  "TAB" 'bm-next
  "Ctrl-b" 'bm-prev
  "Shift-TAB" 'bm-prev
  "Meta-TAB" 'bm-prev
  "Ctrl-l" 'bm-update
  "LMB-Click1" '(goto (mouse-pos))
  "LMB-Click2" 'bm-select-buffer
  "LMB-Off" 'nop)

(defvar bm-pending-deletions '()
  "List of buffers marked for deletion.")

(defvar bm-pending-saves '()
  "List of buffers marked to be saved.")


(defun buffer-menu-mode ()
  "Buffer Menu Mode:\n
This major mode is used in the `*Buffer Menu*' buffer; it provides
interactive commands for manipulating the list of buffers loaded into
the editor.\n
Commands available are,\n
  `d'			Mark buffer for deletion.
  `s', `Ctrl-s'		Mark buffer to be saved.
  `x'			Execute marked saves and deletions.
  `u'			Unmark the current line.
  `1'			Select the current line's buffer in the sole
			 view of the current window.
  `2'			Select the buffer in a second view, with
			 the old buffer in the first.
  `o'			Display the current line's buffer in a different
			view.
  `~'			Toggle the buffer's `modified' flag.
  `%', `-'		Toggle the buffer's read-only status.
  `Ctrl-f', `TAB'	Move forwards through the menu.
  `Ctrl-b', `Meta-TAB'	Cycle backwards through the menu.
  `Ctrl-l'		Redraw the menu, incorporating any changes to the
			buffer-list.
  `q'			Quit the buffer menu."
  (when major-mode-kill
    (funcall major-mode-kill))
  (setq major-mode 'buffer-menu-mode
	major-mode-kill 'buffer-menu-kill
	mode-name "Buffer Menu"
	keymap-path (cons 'bm-keymap keymap-path))
  (add-hook 'unbound-key-hook 'bm-unbound-function)
  (call-hook 'buffer-menu-mode-hook))

(defun buffer-menu-kill ()
  (setq major-mode nil
	major-mode-kill nil
	mode-name nil
	keymap-path (delq 'bm-keymap keymap-path))
  (remove-hook 'unbound-key-hook 'bm-unbound-function))

;;;###autoload
(defun buffer-menu ()
  "Switch to the buffer menu. See `buffer-menu-mode'."
  (interactive)
  (goto-buffer bm-buffer)
  (unless (eq major-mode 'buffer-menu-mode)
    (buffer-menu-mode))
  (bm-list-buffers)
  (goto (pos 0 2)))


(defun bm-unbound-function ()
  (error "No command bound to this key!"))

(defun bm-list-buffers ()
  (let
      ((inhibit-read-only t))
    (clear-buffer)
    (insert "   MR\tName\t\tMode\t\tFile\n   --\t----\t\t----\t\t----\n")
    (let
	((list buffer-list)
	 buf)
      (while (setq buf (car list))
	(format bm-buffer "%c%c %c%c\t%s\t"
		(if (memq buf bm-pending-deletions) ?D ?\ )
		(if (memq buf bm-pending-saves) ?S ?\ )
		(if (buffer-modified-p buf) ?+ ?\ )
		(if (buffer-read-only-p buf) ?- ?\ )
		(buffer-name buf))
	(indent-to 24)
	;; Print out the mode names
	(insert (or (with-buffer buf mode-name) "Generic"))
	(let
	    ((minor-names (with-buffer buf minor-mode-names)))
	  (while minor-names
	    (format bm-buffer " %s" (car minor-names))
	    (setq minor-names (cdr minor-names))))
	(insert "\t")
	(indent-to 40)
	(format bm-buffer "%s\n" (buffer-file-name buf))
	(setq list (cdr list))))))

(defun bm-get-buffer ()
  (unless (> (pos-line (cursor-pos)) 1)
    ;; on the heading
    (error "Can't work on the heading!"))
  (if (looking-at "^[^\t]+[\t]+([^\t]+)\t" (start-of-line))
      (get-buffer (copy-area (match-start 1) (match-end 1)))
    (error "Can't find buffer name")))

(defun bm-find-buffer-line (buf)
  (re-search-forward (concat "^[^\t]+[\t]+"
			    (quote-regexp (buffer-name buf))
			    "\t")
		    (pos 0 2)))

(defun bm-toggle-deletion ()
  (interactive)
  (let
      ((buf (bm-get-buffer))
       (inhibit-read-only t))
    (if (memq buf bm-pending-deletions)
	(progn
	  (setq bm-pending-deletions (delq buf bm-pending-deletions))
	  (set-char ?\  (pos 0 nil)))
      (setq bm-pending-deletions (cons buf bm-pending-deletions))
      (set-char ?D (pos 0 nil)))
    (bm-next)))

(defun bm-toggle-save ()
  (interactive)
  (let
      ((buf (bm-get-buffer))
       (inhibit-read-only t))
    (if (memq buf bm-pending-saves)
	(progn
	  (setq bm-pending-saves (delq buf bm-pending-saves))
	  (set-char ?\  (pos 1 nil)))
      (setq bm-pending-saves (cons buf bm-pending-saves))
      (set-char ?S (pos 1 nil)))
    (bm-next)))

(defun bm-unmark-line ()
  (interactive)
  (let
      ((buf (bm-get-buffer))
       (inhibit-read-only t))
    (setq bm-pending-saves (delq buf bm-pending-saves)
	  bm-pending-deletions (delq buf bm-pending-deletions))
    (set-char ?\  (pos 0 nil))
    (set-char ?\  (pos 1 nil))
    (bm-next)))

(defun bm-execute ()
  (interactive)
  (let
      ((list bm-pending-saves)
       (inhibit-read-only t)
       buf)
    (setq bm-pending-saves nil)
    (while (setq buf (car list))
      (when (save-file buf)
	(let
	    ((pos (bm-find-buffer-line buf)))
	  (when pos
	    (set-char ?\  (pos 1 (pos-line pos)))
	    (unless (buffer-modified-p buf)
	      (set-char ?\  (pos 3 (pos-line pos)))))))
      (setq list (cdr list)))
    (setq list bm-pending-deletions
	  bm-pending-deletions nil)
    (while (setq buf (car list))
      (let
	  ((pos (bm-find-buffer-line buf)))
	(when (kill-buffer buf)
	  (when pos
	    (delete-area pos (forward-line 1 pos)))))
      (setq list (cdr list)))))

(defun bm-select-buffer ()
  (interactive)
  (let
      ((new-buf (bm-get-buffer)))
    (bury-buffer bm-buffer)
    (goto-buffer new-buf)))

(defun bm-select-buffer-whole-window ()
  (interactive)
  (delete-other-views)
  (bm-select-buffer))

(defun bm-select-buffer-two-views ()
  (interactive)
  (let
      ((new-buf (bm-get-buffer))
       (old-buf (nth 1 buffer-list))
       first-view second-view)
    (bury-buffer bm-buffer)
    (if (< (window-view-count) 3)
	(set-current-view (split-view))
      (while (> (window-view-count) 3)
	(delete-view)))
    (setq first-view (window-first-view)
	  second-view (next-view first-view))
    (with-view first-view
      (goto-buffer old-buf))
    (set-current-view second-view)
    (goto-buffer new-buf)))

(defun bm-select-buffer-other-view ()
  (interactive)
  (let
      ((buf (bm-get-buffer)))
    (in-other-view '(goto-buffer buf))))

(defun bm-toggle-modified ()
  (interactive)
  (let
      ((buf (bm-get-buffer))
       (inhibit-read-only t))
    (if (buffer-modified-p buf)
	(progn
	  (set-buffer-modified buf nil)
	  (set-char ?\  (pos 3 nil)))
      (set-buffer-modified buf t)
      (when (buffer-modified-p buf)
	    (set-char ?+ (pos 3 nil)))))
  (bm-next))

(defun bm-toggle-read-only ()
  (interactive)
  (let
      ((buf (bm-get-buffer))
       (inhibit-read-only t))
    (with-buffer buf
      (toggle-buffer-read-only))
    (if (buffer-read-only-p buf)
	(set-char ?- (pos 4 nil))
      (set-char ?\  (pos 4 nil))))
  (bm-next))

(defun bm-update ()
  (interactive)
  (let
      ((old-buf (bm-get-buffer)))
    (bm-list-buffers)
    (goto (or (bm-find-buffer-line old-buf)
	      (pos 0 2)))))

(defun bm-next ()
  (interactive)
  (if (>= (pos-line (cursor-pos)) (- (buffer-length) 2))
      ;; last line
      (goto-glyph (pos nil 2))
    (goto (forward-line))))

(defun bm-prev ()
  (interactive)
  (if (<= (pos-line (cursor-pos)) 2)
      ;; first line
      (goto-glyph (pos nil (- (buffer-length) 2)))
    (goto (forward-line -1))))
