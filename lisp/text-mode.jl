;;;; text-mode.jl -- Modes for editing English text
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

(provide 'text-mode)


;; Basic text mode

(defvar text-mode-keymap (make-keylist))
(bind-keys text-mode-keymap
  "Meta-s" 'center-line
  "Meta-S" 'center-paragraph
  "Meta-=" 'word-count-area)

(defvar text-mode-indent-keymap (make-keylist))
(bind-keys text-mode-indent-keymap
  "TAB" 'text-mode-indent-tab)

(defun text-mode-init ()
  (setq major-mode-kill 'text-mode-kill
	word-regexp "[a-zA-Z0-9]"
	word-not-regexp "[^a-zA-Z0-9]|$"))

;;;###autoload
(defun text-mode ()
  "Mode for editing English text in."
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "Text"
	major-mode 'text-mode
	keymap-path (cons 'text-mode-keymap keymap-path))
  (text-mode-init)
  (call-hook 'text-mode-hook))

;;;###autoload
(defun indented-text-mode ()
  "Variant of `text-mode' in which the TAB key indents to the depth of the
previous line, then works as normal."
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "Indented Text"
	major-mode 'indented-text-mode
	keymap-path (cons 'text-mode-indent-keymap
			  (cons 'text-mode-keymap keymap-path))
	fill-prefix 'text-mode-fill-prefix)
  (text-mode-init)
  (call-hook 'text-mode-hook)
  (call-hook 'indented-text-mode-hook))

(defun text-mode-kill ()
  (setq mode-name nil
	keymap-path (delq 'text-mode-keymap 
			  (delq 'text-mode-indent-keymap keymap-path))
	major-mode nil
	major-mode-kill nil)
  (when (functionp fill-prefix)
    (setq fill-prefix nil))
  t)

(defun text-mode-indent-tab ()
  (interactive)
  (let
      ((pos (re-search-backward "^.+$" (forward-line -1))))
    (if (or (null pos) (> (pos-col (cursor-pos)) (line-length pos)))
	(insert "\t")
      (let
          ((gcurs (char-to-glyph-pos (cursor-pos))))
        (setq gcurs (pos (pos-col gcurs) (pos-line pos))
	      pos (glyph-to-char-pos gcurs))
	(re-search-forward "[\t ]+|$" pos)
	(if (equal (match-end) (end-of-line pos))
	    (insert "\t")
	  (setq pos (pos (pos-col (char-to-glyph-pos (match-end)))
			 (pos-line (cursor-pos))))
	  (if (empty-line-p pos)
	      (set-indent-pos pos)
	    (indent-to (pos-col pos))))))))

(defun text-mode-fill-prefix (op pos)
  (cond
   ((eq op 'insert)
    (unless (zerop (pos-line pos))
      (save-cursor
	(goto pos)
	(indent-to (pos-col (indent-pos (forward-line -1)))))))
   ((eq op 'delete)
    (when (looking-at "^[\t ]+" pos)
      (delete-area (match-start) (match-end))))
   ((eq op 'width)
    (if (zerop (pos-line pos))
	0
      (pos-col (indent-pos (forward-line -1 pos)))))))


;; Misc

;;;###autoload
(defun word-count-area (start end &optional print)
  "Return the number of words in the area of text between START and END in
the current buffer. If PRINT is non-nil this number is also displayed in the
status line."
  (interactive "-m\nM\nt")
  (let
      ((tmp start)
       (count 0))
    ;; Catch the end-of-buffer error
    (condition-case nil
	(while (<= tmp end)
	  (setq count (1+ count)
		tmp (forward-word 1 tmp)))
      (error))
    (when print
      (prin1 count t))
    count))
