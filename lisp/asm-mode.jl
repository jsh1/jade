;;;; asm-mode.jl -- Primitive mode for generic assembler code
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

(provide 'asm-mode)

(defvar asm-indent t
  "When non-nil lines are indented after being split in asm-mode.")

(defvar asm-comment ";"
  "Strings which denotes the start of a comment in asm-mode.")

(defvar asm-keymap
  (bind-keys (make-sparse-keymap)
    "RET" 'asm-ret
    "Shift-RET" 'split-line
    ":" 'asm-colon))

;;;###autoload
(defun asm-mode ()
  "Asm Mode:\n
Major mode for editing generic assembler source. Special commands are:\n
\\{asm-keymap}"
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "Assembler"
	major-mode 'asm-mode
	major-mode-kill asm-mode-kill
	mode-comment-header asm-comment
	local-keymap 'asm-keymap)
  (call-hook 'asm-mode-hook))

;;;###autoload
(defun asm-cpp-mode ()
  "Asm-CPP Mode:\n
Major mode for editing assembler source which is passed through cpp before
being assembled. Currently this only differs from asm-mode in the comments
it inserts."
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (require 'c-mode)
  (setq mode-name "Assembler-CPP"
	major-mode 'asm-cpp-mode
	major-mode-kill asm-mode-kill
	mode-comment-fun c-insert-comment
	local-keymap 'asm-keymap)
  (call-hook 'asm-mode-hook)
  (call-hook 'asm-cpp-mode-hook))

(defun asm-mode-kill ()
  (setq mode-name nil
	major-mode nil
	major-mode-kill nil
	mode-comment-fun nil
	local-keymap nil))

(defun asm-ret ()
  (interactive)
  (when (re-search-backward "[\t ]+$" (end-of-line))
    (delete-area (match-start) (match-end)))
  (insert (if asm-indent "\n\t" "\n")))

(defun asm-colon ()
  (interactive)
  (set-indent-pos (pos 0 nil))
  (insert (if asm-indent ":\t" ":")))
