#| javascript-mode.jl -- Major mode for editing Javascript source

   Copyright (C) 1993-2015 John Harper <jsh@unfactored.org>

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(require 'c-mode)
(provide 'javascript-mode)

(defvar js-body-indent 2
  "Indentation of code with respect to its containing block.")

(defvar js-brace-indent -2
  "Extra indentation of braces in relative to the body of the code they
contain.")

(defvar js-case-indent -2
  "Extra indentation for case statements.")

(make-variable-buffer-local 'js-body-indent)
(make-variable-buffer-local 'js-brace-indent)
(make-variable-buffer-local 'js-case-indent)

;;;###autoload
(defun javascript-mode ()
  "Javascript Mode:\n
Major mode for editing Javascript source code. Local keybindings are:\n
\\{c-mode-keymap}"
  (interactive)
  (and major-mode-kill (major-mode-kill))
  (setq mode-name "Javascript"
	major-mode 'javascript-mode
	major-mode-kill kill-all-local-variables
	mode-comment-fun c-insert-comment
	mode-indent-line c-indent-line
	mode-forward-exp c-forward-exp
	mode-backward-exp c-backward-exp
	mode-defun-header "^\\s+function\\s+"
	mode-defun-footer "^\\s+}"
	paragraph-separate "^[\n\t\f ]*\n"
	paragraph-start paragraph-separate
	local-ctrl-c-keymap c-mode-ctrl-c-keymap
	local-keymap c-mode-keymap
	c-body-indent js-body-indent
	c-brace-indent js-brace-indent
	c-case-indent js-brace-indent
	c-label-indent 0)
  (call-hook 'javascript-mode-hook))
