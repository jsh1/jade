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
  (set! mode-name "Javascript")
  (set! major-mode 'javascript-mode)
  (set! major-mode-kill kill-all-local-variables)
  (set! mode-comment-fun c-insert-comment)
  (set! mode-indent-line c-indent-line)
  (set! mode-forward-exp c-forward-exp)
  (set! mode-backward-exp c-backward-exp)
  (set! mode-defun-header "^\\s+function\\s+")
  (set! mode-defun-footer "^\\s+}")
  (set! paragraph-separate "^[\n\t\f ]*\n")
  (set! paragraph-start paragraph-separate)
  (set! local-ctrl-c-keymap c-mode-ctrl-c-keymap)
  (set! local-keymap c-mode-keymap)
  (set! c-body-indent js-body-indent)
  (set! c-brace-indent js-brace-indent)
  (set! c-case-indent js-brace-indent)
  (set! c-label-indent 0)
  (call-hook 'javascript-mode-hook))
