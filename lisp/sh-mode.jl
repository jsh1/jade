;;;; sh-mode.jl -- major mode for editing shell scripts
;;;  Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'sh-mode)

(defvar sh-basic-indent 2
  "Relative indentation for each level in shell scripts.")

(defvar sh-continuation-indent 4
  "Relative indentation of continuation lines.")

(defvar sh-keymap
  (bind-keys (make-sparse-keymap)
    "TAB" 'indent-line))

;;;###autoload
(defun sh-mode ()
  "sh mode:\n
Major mode for editing bourne-shell style scripts. Local bindings are:\n
\\{sh-keymap}"
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "sh"
	major-mode 'sh-mode
	major-mode-kill 'kill-all-local-variables
	mode-comment-fun 'sh-insert-comment
	mode-indent-line 'sh-indent-line
	keymap-path (cons 'sh-keymap keymap-path))
  (call-hook 'sh-mode-hook))

(defun sh-insert-comment ()
  (if (looking-at ".*#" (start-of-line))
      (progn
	(goto (match-end))
	(unless (looking-at " ")
	  (insert " ")))
    (find-comment-pos)
    (insert "# ")))

(defun sh-get-basic-indent (pos)
  (if (zerop (pos-line pos))
      0
    (setq pos (forward-line -1 pos))
    (while (and (= (get-char (forward-char -2 pos)) ?\\ )
		(> (pos-line pos) 0))
      (setq pos (forward-line -1 pos)))
    (pos-col (indent-pos pos))))

(defun sh-indent-line (&optional pos)
  (setq pos (start-of-line pos))
  (let
      ((indent (sh-get-basic-indent pos)))
    ;; Look at the last token on the previous line
    (if (= (get-char (forward-char -2 pos)) ?\\ )
	(setq indent (+ indent sh-continuation-indent))
      (when (looking-at "((.*[^a-zA-Z0-9\n])?(do|then|in)|.*\\))[ \t]*$"
			(forward-line -1 pos))
	(setq indent (+ indent sh-basic-indent)))
      (when (looking-at ".*;;[\t ]*$" (forward-line -1 pos))
	(setq indent (- indent sh-basic-indent)))
      ;; Look at the contents of this line
      (when (looking-at "[ \t]*(done|fi|esac)" pos)
	(setq indent (- indent sh-basic-indent))))
    (set-indent-pos (pos indent (pos-line pos)))))