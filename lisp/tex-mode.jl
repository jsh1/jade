;;;; tex-mode.jl -- Mode for editing TeX and LaTeX files
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

(provide 'tex-mode)

(unless (boundp 'tex-keymap)
  (setq tex-keymap (make-keylist)
	tex-ctrl-c-keymap (make-keylist)
	tex-ctrl-c-ctrl-c-keymap (make-keylist))
  (bind-keys tex-keymap
    "TAB" 'tab-with-spaces)
  (bind-keys tex-ctrl-c-keymap
    "Ctrl-c" '(setq next-keymap-path '(tex-ctrl-c-ctrl-c-keymap)))
  (bind-keys tex-ctrl-c-ctrl-c-keymap
    "b" '(tex-insert-braces "begin")
    "c" '(tex-insert-braces "cite")
    "e" 'tex-insert-end
    "i" '(tex-insert-braces "textit")
    "l" '(tex-insert-braces "label")
    "m" '(tex-insert-braces "emph")
    "n" '(insert "\\noindent\n")
    "r" '(tex-insert-braces "ref")
    "s" '(tex-insert-braces "section")
    "u" '(tex-insert-braces "subsection")
    "t" '(tex-insert-braces "texttt")
    "{" 'tex-insert-braces
    "]" 'tex-move-over-braces
    "}" 'tex-move-over-braces))

;;;###autoload
(defun tex-mode ()
  "TeX Mode:\n
Major mode for editing TeX and LaTeX source files.\n
Special commands available are,\n
  `Ctrl-c Ctrl-c b'	Insert `\\begin{}' leaving the cursor between the
			 braces
  `Ctrl-c Ctrl-c c'	Insert `\\cite{}'
  `Ctrl-c Ctrl-c e'	Insert the correct LaTeX `\\end' command for the
			 current context
  `Ctrl-c Ctrl-c i'	Insert `\\textit{}'
  `Ctrl-c Ctrl-c l'	Insert `\\label{}'
  `Ctrl-c Ctrl-c m'	Insert `\\emph{}'
  `Ctrl-c Ctrl-c n'	Insert `\\noindent'
  `Ctrl-c Ctrl-c r'	Insert `\\ref{}'
  `Ctrl-c Ctrl-c s'	Insert `\\section{}'
  `Ctrl-c Ctrl-c u'	Insert `\\subsection{}'
  `Ctrl-c Ctrl-c t'	Insert `\\texttt{}'
  `Ctrl-c Ctrl-c {'	Insert a pair of braces and place the cursor between
			them.
  `Ctrl-c Ctrl-c }',
  `Ctrl-c Ctrl-c ]'	Move the cursor to the character after the next
			 closing brace."
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "TeX"
	major-mode 'tex-mode
	major-mode-kill 'tex-mode-kill
	ctrl-c-keymap tex-ctrl-c-keymap
	paragraph-regexp "^[\t ]*$"
	page-regexp "^\\\\((sub)*section|chapter){.*}"
	keymap-path (cons 'tex-keymap keymap-path))
  (eval-hook 'text-mode-hook)
  (eval-hook 'tex-mode-hook))

(defun tex-mode-kill ()
  (setq mode-name nil
	major-mode nil
	major-mode-kill nil
	keymap-path (delq 'tex-keymap keymap-path)))

(defun tex-insert-end ()
  (interactive)
  (let
      ((pos (forward-char -1))
       (depth 0))
    (if (catch 'foo
	  (while (re-search-backward "\\\\(end|begin)\{([^\}]+)" pos)
	    (if (= (get-char (match-start 1)) ?b)
		;; no end
		(if (zerop depth)
		    (throw 'foo t)
		  (setq depth (1- depth)))
	      (setq depth (1+ depth)))
	    (setq pos (forward-char -1 (match-start)))))
	(format (current-buffer) "\\end{%s}\n" (copy-area (match-start 2)
							  (match-end 2)))
      (tex-insert-braces "end")
      (error "Can't find a command to \\end"))))

(defun tex-insert-braces (&optional command)
  (interactive)
  (let
      ((count (and current-prefix-arg
		   (prefix-numeric-argument current-prefix-arg))))
    (if (null count)
	(progn
	  (insert (if command (concat ?\\ command "{}") "{}"))
	  (goto (forward-char -1)))
      (if (> count 0)
	  (progn
	    (insert (if command (concat ?\\ command ?\{) "\{"))
	    (forward-word count nil t)
	    (insert "\}"))
	(forward-word count nil t)
	(insert (if command (concat ?\\ command ?\{) "\{"))
	(forward-word (- count) nil t)
	(insert "\}")))))

(defun tex-move-over-braces ()
  (interactive)
  (goto (forward-char 1 (char-search-forward ?}))))
