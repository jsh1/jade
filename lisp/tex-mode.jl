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

(defvar latex-run-command "latex %s"
  "Program for running LaTeX")

(defvar tex-keymap
  (bind-keys (make-sparse-keymap)
    "TAB" 'tab-with-spaces))

(defvar tex-ctrl-c-keymap
  (bind-keys (make-sparse-keymap)
    "C-c" '(next-keymap-path '(tex-ctrl-c-ctrl-c-keymap))
    "C-f" 'tex-file
    "TAB" 'bibtex-file
    "C-l" 'tex-recenter-output-buffer
    "C-k" 'tex-kill-job
    "C-p" 'tex-print
    "C-v" 'tex-view
    "C-q" 'tex-show-print-queue))

(defvar tex-ctrl-c-ctrl-c-keymap
  (bind-keys (make-sparse-keymap)
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
(defun tex-mode (&optional from-sub-mode)
  "TeX Mode:\n
Major mode for editing TeX source files.\n
Local bindings in this mode are:\n
\\{tex-keymap}
\\{tex-ctrl-c-ctrl-c-keymap,Ctrl-c Ctrl-c}"
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "TeX"
	major-mode 'tex-mode
	major-mode-kill 'tex-mode-kill
	ctrl-c-keymap tex-ctrl-c-keymap
	paragraph-separate "^(([\t\f\n ]|(\\$\\$))*|\\\\(begin|end|noindent).*)\n"
	paragraph-start "^( +|\\\\item)"
	page-start "^\\\\((sub)*(section|paragraph)|chapter){.*}"
	keymap-path (cons 'tex-keymap keymap-path)
	mode-comment-header "%"
	generic-exp-single-delims '(?\" ?\$)
	generic-exp-escape-char 0
	generic-exp-comment-string "%"
	generic-exp-symbol-re "[a-zA-Z0-9:_@-]+"
	generic-exp-special-re "[][(){}\"\$a-zA-Z0-9]")
  (make-local-variable 'ispell-ignore-word-hook)
  (add-hook 'ispell-ignore-word-hook 'tex-ispell-ignore-word-hook)
  (call-hook 'text-mode-hook)
  (call-hook 'tex-mode-hook)
  (cond (from-sub-mode)
	((re-search-backward "^\\\\(document(class|style)|chapter|(sub)*section)"
			     (min (forward-line 100 (start-of-buffer))
				  (end-of-buffer)))
	 (latex-mode t))
	(t
	 (plain-tex-mode t))))

(defun tex-mode-kill ()
  (setq mode-name nil
	major-mode nil
	major-mode-kill nil
	keymap-path (delq 'tex-keymap keymap-path)))

(defun latex-mode (&optional from-super-mode)
  "LaTeX Mode:\n
Major mode for editing LaTeX source files.\n
Local bindings in this mode are:\n
\\{tex-keymap}
\\{tex-ctrl-c-ctrl-c-keymap,Ctrl-c Ctrl-c}"
  (interactive)
  (unless from-super-mode
    (tex-mode t))
  (setq major-mode 'latex-mode
	mode-name "LaTeX")
  (unless (assq 'tex-run-command (buffer-variables))
    (setq tex-run-command latex-run-command))
  (call-hook 'latex-mode-hook))  

(defun plain-tex-mode (&optional from-super-mode)
  "Plain TeX Mode:\n
Major mode for editing Plain TeX source files.\n
Local bindings in this mode are:\n
\\{tex-keymap}
\\{tex-ctrl-c-ctrl-c-keymap,Ctrl-c Ctrl-c}"
  (interactive)
  (unless from-super-mode
    (tex-mode t))
  (call-hook 'plain-tex-mode-hook))
  
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
	    (goto (forward-word count))
	    (insert "\}"))
	(goto (forward-word count))
	(insert (if command (concat ?\\ command ?\{) "\{"))
	(goto (forward-word (- count)))
	(insert "\}")))))

(defun tex-move-over-braces ()
  (interactive)
  (goto (forward-char 1 (char-search-forward ?}))))

(defun tex-ispell-ignore-word-hook (word start end)
  (= (get-char (forward-char -1 start)) ?\\ ))
