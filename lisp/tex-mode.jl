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

(eval-when-compile (require 'tex-shell))

(defvar latex-run-command "latex %s"
  "Program for running LaTeX")

(defvar tex-keymap
  (bind-keys (make-sparse-keymap)
    "TAB" 'tab-with-spaces))

(defvar tex-ctrl-c-keymap
  (bind-keys (make-sparse-keymap)
    "C-c" 'tex-ctrl-c-ctrl-c-keymap
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
(defun tex-mode (#!optional from-sub-mode)
  "TeX Mode:\n
Major mode for editing TeX source files.\n
Local bindings in this mode are:\n
\\{tex-keymap}\\{tex-ctrl-c-keymap,C-c}"
  (interactive)
  (when major-mode-kill
    (major-mode-kill (current-buffer)))
  (set! mode-name "TeX")
  (set! major-mode 'tex-mode)
  (set! major-mode-kill tex-mode-kill)
  (set! local-ctrl-c-keymap tex-ctrl-c-keymap)
  (set! paragraph-separate "^(([\t\f\n ]|(\\$\\$))*|\\\\(begin|end|noindent).*)\n")
  (set! paragraph-start "^( +|\\\\item)")
  (set! page-start "^\\\\((sub)*(section|paragraph)|chapter){.*}")
  (set! local-keymap 'tex-keymap)
  (set! mode-comment-header "%")
  (set! generic-exp-single-delims '(#\" #\$))
  (set! generic-exp-escape-char 0)
  (set! generic-exp-comment-string "%")
  (set! generic-exp-symbol-re "[a-zA-Z0-9:_@-]+")
  (set! generic-exp-special-re "[][(){}\"$a-zA-Z0-9]")
  (make-local-variable 'ispell-ignore-word-hook)
  (add-hook 'ispell-ignore-word-hook tex-ispell-ignore-word-hook)
  (cond (from-sub-mode)
	((re-search-backward "^\\\\(document(class|style)|chapter|(sub)*section)"
			     (min (forward-line 100 (start-of-buffer))
				  (end-of-buffer)))
	 (latex-mode t))
	(t
	 (plain-tex-mode t))))

(defun tex-mode-kill ()
  (set! mode-name nil)
  (set! major-mode nil)
  (set! major-mode-kill nil)
  (set! local-keymap nil))

;;;###autoload
(defun latex-mode (#!optional from-super-mode)
  "LaTeX Mode:\n
Major mode for editing LaTeX source files.\n
Local bindings in this mode are:\n
\\{tex-keymap}\\{tex-ctrl-c-keymap,C-c}"
  (interactive)
  (unless from-super-mode
    (tex-mode t))
  (set! major-mode 'latex-mode)
  (set! mode-name "LaTeX")
  (unless (assq 'tex-run-command (buffer-variables))
    (set! tex-run-command latex-run-command))
  (call-hook 'text-mode-hook)
  (call-hook 'tex-mode-hook)
  (call-hook 'latex-mode-hook))  

;;;###autoload
(defun plain-tex-mode (#!optional from-super-mode)
  "Plain TeX Mode:\n
Major mode for editing Plain TeX source files.\n
Local bindings in this mode are:\n
\\{tex-keymap}\\{tex-ctrl-c-keymap,C-c}"
  (interactive)
  (unless from-super-mode
    (tex-mode t))
  (call-hook 'text-mode-hook)
  (call-hook 'tex-mode-hook)
  (call-hook 'plain-tex-mode-hook))
  
(defun tex-insert-end ()
  (interactive)
  (let
      ((p (forward-char -1))
       (depth 0))
    (if (catch 'foo
	  (while (re-search-backward "\\\\(end|begin){([^}]+)" p)
	    (if (= (get-char (match-start 1)) #\b)
		;; no end
		(if (zero? depth)
		    (throw 'foo t)
		  (set! depth (1- depth)))
	      (set! depth (1+ depth)))
	    (set! p (forward-char -1 (match-start)))))
	(format (current-buffer) "\\end{%s}\n" (copy-area (match-start 2)
							  (match-end 2)))
      (tex-insert-braces "end")
      (error "Can't find a command to \\end"))))

(defun tex-insert-braces (#!optional command)
  (interactive)
  (let
      ((count (and current-prefix-arg
		   (prefix-numeric-argument current-prefix-arg))))
    (if (null? count)
	(progn
	  (insert (concat #\\ command "{}"))
	  (goto (forward-char -1)))
      (if (> count 0)
	  (progn
	    (insert (concat #\\ command #\{))
	    (goto (forward-word count))
	    (insert "}"))
	(goto (forward-word count))
	(insert (concat #\\ command #\{))
	(goto (forward-word (- count)))
	(insert "}")))))

(defun tex-move-over-braces ()
  (interactive)
  (goto (forward-char 1 (char-search-forward #\}))))

(defun tex-ispell-ignore-word-hook (word start end)
  (declare (unused word end))
  (= (get-char (forward-char -1 start)) #\\))
