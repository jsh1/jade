;;;; help.jl -- Online help system
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

(provide 'help)

(defvar help-buffer (make-buffer "*Help*"))
(set-buffer-special help-buffer t)

(defvar help-keymap (make-keylist))
(bind-keys help-keymap
  "SPC" 'next-screen
  "BS" 'prev-screen
  "q" 'bury-buffer)

(defvar help-prompt-keymap (make-keylist))
(bind-keys help-prompt-keymap
  "a" 'apropos-function
  "b" 'describe-keymap
  "e" 'apropos-variable
  "f" 'describe-function
  "h" 'help-help
  "Ctrl-h" 'help-help
  "i" 'info
  "k" 'describe-key
  "m" 'describe-mode
  "?" 'help-help
  "v" 'describe-variable
  "SPC" '(progn (next-screen) (help))
  "BS" '(progn (prev-screen) (help)))

(with-buffer help-buffer
  (setq keymap-path '(help-keymap global-keymap)
	mode-name "Help"
	buffer-record-undo nil))

;;;###autoload
(defun help ()
  "Entrance to the online-help system."
  (interactive)
  (message "Type: a b f h i k m v -- h for more help")
  (setq next-keymap-path '(help-prompt-keymap)))

(defun help-help ()
  "Displays some text describing the options in the help system."
  (interactive)
  (clear-buffer help-buffer)
  (insert
    "\nHelp mode -- Type one of the following:\n
a   `apropos-function'
	Search for functions which match a regular expression.

b   `describe-keymap'
	Print the key bindings which are currently active.

f   `describe-function'
	View the documentation for a particular function.

h   `help-help'
	Display this text.

i   `info'
	Enter the info hypertext viewer.

k   `describe-key'
	Display the command (and its documentation) that a particular
	sequence of keys is currently bound to.

m   `describe-mode'
	Show the documentation for the edit mode of the current buffer.

v   `describe-variable'
	View the documentation and value of a variable."
    (buffer-start) help-buffer)
    (goto-buffer help-buffer)
    (goto-buffer-start)
    (help))

;; Setup the help-buffer for insertion of the help text
(defun help-setup ()
  (goto-other-view)
  (clear-buffer help-buffer)
  (goto-buffer help-buffer)
  (insert "\n----\nType `q' to return to the buffer you were in.")
  (goto-buffer-start))

(defun apropos-function (regexp)
  (interactive "sRegular expression:")
  (help-setup)
  (format help-buffer "Apropos for expression %S:\n" regexp)
  (print (apropos regexp 'fboundp) help-buffer)
  (goto-buffer-start))

(defun apropos-variable (regexp)
  (interactive "sRegular expression:")
  (help-setup)
  (format help-buffer "Apropos for expression %S:\n" regexp)
  (print (apropos regexp 'boundp) help-buffer)
  (goto-buffer-start))

(defun describe-keymap ()
  "Print the full contents of the current keymap (and the keymaps that
it leads to)."
  (interactive)
  (let
      ((old-buf (current-buffer))
       (km-list keymap-path))
    (help-setup)
    (print-keymap km-list old-buf)
    (goto-buffer-start)))

(defun describe-function (fun &aux doc)
  "Display the documentation of a function, macro or special-form."
  (interactive "aDescribe function:")
  (setq doc (documentation fun))
  (help-setup)
  (let*
      ((fval (symbol-function fun))
       (type (cond
	      ((special-form-p fval)
	       "Special Form")
	      ((subrp fval)
	       "Built-in Function")
	      ((eq (car fval) 'macro)
	       "Macro")
	      (t
	       "Function"))))
    (when (consp fval)
      ;; Check if it's been compiled.
      (when (assq 'jade-byte-code fval)
	;; compiled forms
	(setq type (concat "Compiled " type))))
    (format help-buffer "\n%s: %s\n\n" type fun)
    (when (fboundp fun)
      (unless (subrp fval)
	;; A Lisp function or macro, print its argument spec.
	(let
	    ((lambda-list (nth (if (eq (car fval) 'macro) 2 1) fval)))
	  (prin1 fun help-buffer)
	  (when (eq (car lambda-list) 'lambda)
	    ;; A macro
	    (setq lambda-list (cdr lambda-list)))
	  ;; Print the arg list (one at a time)
	  (while lambda-list
	    (let
		((arg-name (symbol-name (car lambda-list))))
	      ;; Unless the argument starts with a `&' print it in capitals
	      (unless (= (aref arg-name 0) ?&)
		(setq arg-name (translate-string (copy-sequence arg-name)
						 upcase-table)))
	      (format help-buffer " %s" arg-name))
	    (setq lambda-list (cdr lambda-list)))
	  (insert "\n\n")))))
  (insert (or doc "Undocumented."))
  (insert "\n")
  (goto-buffer-start))

(defun describe-variable (var)
  (interactive "vDescribe variable:")
  (let
      ((doc (documentation var t))
       (old-buf (current-buffer)))
    (help-setup)
    (format help-buffer
	    "\n%s: %s\nCurrent value: %S\n\n%s\n"
	    (if (const-variable-p var)
		"Constant"
	      "Variable")
	    (symbol-name var)
	    (with-buffer old-buf (symbol-value var t))
	    (or doc "Undocumented."))
      (goto-buffer-start)))

;;;###autoload
(defun describe-mode ()
  "Print the help text for the current editing mode."
  (interactive)
  (let
      ((mode major-mode))
    (help-setup)
    (let
        ((doc (documentation mode)))
      (when (stringp doc)
	(format help-buffer "\n%s\n" doc)
	(goto-buffer-start)))))

;;;###autoload
(defun documentation (symbol &optional is-variable)
  "Returns the documentation-string for SYMBOL. If IS-VARIABLE is t the
documentation for the variable stored in SYMBOL is returned, else
the function doc is provided."
  (when (symbolp symbol)
    (let
	(doc)
      (if is-variable
	  (setq doc (get symbol 'variable-documentation))
	(when (eq (car (symbol-function symbol)) 'autoload)
	  (load (nth 1 (symbol-function symbol))))
	(setq symbol (symbol-function symbol))
	(cond
	 ((subrp symbol)
	  (setq doc (subr-documentation symbol)))
	 ((or (eq 'macro (car symbol)) (eq 'special (car symbol)))
	  (setq doc (nth 3 symbol)))
	 (t
	  (setq doc (nth 2 symbol)))))
      (when (numberp doc)
	(setq doc (get-doc-string doc)))
      (when (stringp doc)
	doc))))

;;;###autoload
(defun document-var (symbol doc-string)
  "Sets the `variable-documentation' property of SYMBOL to DOC-STRING."
  (put symbol 'variable-documentation doc-string)
  symbol)
