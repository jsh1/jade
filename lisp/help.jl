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
    (start-of-buffer) help-buffer)
    (goto-buffer help-buffer)
    (goto (start-of-buffer))
    (help))

;; Setup the help-buffer for insertion of the help text
(defmacro help-wrapper (&rest forms)
  (list 'with-view '(other-view)
	'(goto-buffer help-buffer)
	'(clear-buffer)
	(cons 'progn forms)
	'(goto (start-of-buffer))
	'(shrink-view-if-larger-than-buffer)))

(defun apropos-function (regexp)
  (interactive "sRegular expression:")
  (help-wrapper
   (format help-buffer "Apropos for expression %S:\n" regexp)
   (print (apropos regexp 'fboundp) help-buffer)))

(defun apropos-variable (regexp)
  (interactive "sRegular expression:")
  (help-wrapper
   (format help-buffer "Apropos for expression %S:\n" regexp)
   (print (apropos regexp 'boundp) help-buffer)))

(defun describe-keymap ()
  "Print the full contents of the current keymap (and the keymaps that
it leads to)."
  (interactive)
  (let
      ((old-buf (current-buffer))
       (km-list keymap-path))
    (help-wrapper
     (print-keymap km-list old-buf))))

(defun describe-function (fun &aux doc)
  "Display the documentation of a function, macro or special-form."
  (interactive "aDescribe function:")
  (setq doc (documentation fun))
  (help-wrapper
   (let*
       ((fval (symbol-function fun))
	(type (cond
	       ((special-form-p fval)
		"Special Form")
	       ((macrop fval)
		"Macro")
	       ((subrp fval)
		"Built-in Function")
	       (t
		"Function"))))
     ;; Check if it's been compiled.
     (when (or (bytecodep fval)
	       (and (consp fval) (assq 'jade-byte-code fval)))
       (setq type (concat "Compiled " type)))
     (format help-buffer "\n%s: %s\n\n" type fun)
     (when (fboundp fun)
       (when (or (consp fval) (bytecodep fval))
	 ;; A Lisp function or macro, print its argument spec.
	 (let
	     ((lambda-list (if (consp fval)
			       (nth (if (eq (car fval) 'macro) 2 1) fval)
			     (aref fval 0))))
	   (prin1 fun help-buffer)
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
   (insert "\n")))

(defun describe-variable (var)
  (interactive "vDescribe variable:")
  (let
      ((doc (documentation var t))
       (old-buf (current-buffer)))
    (help-wrapper
     (format help-buffer
	     "\n%s: %s\nCurrent value: %S\n\n%s\n"
	     (if (const-variable-p var)
		 "Constant"
	       "Variable")
	     (symbol-name var)
	     (with-buffer old-buf (symbol-value var t))
	     (or doc "Undocumented.")))))

;;;###autoload
(defun describe-mode ()
  "Print the help text for the current editing mode."
  (interactive)
  (let*
      ((mode major-mode)
       (doc (documentation mode)))
    (help-wrapper
     (when (stringp doc)
       (format help-buffer "\n%s\n" doc)))))

;;;###autoload
(defun documentation (symbol &optional is-variable)
  "Returns the documentation-string for SYMBOL. If IS-VARIABLE is t the
documentation for the variable stored in SYMBOL is returned, else
the function doc is provided."
  (when (symbolp symbol)
    (let
	(doc)
      (if is-variable
	  (setq doc (or (subr-documentation symbol t)
			(get symbol 'variable-documentation)))
	(when (eq (car (symbol-function symbol)) 'autoload)
	  (load (nth 1 (symbol-function symbol))))
	(setq symbol (symbol-function symbol))
	(cond
	 ((or (subrp symbol)
	      (bytecodep symbol))
	  (setq doc (subr-documentation symbol)))
	 ((or (eq 'macro (car symbol)) (eq 'special (car symbol)))
	  (setq doc (nth 3 symbol)))
	 (t
	  (setq doc (nth 2 symbol)))))
      (when (numberp doc)
	(setq doc (get-documentation doc)))
      (when (stringp doc)
	doc))))

;;;###autoload
(defun document-var (symbol doc-string)
  "Sets the `variable-documentation' property of SYMBOL to DOC-STRING."
  (put symbol 'variable-documentation doc-string)
  symbol)

;;;###autoload
(defun get-documentation (offset)
  "Return the documentation string starting at position OFFSET in the file
of such strings."
  (let
      ((file (open-file documentation-file 'read)))
    (when file
      (unwind-protect
	  (let
	      ((strings '())
	       line done)
	    (seek-file file offset 'start)
	    (while (and (not done) (setq line (read-line file)))
	      (if (string-match "\f" line)
		  (setq strings (cons (substring line 0 (match-start)) strings)
			done t)
		(setq strings (cons line strings))))
	    (apply 'concat (nreverse strings)))
	(close-file file)))))

;;;###autoload
(defun add-documentation (string)
  "Adds a documentation string STRING to the file of such strings, returning
the integer offset at which the string starts in the file."
  (let
      ((file (open-file documentation-file 'append)))
    (when file
      (unwind-protect
	  (prog1
	      (seek-file file)
	    (write file string)
	    (write file ?\f))
	(close-file file)))))
