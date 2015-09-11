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

(require 'lisp-doc)
(provide 'help)

(defvar help-keymap
  (bind-keys (make-sparse-keymap)
    "SPC" 'next-screen
    "BS" 'prev-screen
    "q" 'kill-current-buffer))

(defvar help-prompt-keymap
  (bind-keys (make-sparse-keymap)
    "a" 'apropos-function
    "b" 'describe-keymap
    "e" 'apropos-variable
    "f" 'describe-function
    "C-f" 'info-describe-function
    "h" 'help-help
    "Ctrl-h" 'help-help
    "i" 'info
    "k" 'describe-key
    "w" 'where-is
    "m" 'describe-mode
    "?" 'help-help
    "v" 'describe-variable
    "C-v" 'info-describe-variable
    "SPC" '(progn (next-screen) (help))
    "BS" '(progn (prev-screen) (help))))

;;;###autoload
(defun help ()
  "Entrance to the online-help system."
  (interactive)
  (message "Type: a b f h i k m v -- h for more help")
  (next-keymap-path '(help-prompt-keymap)))

(defun help-setup ()
  "Help mode:\n
Major mode for displaying online help. Local bindings are:\n
\{help-keymap}"
  (let
      ((buffer (open-buffer "*Help*")))
    (with-buffer buffer
      (unless (eq? major-mode 'help-setup)
	(setq local-keymap 'help-keymap
	      mode-name "Help"
	      major-mode 'help-setup)
	(set-buffer-record-undo nil)
	(set-buffer-read-only nil t)))
    buffer))

;; Setup the help-buffer for insertion of the help text
;; *standard-output* is bound to the correct output stream
(defmacro help-wrapper (#!rest forms)
  `(with-view (other-view)
     (goto-buffer (help-setup))
     (clear-buffer)
     (let
	 ((*standard-output* (current-buffer))
	  (inhibit-read-only t))
       (progn ,@forms)
       (goto (start-of-buffer))
       (shrink-view-if-larger-than-buffer))))
  
(defun help-help ()
  "Displays some text describing the options in the help system."
  (interactive)
  (help-wrapper
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
	View the documentation and value of a variable.

w   `where-is'
	Prompt for the name of a command, then display all key bindings
	that invoke may be used to invoke it.")
   (help)))

(defun apropos-output (symbols)
  (mapc (lambda (sym)
	  (describe-value (symbol-value sym t) sym)) symbols))

(defun apropos-function (regexp #!optional all-functions)
  (interactive "sRegular expression:\nP")
  (help-wrapper
   (format *standard-output* "Apropos %s `%s':\n\n"
	   (if all-functions "function" "command") regexp)
   (apropos-output (apropos regexp (if all-functions
				       (lambda (s)
					 (and (bound? s)
					      (function? (symbol-value s))))
				     commandp)))))

(defun apropos-variable (regexp)
  (interactive "sRegular expression:")
  (help-wrapper
   (format *standard-output* "Apropos variable `%s':\n" regexp)
   (apropos-output (apropos regexp 'bound?))))

(defun describe-keymap ()
  "Print the full contents of the current keymap (and the keymaps that
it leads to)."
  (interactive)
  (let
      ((buffer-in-scope (current-buffer)))
    (help-wrapper
     (print-keymap nil buffer-in-scope))))

(defun describe-function (fun)
  "Display the documentation of a function, macro or special-form."
  (interactive
   (list (prompt-for-function "Describe function:" (symbol-at-point))))
  (let
      ((doc (documentation fun)))
    (help-wrapper
     (insert "\n")
     (describe-value (symbol-value fun) fun)
     (insert "\n")
     (insert (if doc (substitute-command-keys doc) "Undocumented."))
     (insert "\n"))))

(defun describe-variable-1 (var #!optional in-buffer)
  (format *standard-output*
	  "\nVariable: %s\nCurrent value: %S\n\n"
	  (symbol-name var)
	  (with-buffer (or in-buffer (current-buffer)) (symbol-value var t))))

(defun describe-variable (var)
  (interactive
   (list (prompt-for-variable "Describe variable:" (symbol-at-point))))
  (let
      ((doc (documentation var))
       (old-buf (current-buffer)))
    (help-wrapper
     (describe-variable-1 var old-buf)
     (format *standard-output* "%s\n" (or doc "Undocumented.")))))

;;;###autoload
(defun describe-mode ()
  "Print the help text for the current editing mode."
  (interactive)
  (let*
      ((mode major-mode)
       (doc (documentation mode)))
    (when doc
      (setq doc (substitute-command-keys doc)))
    (help-wrapper
     (when (string? doc)
       (format *standard-output* "\n%s\n" doc)))))

;;;###autoload
(defun substitute-command-keys (string)
  "Replace special marker expressions in STRING with expansions describing
the current key binding environment. The following markers are supported:

\\[COMMAND]		Replaced by the name of a key that is bound to
			 the command named COMMAND
\\{KEYMAP}		Replaced by a list describing all key bindings
			 defined by KEYMAP
\\{KEYMAP,PREFIX}	Similar to \\{KEYMAP}, except that all binding
			 descriptions are prefixed by PREFIX, a string
			 naming a key bound to KEYMAP
\\<KEYMAP>		Replaced by a null string; this notes that all
			 following \\[..] expansions should search KEYMAP
			 for their bindings.

This function is used by the describe-mode command, hence the documentation
strings of modes may contain any of these expansions."
  (let
      ((out nil)
       (point 0)
       (whereis-rel nil)
       (*print-escape* t))
    (while (string-match "\\\\[[{<]([^]}>,]+)(,([^]}>]+))?[]}>]" string point)
      (setq out (cons (substring string point (match-start)) out)
	    point (match-end))
      (let
	  ((symbol (intern (expand-last-match "\\1")))
	   (arg (expand-last-match "\\3"))
	   (type (aref string (1- point))))
	(cond
	 ((= type #\])
	  ;; where-is SYMBOL
	  (let
	      ((result (where-is symbol whereis-rel)))
	    (setq out (cons (or (car result)
				(concat "Meta-x " (symbol-name symbol)))
			    out))))
	 ((= type #\})
	  ;; print-keymap SYMBOL
	  (map-keymap (lambda (k prefix)
			(setq out (cons (format nil "%-24s %S\n"
						(concat "  "
							arg
							(if (string=? arg "")
							    "" " ")
							(or prefix "")
							(if prefix " " "")
							(event-name (cdr k)))
						(car k))
					out)))
		      (symbol-value symbol)))
	 ((= type #\>)
	  ;; next where-is is relative to keymap SYMBOL
	  (setq whereis-rel (symbol-value symbol))))))
    (setq out (cons (substring string point) out))
    (apply concat (nreverse out))))
