;;;; file-subst.jl -- Build files from templates
;;;  Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'file-subst)

(defvar file-subst-regexp "@([^@]*)@"
  "Regexp matching a substitution to perform.")

(defvar file-subst-map-regexp "^(.*)\\.in$"
  "Default regexp for map-file-subst. The first subexpression should give
the name of the output file.")

(defvar file-subst-vars nil
  "Alist of file-subst expansions")

;;;###autoload
(defun file-subst (input-file output-file)
  "Use the template file INPUT-FILE to create a new file OUTPUT-FILE in
an editor buffer. On completion, OUTPUT-FILE will be left, unsaved, as
the current buffer.

Expansions are included in the body of INPUT-FILE as strings \"@FORM@\"
where FORM is a Lisp form to be evaluated. The entire string is replaced
by the value of FORM, if it evaluates to a string or a number. Otherwise
the input string is just deleted.

While FORM is being evaluated, the cursor is set at the position from
which the expansion template was deleted. This provides an alternative
to FORM returning the expansion--it may just insert the expansion into
the current buffer and return something that isn't a string or a number.
E.g. to include a file, have something like `@(insert-file \"FOO\")@'

To avoid the need to set free variables in files, a special function
`file-subst-set' gives symbols temporary values for the duration of the
file-subst function. If an expansion FORM matches any symbol that has
previously had a value given to it in this way, the file-local value
will be used.

Finally, note that the string \"@@\" expands to a single @ character."
  (interactive (let*
		   ((input (prompt-for-file "Input file:" t
					    (buffer-file-name)))
		    (default-out (when (string-match file-subst-map-regexp
						     input)
				   (substring input
					      (match-start 1) (match-end 1))))
		    (output (prompt-for-file "Output file:" nil
					     default-out default-out)))
		 (list input output)))
  (let*
      ((file-subst-vars nil)
       (output-buffer (find-file output-file))
       (p (pos 0 0)))
    (file-subst-set 'date (current-time-string nil "%B %d, %Y"))
    ;; Now in output-buffer
    (clear-buffer)
    (insert-file-contents input-file)
    (while (setq p (re-search-forward file-subst-regexp p))
      (if (equal (match-start 1) (match-end 1))
	  ;; Null expansion, shortcut for @
	  (progn
	    (delete-area p (match-start 1))
	    (setq p (forward-char 1 p)))
	;; Found an expansion to perform
	(let
	    ((value (read-from-string (copy-area (match-start 1)
						 (match-end 1)))))
	  (delete-area p (match-end))
	  (goto p)
	  (setq value (if (and (symbolp value) (assq value file-subst-vars))
			  (cdr (assq value file-subst-vars))
			(eval value)))
	  (when (or (stringp value) (numberp value))
	    (goto p)
	    (princ value output-buffer)))))))

;;;###autoload
(defun map-file-subst (file-list &optional regexp force)
  "Call `file-subst' for each file named in FILE-LIST, using REGEXP to
find the output file names. When REGEXP is undefined, use the value of
the `file-subst-map-regexp' variable.

Files are only built if FORCE is non-nil or the output file is older
than the input file (or it doesn't exist). No files are saved, they're
left in editor buffers."
  (mapc #'(lambda (f)
	    (let
		((out (if (string-match (or regexp file-subst-map-regexp) f)
			  (substring f (match-start 1) (match-end 1))
			(prompt-for-string
			 (format nil "Output file for `%s'" f) f))))
	      (when (or force (file-newer-than-file-p f out))
		(file-subst f out))))
	file-list))

(defun file-subst-set (var value)
  "Set the value of symbol VAR when used as a file substitution expansion
in the current file, to VALUE."
  (let
      ((cell (assq var file-subst-vars)))
    (if cell
	(rplacd cell value)
      (setq file-subst-vars (cons (cons var value) file-subst-vars)))))
