;;;; sawmill-minor-mode.jl -- sawmill interactions
;;;  Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
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

(eval-when-compile (require 'info))
(eval-when-compile (require 'find-autoloads))

(defvar smm-active nil)
(make-variable-buffer-local 'smm-active)

(setq minor-mode-alist (cons '(smm-active " Sawmill") minor-mode-alist))
(setq minor-mode-keymap-alist (cons '(smm-active . smm-keymap)
				    minor-mode-keymap-alist))

(defvar smm-keymap (bind-keys (make-sparse-keymap)
		     "C-M-x" 'smm-eval-print-sexp
		     "C-j" 'smm-eval-insert-sexp))

(defun smm-eval (form #!optional module)
  ;; avoid requiring sawfish.client at compile-time
  ((lambda (x) (require x)) 'sawfish.client)
  (declare (bound sawfish-client-eval))
  (if (not module)
      (sawfish-client-eval form t)
    (sawfish-client-eval `(eval-in ',form ',module) t)))

(defun smm-identify-module (#!optional point buf)
  "Return the name of the Lisp module that the cursor is in."
  (if (or (re-search-backward
	   "\\(define-structure[ \t\n]+(.+?)[ \t\n]" point buf)
	  (re-search-backward
	   "\\(declare \\(in-module[ \t\n]+(.+?)[ \t\n]*\\)" point buf)
	  (re-search-forward
	   "\\(define-structure[ \t\n]+(.+?)[ \t\n]" point buf)
	  (re-search-forward
	   "\\(declare \\(in-module[ \t\n]+(.+?)[ \t\n]" point buf))
      (intern (expand-last-match "\\1"))
    nil))

(defun smm-eval-sexp ()
  "Evaluates the Lisp expression before the cursor and returns its value."
  (interactive)
  (goto (lisp-backward-sexp))
  (smm-eval (read (current-buffer)) (smm-identify-module)))

(defun smm-eval-insert-sexp ()
  "Evaluates the Lisp expression before the cursor, then inserts its value
into the buffer."
  (interactive)
  (format (current-buffer) "\n%s" (smm-eval-sexp)))

(defun smm-eval-print-sexp ()
  "Evaluates the Lisp expression before the cursor, then displays its value
in the status line."
  (interactive)
  (princ (smm-eval-sexp) t))

(defun sawmill-describe-function (fn #!optional index-node)
  (interactive
   (list (prompt-for-string "Sawmill function:" (symbol-at-point))
	 "Function Index"))
  (when fn
    (info-visit-node "sawmill" index-node fn)))

(defun sawmill-describe-variable (fn)
  (interactive
   (list (prompt-for-string "Sawmill variable:" (symbol-at-point))))
  (sawmill-describe-function fn "Variable Index"))

(defun sawmill-autoload-finder (point buf module)
  (cond ((looking-at
	  "[ \t]*\\(define-command(-to-screen)? '([^ ]+)" point buf)
	 (let ((form (read (cons buf point))))
	   (let ((name (cadr form))
		 (keys (cdddr form)))
	     ;; remove #:spec keys
	       (let ((tem (memq #:spec keys)))
		 (when tem
		   (rplacd tem (cddr tem))
		   (setq keys (delq (car tem) keys))))
	     (prin1-to-string `(autoload-command ,name ',module ,@keys)))))))

;;;###autoload
(defun sawmill-minor-mode ()
  (interactive)
  (if smm-active
      (progn
	(setq smm-active nil)
	(setq autoload-finder nil)
	(setq info-documentation-files
	      (delete "sawmill" info-documentation-files)))
    (setq smm-active t)
    (make-local-variable 'autoload-finder)
    (setq autoload-finder sawmill-autoload-finder)
    (make-local-variable 'info-documentation-file)
    (setq info-documentation-files
	  (if (boundp 'info-documentation-files)
	      (cons "sawmill" info-documentation-files) (list "sawmill")))))

;;;###autoload
(defun sawmill-console ()
  (interactive)
  (let
      ((buffer (open-buffer "*sawmill*")))
    (goto-buffer buffer)
    (lisp-mode)
    (sawmill-minor-mode)))

(put 'defcustom 'lisp-indent 'defun)
(put 'defgroup 'lisp-indent 'defun)
(put 'define-command 'lisp-indent 'defun)
(put 'define-placement-mode 'lisp-indent 'defun)
(put 'define-focus-mode 'lisp-indent 'defun)
(put 'define-match-window-property 'lisp-indent 'defun)
(put 'define-match-window-setter 'lisp-indent 'defun)
(put 'define-match-window-formatter 'lisp-indent 'defun)
