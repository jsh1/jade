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

(defvar sawmill-client-program "sawfish-client")

(defun smm-eval (form)
  (let*
      ((output (make-string-output-stream))
       (process (make-process output))
       (print-escape t))
    (if (zerop (call-process process nil sawmill-client-program
			     "-w" "-e" (format nil "%S" form)))
	;; success
	(get-output-stream-string output)
      (error "can't call sawmill-client"))))

(defun smm-eval-sexp ()
  "Evaluates the Lisp expression before the cursor and returns its value."
  (interactive)
  (goto (lisp-backward-sexp))
  (smm-eval (read (current-buffer))))

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

(defun sawmill-describe-function (fn &optional index-node)
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
  (cond ((looking-at "[ \t]*\\(define-command(-to-screen)? '([^ ]+)" point buf)
	 (let ((name (expand-last-match "\\2"))
	       (type nil))
	   (condition-case nil
	       (with-buffer buf
		 (save-excursion
		   (goto (match-end))
		   (goto (forward-exp 3))
		   (let ((end (cursor-pos)))
		     (goto (backward-exp 1))
		     (setq type (copy-area (cursor-pos) end)))))
	     (error))
	   (if (and type (not (string= type "")))
	       (format nil "(autoload-command '%s '%s %s)" name module type)
	     (format nil "(autoload-command '%s '%s)" name module))))
	     
	((looking-at "[ \t]*\\(define-([^ ]+) '([^ ]+)" point buf)
	 (format nil "(autoload-%s '%s '%s)"
		 (expand-last-match "\\1") (expand-last-match "\\2") module))))

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
