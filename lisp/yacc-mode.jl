;;;; yacc-mode.jl -- Major mode for editing yacc grammars
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

(require 'c-mode)
(require 'text-mode)
(provide 'yacc-mode)

(defvar yacc-mode-c-minor t
  "When non-nil yacc-mode installs c-mode as a minor-major mode in certain
parts of yacc buffers. If the variable's value is `t' all parts of the
buffer which are though to be C are configured as such, otherwise only
the C sections at the top and bottom of the file are used.")

(defvar yacc-mode-scan-when-idle t
  "When non-nil each time the editor becomes idle, and the buffer has been
modified, rescan for C regions. This variable is only consulted when the
mode is first initialised.")

(defvar yacc-mode-last-scan nil)
(make-variable-buffer-local 'yacc-mode-last-scan)

(defvar yacc-mode-keymap (bind-keys (make-sparse-keymap)
			   "TAB" 'text-mode-indent-tab))

(defvar yacc-mode-ctrl-c-keymap (bind-keys (make-sparse-keymap)
				  "C-l" 'yacc-mode-make-c-minor))

;;;###autoload
(defun yacc-mode ()
  "Yacc Mode:\n
Major mode for editing yacc grammars. Mode-local commands are:\n
\\{yacc-mode-keymap}\\{yacc-mode-ctrl-c-keymap,C-c}

The variable `yacc-mode-c-minor' controls whether or not the parts of the
Yacc grammar that are written in C have the c-mode installed in them. Set
the variable to the symbol `t' to recognize all C sections, if otherwise
non-nil only the declarations at the start of the grammar and the additional
C code at the end are recognized.

The \\[yacc-mode-make-c-minor] command reparses the buffer for C sections."
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill))
  (setq mode-name "Yacc"
	major-mode 'yacc-mode
	major-mode-kill 'yacc-mode-kill
	mode-comment-fun 'c-insert-comment
	mode-forward-exp 'c-forward-exp
	mode-backward-exp 'c-backward-exp
	paragraph-separate "^[\n\t\f ]*\n"
	paragraph-start "^([\n\t\f ]*\n|[a-zA-z0-9_]+:)"
	ctrl-c-keymap yacc-mode-ctrl-c-keymap
	keymap-path (cons 'yacc-mode-keymap keymap-path))
  (call-hook 'yacc-mode-hook)
  (when yacc-mode-scan-when-idle
    (make-local-variable 'idle-hook)
    (add-hook 'idle-hook 'yacc-mode-idle-function))
  (yacc-mode-make-c-minor))

(defun yacc-mode-kill ()
  (yacc-mode-delete-minors)
  (kill-all-local-variables))

(defun yacc-mode-make-c-minor ()
  "Rescan the current buffer for C regions embedded in the yacc grammar.
Give any such regions minor-major c-modes."
  (interactive)
  (let
      (rules-start rules-end)
    (save-excursion
      (when yacc-mode-c-minor
	;; Look for `%{ C DECLS %}' and section after second `%%' marker
	(when (re-search-forward "^%{" (start-of-buffer))
	  (let
	      ((start (match-end)))
	    (when (re-search-forward "^%}" start)
	      (unless (eq (buffer-get 'minor-major start) 'c-mode)
		(extent-put 'front-sticky t
			    (minor-major-mode 'c-mode start (match-start)))))))
	(when (and (re-search-forward "^%%" (start-of-buffer))
		   (setq rules-start (match-end))
		   (setq rules-end (re-search-forward "^%%" (match-end))))
	  (unless (eq (buffer-get 'minor-major (match-end)) 'c-mode)
	    (extent-put 'front-sticky t
			(minor-major-mode 'c-mode
					  (match-end) (end-of-buffer))))))
      (when (eq yacc-mode-c-minor t)
	;; Scan all rules for C actions
	(save-restriction
	  (restrict-buffer (or rules-start (start-of-buffer))
			   (or rules-end (end-of-buffer)))
	  (goto (restriction-start))
	  (let
	      ((tem (restriction-start)))
	    (while (re-search-forward "[\t\n ]({)" tem)
	      (let
		  ((start (match-start 1))
		   end)
		(setq tem (match-end))
		(when (and (not (eq (buffer-get 'minor-major tem) 'c-mode))
			   (setq end (condition-case nil
					 (c-forward-exp 1 start)
				       (error))))
		  (extent-put 'rear-sticky nil
			      (minor-major-mode 'c-mode start end))
		  (setq tem end))))))))))

(defun yacc-mode-idle-function ()
  (when (or (not yacc-mode-last-scan) (> (buffer-changes) yacc-mode-last-scan))
    (yacc-mode-make-c-minor)
    ;; Ensure that only one copy of yacc-mode-last-scan per buffer
    ;; is maintained
    (extent-set 'yacc-mode-last-scan (buffer-changes) (extent-root))))

(defun yacc-mode-delete-minors ()
  "Delete all extents in the current buffer that use c-mode as a minor-major
mode."
  (interactive)
  (let
      (extents)
    (map-extents #'(lambda (e)
		     (when (eq (extent-get 'minor-major e) 'c-mode)
		       (setq extents (cons e extents))))
		 (start-of-buffer) (end-of-buffer))
    (mapc 'delete-extent extents)))
