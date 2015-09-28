;;;; jade.jl -- Standard initialisation script
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

(provide 'jade)

(message "Initialising; wait..." t)

(set! *user-module* 'jade)

(require 'rep.regexp)
(require 'rep.system)
(require 'rep.io.files)
(require 'rep.io.processes)

;; Load standard libraries
(load "loadkeys")
(load "popup-menus")
(load "windows")
(load "buffers")
(load "modes")
(load "edit")

;; ignore file errors on stdio streams
(when (variable-bound? 'set-file-ignore-errors)
  (set-file-ignore-errors (stdin-file) t)
  (set-file-ignore-errors (stdout-file) t)
  (set-file-ignore-errors (stderr-file) t))

(defun jade-load-all (file)
  (load-all file (lambda (f) (load f nil t))))

;; Install all autoload hooks
(jade-load-all "autoload")

;; Do and operating- and window-system initialisation
(jade-load-all (concat "ws-" (symbol-name window-system)))

;; Load site specific initialisation. Errors here are trapped since
;; they're probably not going to leave the editor in an unusable state
(unless (get-command-line-option "--no-rc")
  (condition-case error-data
      (progn
	;; First the site-wide stuff
	(jade-load-all "site-init")

	;; then the users rep configuration, or site-wide defaults
	(or (load (concat (user-home-directory) ".reprc") t t t)
	    (load "rep-defaults" t))

	;; then the jade specific user configuration
	(or (load (concat (user-home-directory) ".jaderc") t t t)
	    (load "jade-default" t)))
    (error
     (format (stderr-file) "error in local config--> %S\n" error-data))))

(message (concat "Built " jade-build-id))

;; Set up the first window as command shell type thing
(with-buffer default-buffer
  (lisp-mode))

;; Print a message in the first buffer
(format default-buffer ";; Jade %s, Copyright (C) 1993-2011 John Harper\n;; Jade comes with ABSOLUTELY NO WARRANTY; for details see the file COPYING\n\n"	jade-version)

;; Don't want it in the undo list
(set-buffer-undo-list nil)
(set-buffer-modified default-buffer nil)

;; Use all arguments which are left.
(do () ((null? *command-line-args*))
  (let ((arg (car *command-line-args*)))
    (set! *command-line-args* (cdr *command-line-args*))
    (cond ((equal? "-f" arg)
	   (set! arg (car *command-line-args*))
	   (set! *command-line-args* (cdr *command-line-args*))
	   ((variable-ref (read-from-string arg))))
	  ((equal? "-l" arg)
	   (set! arg (car *command-line-args*))
	   (set! *command-line-args* (cdr *command-line-args*))
	   (cond ((file-exists? arg)
		  (load arg nil t t))
		 ((string-match "\\.jlc?$" arg)
		  (load arg))
		 (t (require (intern arg)))))
	  ((equal? "-q" arg)
	   (throw 'quit 0))
	  (t
	   (find-file arg)))))
