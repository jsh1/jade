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

;; Load standard libraries
(load "loadkeys")
(load "windows")
(load "buffers")
(load "modes")
(load "edit")

;; Install all autoload hooks. This is done last so that it works
;; when dumped. We load autoload.jl to ensure that we don't get a
;; compiled (and possibly out of date) version
(load-all "autoload.jl" t)

;; Do and operating- and window-system initialisation
(load-all (concat "os-" (symbol-name operating-system)) t)
(load-all (concat "ws-" (symbol-name window-system)) t)

;; Load site specific initialisation. Errors here are trapped since
;; they're probably not going to leave the editor in an unusable state
(if (not (member "--no-rc" command-line-args))
    (condition-case error-data
	(progn
	  ;; First the site-wide stuff
	  (load-all "site-init")

	  ;; then the users rep configuration, or site-wide defaults
	  (or (load (concat (user-home-directory) ".reprc") t t)
	      (load "rep-defaults" t))

	  ;; then the jade specific user configuration
	  (or (load (concat (user-home-directory) ".jaderc") t t)
	      (load "jade-default" t)))
      (error
       (format (stderr-file) "error in local config--> %S\n" error-data)))
  (setq command-line-args (delete "--no-rc" command-line-args)))

(message (concat "Built " jade-build-id))

;; Set up the first window as command shell type thing
(with-buffer default-buffer
  (lisp-mode))

;; Print a message in the first buffer
(format default-buffer ";; Jade %s, Copyright (C) 1993-1999 John Harper\n;; Jade comes with ABSOLUTELY NO WARRANTY; for details see the file COPYING\n\n"	jade-version)

;; Don't want it in the undo list
(setq buffer-undo-list nil)
(set-buffer-modified default-buffer nil)

;; Use all arguments which are left.
(let
    (arg)
  (while (setq arg (car command-line-args))
    (setq command-line-args (cdr command-line-args))
    (cond
      ((equal "-f" arg)
       (setq arg (car command-line-args)
	     command-line-args (cdr command-line-args))
       (funcall (read-from-string arg)))
      ((equal "-l" arg)
       (setq arg (car command-line-args)
	     command-line-args (cdr command-line-args))
       (load arg))
      ((equal "-q" arg)
       (throw 'quit 0))
      (t
       (find-file arg)))))
