;;;; find-url.jl -- Code to follow a [selected] url to its destination
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


;; Configuration

(defvar find-url-alist '(("^http:" . find-url-external)
			 ("^file:" . find-url-file)
			 ("^ftp:" . find-url-ftp)
			 ("^telnet:" . find-url-telnet))
  "Alist of (REGEXP . FUNC) matching URLs to the Lisp functions used to
display them. If none match, the first in the list is used.")

(defvar find-url-external-command
  "netscape -remote 'openUrl(%s)' || netscape '%s'"
  "Shell command used to direct an external web browser to load a http: url.
Any `%s' substrings will be replaced by the name of the url.")


;; Functions

;;;###autoload
(defun find-url (url)
  "Find the univeral resource locator URL. This may involve switching to some
kind of editor buffer, or spawning an external process."
  (interactive (list (prompt-for-string "URL:" (and (blockp) (copy-block)))))
  (catch 'foo
    (mapc #'(lambda (cell)
	      (when (string-match (car cell) url nil t)
		(throw 'foo (funcall (cdr cell) url))))
	  find-url-alist)))

(defun find-url-external (url)
  "Spawn an external process (using find-url-external-command as template)
to view URL."
  (let
      ((args (cons url nil)))
    ;; An inifinite list of URLS to pass to format
    (rplacd args args)
    (message "Calling external browser..." t)
    (shell-command (apply 'format nil find-url-external-command args))))

(defun find-url-file (url)
  "Decode URL assuming that it locates a local file, then find this file in
a buffer."
  (when (string-match "^(file:|)/(/localhost|)(.*)$" url nil t)
    (find-file (expand-last-match "\\3"))))

;; Need to implement find-url-ftp and find-url-telnet
