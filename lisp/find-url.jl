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

;; TODO:
;; + Also need "news:", "rlogin:", "wais:", "gopher:"...
;;   see: http://www.w3.org/Addressing/URL/url-spec.txt
;; + Doesn't handle %XX character encoding

(provide 'find-url)

;; Configuration

(defvar find-url-alist '(("^file:" . find-url-file)
			 ("^http:" . find-url-http)
			 ("^ftp:" . find-url-ftp)
			 ("^telnet:" . find-url-telnet)
			 ("^mailto:" . find-url-mailto))
  "Alist of (REGEXP . FUNC) matching URLs to the Lisp functions used to
display them.")

(defvar find-url-default 'find-url-external
  "The function to pass any urls to that aren't matched by find-url-alist.")

(defvar find-url-external-command
  "(netscape -remote 'openUrl(%s)' || netscape '%s') >/dev/null 2>&1 </dev/null &"
  "Shell command used to direct an external web browser to load a http: url.
Any `%s' substrings will be replaced by the name of the url.")

(defvar wget-program "wget"
  "Name of program used to invoke Wget.")

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
	  find-url-alist)
    ;; Call default function
    (funcall find-url-default url)))

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
  (when (string-match "^(file:/)?(/localhost|)(.*)$" url nil t)
    (find-file (expand-last-match "\\3"))))

(defun find-url-telnet (url)
  "Decode a telnet URL, and invoke the telnet package."
  (when (string-match "^(telnet://)?([^:]+)(:[0-9]+|)$" url nil t)
    (let
	((host (expand-last-match "\\2"))
	 (port (if (= (match-start 3) (match-end 3))
		   nil
		 (read-from-string
		  (substring url (1+ (match-start 3)) (match-end 3))))))
      (telnet host port))))

(defun find-url-mailto (url)
  "Decode a mailto url, and setup a mail message."
  (when (string-match "^(mailto:)?(.*)$" url nil t)
    (let
	((addr (expand-last-match "\\2")))
      (mail-setup addr))))

(defun find-url-ftp (url)
  ;; XXX Doesn't handle ";type=<typecode>" appendage
  (when (string-match "^ftp://(([^:@]+)(:([^@]+))?@)?([^/]+)/?" url)
    (let
	((user (expand-last-match "\\2"))
	 (passwd (expand-last-match "\\4"))
	 (host (expand-last-match "\\5"))
	 (file (substring url (match-end))))
      (when (and (not (string= user ""))
		 (not (string= passwd "")))
	(remote-ftp-add-passwd user host passwd))
      ;; XXX What if the method of retrieving files from HOST isn't FTP?
      (find-file (concat ?/ (if (string= user "") "anonymous" user)
			 ?@ host ?: file)))))

(defun find-url-http (url)
  (require 'mail-headers)
  (let*
      ((buffer (open-buffer "*wget-output*"))
       (errors (or (get-buffer "*wget-errors*")
		   (make-buffer "*wget-errors*")))
       (process (make-process buffer)))
    (set-process-error-stream process errors)
    (clear-buffer buffer)
    (clear-buffer errors)
    (message (format nil "wget %s..." url) t)
    (if (zerop (call-process process nil wget-program "-s" "-O" "-" url))
	;; Success
	(let
	    (content-type)
	  (message (format nil "wget %s...done" url))
	  ;; Look in the error output to see if wget printed a `Location:'
	  ;; message
	  (and (re-search-forward "^Location:[ \t]*([^ \t]+)"
				  (start-of-buffer errors) errors t)
	       (setq url (expand-last-match "\\1")))
	  (with-buffer buffer
	    (setq content-type (mail-get-header "content-type"))
	    (and (re-search-forward "^[ \t]*\n" (start-of-buffer))
		 (restrict-buffer (match-end) (end-of-buffer))))
	  (cond
	   ((string-match "html" content-type nil t)
	    (message "Parsing HTML..." t)
	    (html-display buffer url)
	    (message "Parsing HTML...done" t))
	   (t
	    ;; XXX needs completing
	    (goto-buffer buffer))))
      ;; Failure
      (with-view (other-view)
	(goto-buffer errors))
      (error "Wget returned non-zero!"))))
