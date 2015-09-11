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

;; pacify the compiler
(eval-when-compile (require 'html-display))

;; Configuration

(defvar find-url-default 'find-url-http
  "The function to pass any urls to that aren't matched by find-url-alist.")

(defvar find-url-external-command
  "( netscape -remote 'openUrl(%s)' || netscape '%s' ) &"
  "Shell command used to direct an external web browser to load a http: url.
Any `%s' substrings will be replaced by the name of the url.")

(defvar wget-program "wget"
  "Name of program used to invoke Wget.")

(defvar find-url-asynchronously t
  "When non-nil URL's are loaded asynchronously where possible.")

(defvar find-url-processes nil
  "A list of (URL . PROCESS) objects for all asynchronous wget commands.")

;; Functions

;;;###autoload
(defun find-url (url #!optional call-external)
  "Find the univeral resource locator URL. This may involve switching to some
kind of editor buffer, or spawning an external process. If CALL-EXTERNAL is
non-nil, then the external viewer is invoked."
  (interactive (let
		   ((arg current-prefix-arg))
		 (list (prompt-for-string
			"URL:" (and (blockp) (copy-block))) arg)))
  (if call-external
    (find-url-external url)
    (catch 'foo
      (mapc (lambda (cell)
	      (when (string-match (car cell) url nil t)
		(throw 'foo ((cdr cell) url))))
	    find-url-alist)
      ;; Call default function
      (find-url-default url))))

;;;###autoload
(defun find-file-as-url (filename)
  "Display the file FILENAME as if it was loaded through the web browser."
  (interactive "fFile to display as if a URL:")
  (and (find-file filename)
       (find-url-magic-buffer
	(concat "file:/" (canonical-file-name filename)))))

(defun find-url-external (url)
  "Spawn an external process (using find-url-external-command as template)
to view URL."
  (interactive (list (prompt-for-string "URL:" (and (blockp) (copy-block)))))
  (let
      ((args (cons url nil)))
    ;; An inifinite list of URLS to pass to format
    (rplacd args args)
    (message "Calling external browser..." t)
    (system (apply format nil find-url-external-command args))))

(defun find-url-magic-buffer (url)
  (when (or (string-match "\\.html?$" (buffer-file-name))
	    (search-forward "<html>" nil nil t))
    ;; Assume buffer contains HTML, decode it and delete
    ;; the original buffer
    (let
	((buffer (current-buffer))
	 anchor)
      (when (string-match "^(.*)#(.*)$" url)
	(setq url (expand-last-match "\\1"))
	(setq anchor (expand-last-match "\\2")))
      (html-display buffer url nil)
      (kill-buffer buffer)
      (and anchor (html-display-goto-anchor anchor)))))

(defun find-url-file (url)
  "Decode URL assuming that it locates a local file, then find this file in
a buffer."
  (when (string-match "^file:([^#]+)" url nil t)
    (find-file (expand-last-match "\\1"))
    (find-url-magic-buffer url)))

(defun find-url-telnet (url)
  "Decode a telnet URL, and invoke the telnet package."
  (when (string-match "^(telnet://)?([^:]+)(:[0-9]+|)$" url nil t)
    (let
	((host (expand-last-match "\\2"))
	 (port (if (= (match-start 3) (match-end 3))
		   nil
		 (string->number
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
  (when (string-match "^ftp://(([^:@]+)(:([^@]+))?@)?([^/]+)/?([^#]*)" url)
    (let
	((user (expand-last-match "\\2"))
	 (passwd (expand-last-match "\\4"))
	 (host (expand-last-match "\\5"))
	 (file (expand-last-match "\\6")))
      (when (and (not (string=? user ""))
		 (not (string=? passwd "")))
	(require 'rep.io.file-handlers.remote.ftp)
	(remote-ftp-add-passwd user host passwd))
      ;; XXX What if the method of retrieving files from HOST isn't FTP?
      (find-file (concat #\/ (if (string=? user "") "anonymous" user)
			 #\@ host #\: file))
      (find-url-magic-buffer url))))

(defun find-url-http-loaded (process url anchor view output errors)
  (require 'mail-headers)
  (unless (process-in-use? process)
    (setq find-url-processes (delete (cons url process) find-url-processes))
    (if (zero? (process-exit-value process))
	;; Success
	(let
	    (content-type)
	  (message (format nil "wget %s...done" url))
	  ;; Look in the error output to see if wget printed a `Location:'
	  ;; message
	  (and (re-search-forward "^Location:[ \t]*([^ \t]+)"
				  (start-of-buffer errors) errors t)
	       (setq url (expand-last-match "\\1")))
	  (with-buffer output
	    (setq content-type (mail-get-header "content-type"))
	    (and (re-search-forward "^[ \t]*\n" (start-of-buffer))
		 (restrict-buffer (match-end) (end-of-buffer))))
	  (cond
	   ((string-match "html" content-type nil t)
	    (message "Parsing HTML..." t)
	    (html-display output url view)
	    (when anchor
	      (html-display-goto-anchor anchor))
	    (message "Parsing HTML...done" t))
	   (t
	    ;; Not HTML. Just rename the buffer to what we think
	    ;; the filename is then display it
	    (if (string-match "/([^/]+)$" url)
		(set-buffer-name output (make-buffer-name
					 (expand-last-match "\\1")))
	      (set-buffer-name output (make-buffer-name url)))
	    (set-buffer-modified output nil)
	    (set-buffer-read-only output t)
	    (with-view view
	      (goto-buffer output)))))
      ;; Failure
      (with-view view
	(goto-buffer errors))
      (error "Wget returned non-zero!"))))
	       
(defun find-url-http (url)
  (let
      (load-url anchor buffer)
    (if (string-match "^(.*)#(.*)$" url)
	(progn
	  (setq load-url (expand-last-match "\\1"))
	  (setq anchor (expand-last-match "\\2")))
      (setq load-url url))
    (when (bound? 'html-display-find-url)
      (setq buffer (html-display-find-url url)))
    (if buffer
	(progn
	  (goto-buffer buffer)
	  (when anchor
	    (html-display-goto-anchor anchor)))
      (setq buffer (make-buffer "*wget-output*"))
      (let
	  ((errors (make-buffer "*wget-errors*"))
	   (process (make-process (cons buffer t)))
	   args)
	(setq args (list wget-program "-s" "-O" "-" load-url))
	(set-process-error-stream process (cons errors t))
	(clear-buffer buffer)
	(clear-buffer errors)
	(message (format nil "wget %s..." url) t)
	(if find-url-asynchronously
	    (progn
	      (set-process-function
	       process
	       (let ((view (current-view)))
		 (lambda (p)
		   (find-url-http-loaded p load-url anchor
					 view buffer errors))))
	      (or (apply start-process process args)
		  (error "Can't start wget"))
	      (setq find-url-processes (cons (cons url process)
					     find-url-processes)))
	  (apply call-process process nil args)
	  (find-url-http-loaded process url anchor
				(current-view) buffer errors))))))

(defun find-url-abort (url #!optional kill)
  "Terminate the asynchronous connection loading URL."
  (interactive (let
		   ((arg current-prefix-arg))
		 (list (prompt-from-list
			(mapcar car find-url-processes) "Abort URL:") arg)))
  (let
      ((cell (assoc url find-url-processes)))
    (if cell
	(progn
	  (set-process-function
	   (cdr cell)
	   (lambda (p)
	     (declare (unused p))
	     (setq find-url-processes (delq cell find-url-processes))
	     (message "[wget exited]")))
	  ((if kill kill-process interrupt-process) (cdr cell)))
      (message "[No wget for that URL]"))))


;; init

(defvar find-url-alist (list (cons "^file:" find-url-file)
			     (cons "^http:" find-url-http)
			     (cons "^ftp:" find-url-ftp)
			     (cons "^telnet:" find-url-telnet)
			     (cons "^mailto:" find-url-mailto))
  "Alist of (REGEXP . FUNC) matching URLs to the Lisp functions used to
display them.")
