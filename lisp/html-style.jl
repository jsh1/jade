;;;; html-style.jl -- Cooperate with file-subst to build HTML
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

(require 'file-subst)
(require 'maildefs)			;for user-mail-address
(provide 'html-style)

;;; Commentary:
;;;
;;; This file uses the generic substitute-into-a-template features of
;;; file-subst.jl to allow easy creation of multiple HTML files following
;;; a single style.
;;;
;;; The basic idea is to create a .html.in file for each .html file
;;; you wish to build. The .in file has calls to the Lisp html-style
;;; functions, using the @...@ expansion mechanism.
;;;
;;; The currently defined functions include:
;;;
;;;	html-style-header TITLE-STRING
;;;	html-style-footer [ID-STRING]
;;;	html-style-small-break
;;;	html-style-large-break
;;;	html-style-title NEW-HEADING [ANCHOR-NAME]
;;;
;;; Each function is documented in the usual way. An example use of all
;;; this might be something like the following:
;;;
;;;	@(progn
;;;	   (require 'html-style)
;;;	   (html-style-header "The title of this document"))@
;;;
;;;	The body of the first section would follow, using whatever
;;;	HTML code is desired. Note that in the above expansion we
;;;	require html-style to ensure that it's loaded.
;;;
;;;	@(html-style-title "The Next Section")@
;;;
;;;	After that heading we'd have some more text.. Plus whatever
;;;	else is wanted..
;;;
;;;	@(html-style-footer)@
;;;
;;; The html-style-footer call must be the last thing in the document.
;;;
;;; All configuration is done via the html-style-X variables defined
;;; below, and perhaps most importantly the html-style-templates
;;; alist. This defines the actual HTML code that is inserted by
;;; the html-style functions. To change the page layout, this is the
;;; variable that must be changed.
;;;
;;; Obviously, this is all a bit restrictive, but it works very well
;;; at solving the problem it was supposed to address, namely keeping
;;; a coherent style across my web pages.
;;;
;;; TODO:
;;;  * maintain a database of different styles, instead of just the
;;;    one in html-style-templates


;; Configuration

(defvar html-style-default 'plain
  "Default style to use when building html.")

(defvar html-style-current nil
  "Style of the current document.")

(defvar html-style-home-page-url "http://foo.bar.com/~baz/"
  "Default home page URL.")

(defvar html-style-home-page-name "J. Random User"
  "Name to associate with home page URL.")

(defvar html-style-bg-color "#ffffff"
  "Background colour of files created by html-style commands")

(defvar html-style-text-color "#000000"
  "Text colour of files created by html-style commands")

(defvar html-style-link-color "#1f00ff"
  "Link colour for files created by html-style commands")

(defvar html-style-alink-color "#ff0000"
  "Active link colour for files created by html-style commands") 

(defvar html-style-vlink-color "#9900DD"
  "Visited link colour for files created by html-style commands")

;; The standard style is very simple
(defvar html-style-plain-template
  '((header . "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\"
            \"http://www.w3.org/TR/REC-html40/strict.dtd\">
<html>
<head>
<link rev=\"made\" href=\"mailto:@mail-address@\">
<title>@title@</title>
</head>
<body bgcolor=\"@bg-color@\"
      text=\"@text-color@\"
      link=\"@link-color@\"
      alink=\"@alink-color@\"
      vlink=\"@vlink-color@\">\n")
  (title . "<h1 align=center>@title@</h1>")
  (section-start . "")
  (section-end . "<p>")
  (break . "<p>\n")
  (footer . "<hr><address>
<a href=\"@home-page-url@\">@home-page-name@</a><br>
<a href=\"mailto:@mail-address@\">@mail-address@</a><br>
Created: @date@.
</address><hr>@id-string@
</body>
</html>")
  (id-string . "\n<!-- @id@ -->"))
  "Alist of insertion templates for the plain html-style.")

(defvar html-style-alist (list (cons 'plain html-style-plain-template))
  "List of (STYLE . TEMPLATE-ALIST).")


;; Support functions

(defun html-style-quote-ats (string)
  "Return a copy of STRING with all @ characters replaced by @@"
  (let
      ((point 0))
    (while (string-match "@" string point)
      (setq string (concat (substring string 0 (match-start))
			   "@@"
			   (substring string (match-end)))
	    point (1+ (match-end))))
    string))

(defun html-style-init-vars ()
  "Initialise all file-subst variables exported by html-style. Variables
are only set if they don't already have a value."
  (let
      ((vars (list (cons 'mail-address
			 (html-style-quote-ats user-mail-address))
		   (cons 'home-page-url
			 (html-style-quote-ats html-style-home-page-url))
		   (cons 'home-page-name
			 (html-style-quote-ats html-style-home-page-name))
		   (cons 'bg-color html-style-bg-color)
		   (cons 'text-color html-style-text-color)
		   (cons 'link-color html-style-link-color)
		   (cons 'alink-color html-style-alink-color)
		   (cons 'vlink-color html-style-vlink-color))))
    (setq html-style-current html-style-default)
    ;; Merge in file-subst variables that aren't already set
    (mapc #'(lambda (cell)
	      (unless (assq (car cell) file-subst-vars)
		(file-subst-set (car cell) (cdr cell)))) vars)))

(defmacro html-style-get (tag)
  "Return the html template for TAG."
  `(cdr (assq ,tag (cdr (assq html-style-current html-style-alist)))))

(defmacro html-style-insert (tag)
  "Insert the html template for TAG."
  `(insert (html-style-get ,tag)))

;;;###autoload
(defun html-style-add-style (name template)
  (setq html-style-alist (cons (cons name template) html-style-alist)))


;; Functions that can be embedded in .html.in files

(defun html-style-set-style (style)
  "Set the style used for the current document."
  (setq html-style-current style))

(defun html-style-header (title #!optional no-heading)
  "Insert the header for a html file. Includes the first heading of TITLE,
and the start of the first section of text."
  (html-style-init-vars)
  (file-subst-set 'title title)
  (html-style-insert 'header)
  (unless no-heading
    (html-style-insert 'title)
    (html-style-insert 'section-start)))

(defun html-style-footer (#!optional id)
  "Insert the footer for a html file. If ID is defined, it should be a string
identifying the current revision of the html file."
  (if id
      (progn
	(file-subst-set 'id-string (html-style-get 'id-string))
	(file-subst-set 'id id))
    (file-subst-set 'id-string nil))
  (html-style-insert 'section-end)
  (html-style-insert 'footer))

(defun html-style-small-break ()
  "Insert a small break in the body of an html file."
  (html-style-insert 'break))

(defun html-style-large-break ()
  "Insert a large break in the body of an html file."
  (html-style-insert 'section-end)
  (html-style-insert 'section-start))

(defun html-style-title (title #!optional anchor)
  "Insert a break, a heading, and the start of a new section, in the body of
an html file."
  (when (eq anchor t)
    (setq anchor title))
  (file-subst-set 'title title)
  (html-style-insert 'section-end)
  (when anchor
    (insert (format nil "<a name=\"%s\">" anchor)))
  (html-style-insert 'title)
  (when anchor
    (insert "</a>"))
  (html-style-insert 'section-start))
