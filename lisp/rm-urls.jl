;;;; rm-urls.jl -- Highlight URL-like text in mail messages
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

(require 'read-mail)
(provide 'rm-urls)

;; Commentary:
;;
;; This is a hack, it assumes knowledge of the html-display module that
;; it has no right to. Also, the structure of the link extent..

(defvar rm-urls-regexp "(http|ftp)://[^ \t\n\f\r]+")

(defvar rm-urls-max-lines 256)

(defvar rm-urls-mouse-map
  (bind-keys (make-sparse-keymap)
    "button2-click1" 'goto-mouse
    "button2-move" 'goto-mouse
    "button2-off" 'html-display-mouse-select))

;; Called from the rm-display-message-hook
(defun rm-highlight-urls (message folder)
  (let
      ((point (start-of-buffer)))
    (save-restriction
      (restrict-buffer (start-of-buffer)
		       (min (end-of-buffer)
			    (forward-line rm-urls-max-lines
					  (start-of-buffer))))
      (while (re-search-forward rm-urls-regexp point nil t)
	;; Require these here so that they're only loaded on demand
	(require 'html-display)
	(require 'html-decode)
	(setq point (match-end))
	(let
	    ((extent (make-extent (match-start) point
				  (list 'face (html-decode-elt-face 'link)
					'mouse-face active-face
					'mouse-keymap rm-urls-mouse-map
					'html-anchor-params
					(list (cons
					       'href (copy-area
						      (match-start)
						      point)))))))
	  (extent-put extent 'popup-menus html-display-link-menus))))))

;; Add the function at the end of the hook, to guarantee it's after
;; the mime-decoder
(add-hook 'rm-display-message-hook 'rm-highlight-urls t)
