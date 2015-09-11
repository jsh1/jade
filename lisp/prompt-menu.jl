;;;; prompt-menu.jl -- Mimic popup-menus using normal prompts
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

(require 'popup-menus)
(provide 'prompt-menu)

(defvar prompt-menu-tree nil)

(defun prompt-menu-create-menu (spec)
  (let
      (menu)
    (mapc (lambda (cell)
	    (let
		(label item)
	      (when (and cell (symbol? (car cell)))
		(setq cell (symbol-value (car cell))))
	      (unless (null? cell)
		(setq label (car cell))
		(if (function? (cdr cell))
		    (setq cell ((cdr cell)))
		  (setq cell (cdr cell)))
		(cond
		 ((function? (car cell))
		  (setq item (list* 'command label (car cell))))
		 ((pair? (car cell))
		  (setq item (list* 'sub label
				    (prompt-menu-create-menu cell))))
		 ((eq? (car cell) t)
		  (setq item (list* 'function label (nth 1 cell))))))
	      (when item
		(setq menu (cons item menu)))))
	  spec)
    (nreverse menu)))

(defun prompt-menu-find (tree item)
  (catch 'return
    (mapc (lambda (x)
	    (when (string-match
		   (concat #\^ (quote-regexp (nth 1 x)) #\$) item nil t)
	      (throw 'return x))) tree)
    nil))

(defun prompt-menu-prompt (tree title)
  (let*
      ((lst (mapcar (lambda (x) (car (cdr x))) tree))
       (choice (prompt-from-list lst (concat title #\:))))
    (while choice
      (setq choice (or (prompt-menu-find tree choice)
		       (error "Can't find menu")))
      (when (eq? (car choice) 'sub)
	(setq choice (prompt-menu-prompt (nthcdr 2 choice)
					 (concat title #\/ (nth 1 choice)))))
      (when choice
	(throw 'prompt-menu-top choice))
      (setq choice (prompt-from-list lst (concat title #\:))))))

(defun popup-menu-from-spec (spec)
  (let*
      ((prompt-menu-tree (prompt-menu-create-menu spec))
       (prompt-list-fold-case t)
       (item (catch 'prompt-menu-top
	       (prompt-menu-prompt prompt-menu-tree "Menu"))))
    (when item
      (cond ((eq? (car item) 'command)
	     (popup-menu-dispatch-command (nthcdr 2 item)))
	    ((eq? (car item) 'function)
	     ((nthcdr 2 item)))))))
