;;;; keymap.jl -- extra functions for handling keymaps
;;;  Copyright (C) 1994 John Harper <john@dcs.warwick.ac.uk>
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

(require 'help)
(provide 'keymap)


;; Map a function over all key bindings in a keymap

(defvar map-keymap-recursively t
  "When nil, the map-keymap function will only map over the first level of
keymaps, i.e. all prefix keys are ignored.")

(defvar km-prefix-string nil)
(defvar km-keymap-list nil)

(defun active-keymaps (buffer)
  (with-buffer buffer
    (let
	((maps nil)
	 (e (get-extent))
	 (minor minor-mode-keymap-alist)
	 tem)
      (while e
	(setq tem (extent-get e 'keymap))
	(when tem
	  (setq maps (cons tem maps)))
	(setq e (extent-parent e)))
      (while minor
	(when (symbol-value (car (car minor)))
	  (setq maps (cons (cdr (car minor)) maps)))
	(setq minor (cdr minor)))
      (when local-keymap
	(setq maps (cons local-keymap maps)))
      (when global-keymap
	(setq maps (cons global-keymap maps)))
      (mapcar #'(lambda (km)
		  (if (symbolp km)
		      ;; dereference the symbol in the correct buffer
		      (symbol-value km)
		    km))
	      (nreverse maps)))))

;;;###autoload
(defun map-keymap (function &optional keymap buffer)
  "Map FUNCTION over all key bindings under the keymap or list of keymaps
KEYMAP (by default, the active keymaps of BUFFER). If BUFFER is defined it
gives the buffer to dereference all variables in.

FUNCTION is called as (FUNCTION KEY PREFIX), where KEY is a cons cell
(COMMAND . EVENT), and PREFIX a string describing the prefix keys of this
binding, or nil if there was no prefix."
  (unless buffer
    (setq buffer (current-buffer)))
  (when (keymapp keymap)
    (setq keymap (list keymap)))
  (let
      ((km-keymap-list (or keymap (active-keymaps buffer)))
       (done-list nil))
    (while km-keymap-list
      (let
	  ((keymap (car km-keymap-list))
	   km-prefix-string)
	(setq km-keymap-list (cdr km-keymap-list))
	(when (and (not (keymapp keymap)) (consp keymap))
	  (setq km-prefix-string (cdr keymap)
		keymap (car keymap)))
	(unless (memq keymap done-list)
	  (setq done-list (cons keymap done-list))
	  (when (symbolp keymap)
	    (setq keymap (with-buffer buffer (symbol-value keymap))))
	  (when (keymapp keymap)
	    (if (vectorp keymap)
		(let
		    ((i (1- (length keymap))))
		  (while (>= i 0)
		    (km-map-keylist (aref keymap i) function buffer)
		    (setq i (1- i))))
	      (km-map-keylist (cdr keymap) function buffer))))))))

;; Map over a single list of keybindings
(defun km-map-keylist (keylist function buffer)
  (mapc #'(lambda (k)
	    (cond
	     ((eq k 'keymap))		;An inherited sparse keymap
	     ((or (and (symbolp (car k))
		       (eq (symbol-value (car k) t) 'keymap))
		  (eq (car (car k)) 'next-keymap-path))
	      ;; A prefix key
	      (when map-keymap-recursively
		(let
		    ((this-list (if (symbolp (car k))
				    (list (with-buffer buffer
					    (symbol-value (car k) t)))
				  (with-buffer buffer (eval (nth 1 (car k))))))
		     (event-str (event-name (cdr k))))
		  (when (listp this-list)
		    ;; Another keymap-list, add it to the list of those waiting
		    (let*
			((new-str (concat km-prefix-string
					  (if km-prefix-string ?\ )
					  event-str))
			 (new-list (mapcar #'(lambda (km)
					       (cons km new-str)) this-list)))
		      (setq km-keymap-list (append km-keymap-list
						   new-list)))))))
	      (t
	       ;; A normal binding
	       (funcall function k km-prefix-string))))
	keylist))


;; Substitute one command for another in a keymap

;;;###autoload
(defun substitute-key-definition (olddef newdef &optional keymap)
  "Substitute all occurrences of the command OLDDEF for the command NEWDEF
in the keybindings under the keymap or list of keymaps KEYMAP. When KEYMAP
is nil, the currently active keymaps used, i.e. all key bindings currently
in effect."
  (interactive "COld command:\nCReplacement command:")
  (map-keymap #'(lambda (k pfx)
		  (when (eq (car k) olddef)
		    (rplaca k newdef))) keymap))


;; Adding bindings to a feature that may not yet be loaded

;;;###autoload
(defmacro lazy-bind-keys (feature keymap &rest bindings)
  "Install the list of BINDINGS in KEYMAP, assuming that KEYMAP is available
once FEATURE has been provided. If FEATURE has not yet been loaded, arrange
for the bindings to be installed if and when it is."
  `(if (featurep ',feature)
       (bind-keys ,keymap ,@bindings)
     (eval-after-load ,(symbol-name feature) '(bind-keys ,keymap ,@bindings))))


;; Printing keymaps

;;;###autoload
(defun print-keymap (&optional keymap buffer)
  "Prints a description of the installed keymaps in the current buffer."
  (insert "\nKey/Event")
  (indent-to 24)
  (insert "Binding\n---------")
  (indent-to 24)
  (insert "-------\n\n")
  (map-keymap #'(lambda (k prefix)
		  (format (current-buffer) "%s%s%s "
			  (or prefix "")
			  (if prefix " " "")
			  (event-name (cdr k)))
		  (indent-to 24)
		  (format (current-buffer) "%S\n" (car k)))
	      keymap buffer))


;; Search for a named command in the current keymap configuration

(defvar km-where-is-results nil)

;;;###autoload
(defun where-is (command &optional keymap buffer output)
  (interactive "CWhere is command:\n\n\nt")
  (let
      ((km-where-is-results nil))
    (map-keymap #'(lambda (k pfx)
		    (when (eq (car k) command)
		      (setq km-where-is-results
			    (cons (concat pfx (and pfx " ")
					  (event-name (cdr k)))
				  km-where-is-results))))
		keymap buffer)
    (when output
      (message (format nil "`%s' is on %s" command
		       (or km-where-is-results "no keys"))))
    km-where-is-results))
      

;; Get one event

(defvar read-event-cooked nil)

(defun read-event-callback ()
  (throw 'read-event (if read-event-cooked
			 (current-event-string)
		       (current-event))))

;;;###autoload
(defun read-event (&optional title cooked)
  "Read the next event and return a cons cell containing the two integers that
define that event. If COOKED is non-nil, return the _string_ that the event
is bound to be the operating system, not the event itself."
  (let
      ((temp-buffer (make-buffer "*read-event*"))
       (read-event-cooked cooked))
    (with-view (minibuffer-view)
      (with-buffer temp-buffer
	(add-hook 'unbound-key-hook read-event-callback)
	(next-keymap-path nil)
	(insert (or title "Enter key: "))
	(catch 'read-event
	  (recursive-edit))))))

;;;###autoload
(defun next-event (&optional cooked)
  "Wait for the next input event, then return it. If COOKED is non-nil, return
the string that the operating system would normally insert for that event."
  (let
      ((old-ukh unbound-key-hook)
       (read-event-cooked cooked))
    (add-hook 'unbound-key-hook read-event-callback)
    (next-keymap-path nil)
    (unwind-protect
	(catch 'read-event
	  (recursive-edit))
      (setq unbound-key-hook old-ukh))))

;;;###autoload
(defun describe-key ()
  "Read an event sequence from the user then display details of the command it
would invoke."
  (interactive)
  (let
      ((path 'global-keymap)
       names event command done)
    (while (not done)
      (setq event (read-event (concat "Enter a key sequence: " names))
	    names (concat names (if names ?\ ) (event-name event)))
      (next-keymap-path path)
      (setq command (lookup-event-binding event))
      (if command
	  (cond ((and (symbolp command)
		      (eq (symbol-value command t) 'keymap))
		 ;; a prefix key
		 (setq path (list command)))
		((eq (car command) 'next-keymap-path)
		 ;; A link to another keymap
		 (setq path (eval (nth 1 command))))
		(t
		 ;; End of the chain
		 (help-wrapper
		  (format (current-buffer) "\n%s -> %S\n" names command)
		  (when (functionp command)
		    (format (current-buffer)
			    "\n%s\n"
			    (or (documentation command) ""))))
		 (setq done t)))
	(message (concat names " is unbound. "))
	(setq done t)))))
