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

(provide 'keymap)

;;;###autoload
(defun print-keymap (&optional keymap-list buffer)
  "Prints a description of the installed keymaps in the current buffer."
  (unless keymap-list
    (setq keymap-list keymap-path))
  (unless buffer
    (setq buffer (current-buffer)))
  (insert "\nKey/Event")
  (indent-to 24)
  (insert "Binding\n---------")
  (indent-to 24)
  (insert "-------\n\n")
  (let
      (done-list)			; keymaps already printed
    (while keymap-list
      (let
	  ((keymap (car keymap-list))
	   km-prefix-string)
	(setq keymap-list (cdr keymap-list))
	(when (and (not (keymapp keymap)) (consp keymap))
	  (setq km-prefix-string (cdr keymap)
		keymap (car keymap)))
	(unless (memq keymap done-list)
	  (setq done-list (cons keymap done-list))
	  (when (symbolp keymap)
	    (format (current-buffer) " -- %s:\n" keymap)
	    (setq keymap (with-buffer buffer (symbol-value keymap))))
	  (when (keymapp keymap)
	    (cond
	     ((vectorp keymap)
	      (let
		  ((i (length keymap)))
		(while (>= i 0)
		  (km-print-list (aref keymap i))
		  (setq i (1- i)))))
	     (t
	      (km-print-list (cdr keymap)))))
	  (insert "\n"))))))

;; Print one keymap. This accesses the free variables `keymap-list' and
;; `km-prefix-string' -- both in describe-keymap.
(defun km-print-list (keymap)
  (let
      (key cmd event-str)
    (while keymap
      (setq key (car keymap)
	    cmd (aref key 2)
	    event-str (event-name (cons (aref key 0) (aref key 1))))
      (when (and (eq (car cmd) 'setq) (eq (nth 1 cmd) 'next-keymap-path))
	;; Link to another keymap; add it to the list of keymaps to
	;; examine later.
	(let*
	    ((new-str (concat km-prefix-string (if km-prefix-string ?\ )
			      event-str))
	     (new-list (mapcar #'(lambda (km)
				   (cons km new-str))
			       (eval (nth 2 cmd)))))
	  (setq keymap-list (append keymap-list new-list))))
      (insert (concat km-prefix-string (if km-prefix-string ?\ )  event-str))
      (indent-to 24)
      (prin1 cmd (current-buffer))
      (insert "\n")
      (setq keymap (cdr keymap)))))


;; Get one event

(defun km-read-event-fun ()
  (throw 'read-event (current-event)))

;;;###autoload
(defun read-event (&optional title)
  "Read the next event and return a cons cell containing the two integers that
define that event."
  (let
      ((buffer (current-buffer))
       (old-kp keymap-path)
       (old-nkp next-keymap-path))
    (setq keymap-path nil
	  next-keymap-path nil
	  status-line-cursor t)
    (add-hook 'unbound-key-hook 'km-read-event-fun)
    (unwind-protect
	(catch 'read-event
	  (message (or title "Type a key:"))
	  (recursive-edit))
      (with-buffer buffer
	(remove-hook 'unbound-key-hook 'km-read-event-fun)
	(setq keymap-path old-kp
	      next-keymap-path old-nkp
	      status-line-cursor nil)))))

;;;###autoload
(defun describe-key ()
  "Read an event sequence from the user then display details of the command it
would invoke."
  (interactive)
  (let
      (names event command done)
    (while (not done)
      (setq event (read-event (concat "Enter a key sequence: " names))
	    names (concat names (if names ?\ )
			  (event-name event)))
      (if (setq command (lookup-event-binding event t))
	  (if (and (eq (car command) 'setq)
		   (eq (nth 1 command) 'next-keymap-path))
	      ;; A link to another keymap
	      (call-command command)
	    ;; End of the chain
	    (require 'help)
	    (help-setup)
	    (format (current-buffer) "\n%s -> %S\n" names command)
	    (when (functionp command)
	      (format (current-buffer)
		      "\n%s\n"
		      (or (documentation command) "")))
	    (goto-buffer-start)
	    (setq done t))
	(message (concat names " is unbound. "))
	(setq done t
	      next-keymap-path nil)))))

