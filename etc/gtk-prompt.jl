;;;; gtk-prompt.jl -- Rebind some prompts to GTK
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

(require 'prompt)
(require 'gtk)

(defvar gtk-prompt-enable 'mouse)

(defvar gtk-prompt-old-y-or-n-p y-or-n-p)
(defvar gtk-prompt-old-yes-or-no-p yes-or-no-p)
(defvar gtk-prompt-old-map-y-or-n-p map-y-or-n-p)
(defvar gtk-prompt-old-prompt-for-string prompt-for-string)
(defvar gtk-prompt-old-prompt-from-list prompt-from-list)
(defvar gtk-prompt-old-prompt-for-file prompt-for-file)
(defvar gtk-prompt-old-prompt-for-directory prompt-for-directory)


;; General purpose GTK dialogs

;; Each BUTTON is (TEXT . RETURNED-VALUE)
(defun gtk-prompt-dialog (msg #!rest buttons)
  (let
      ((window (gtk-window-new 'toplevel))
       (vbox (gtk-vbox-new nil 0))
       (label (gtk-label-new msg))
       (bbox (gtk-hbutton-box-new)))
    (catch 'exit
      (unwind-protect
	  (progn
	    (gtk-container-border-width window 6)
	    (gtk-signal-connect window "delete_event"
				#'(lambda ()
				   (throw 'exit nil)))
	    (gtk-container-add window vbox)
	    (gtk-box-pack-start vbox label)
	    (gtk-box-pack-end vbox bbox)
	    (mapc #'(lambda (cell)
		      (let
			  ((button (gtk-button-new-with-label (car cell))))
			(GTK-WIDGET-SET-FLAGS button '(can-default))
			(gtk-box-pack-start bbox button nil nil)
			(gtk-signal-connect button "clicked"
					    #'(lambda ()
						(throw 'exit (cdr cell))))))
		  buttons)
	    (gtk-widget-show-all window)
	    (recursive-edit))
	(gtk-widget-destroy window)))))

(defun gtk-prompt-for-string (#!optional title start)
  (let
      ((window (gtk-window-new 'toplevel))
       (vbox (gtk-vbox-new nil 0))
       (hbox (gtk-hbox-new nil 0))
       (label (gtk-label-new (or title "Enter string:")))
       (entry (gtk-entry-new))
       (bbox (gtk-hbutton-box-new))
       button)
    (catch 'exit
      (unwind-protect
	  (progn
	    (gtk-container-border-width window 6)
	    (gtk-signal-connect window "delete_event" #'(lambda ()
							  (throw 'exit nil)))
	    (gtk-container-add window vbox)
	    (gtk-box-pack-start vbox hbox)
	    (gtk-box-set-spacing hbox 5)
	    (gtk-box-pack-start hbox label)
	    (when start
	      (gtk-entry-set-text entry start))
	    (gtk-box-pack-end hbox entry)
	    (gtk-box-pack-end vbox bbox)
	    (setq button (gtk-button-new-with-label "Ok"))
	    (GTK-WIDGET-SET-FLAGS button '(can-default))
	    (gtk-box-pack-start bbox button)
	    (let
		((fun #'(lambda ()
			  (throw 'exit (gtk-entry-get-text entry)))))
	      (gtk-signal-connect button "clicked" fun)
	      (gtk-signal-connect entry "activate" fun))
	    (setq button (gtk-button-new-with-label "Cancel"))
	    (GTK-WIDGET-SET-FLAGS button '(can-default))
	    (gtk-box-pack-start bbox button)
	    (gtk-signal-connect button "clicked" #'(lambda ()
						     (throw 'exit nil)))
	    (gtk-widget-show-all window)
	    (gtk-widget-grab-focus entry)
	    (recursive-edit))
	(gtk-widget-destroy window)))))

(defun gtk-prompt-from-list-activate (combo dont-validate)
  (let
      ((text (gtk-entry-get-text (gtk-combo-entry combo))))
    (if (or dont-validate
	    (prompt-validate-from-list text))
	(throw 'exit text)
      (beep))))

(defun gtk-prompt-from-list (prompt-list title #!optional start dont-validate)
  (let
      ((window (gtk-window-new 'toplevel))
       (vbox (gtk-vbox-new nil 0))
       (hbox (gtk-hbox-new nil 0))
       (label (gtk-label-new (or title "Enter string:")))
       (combo (gtk-combo-new))
       (bbox (gtk-hbutton-box-new))
       button)
    (catch 'exit
      (unwind-protect
	  (progn
	    (gtk-container-border-width window 6)
	    (gtk-signal-connect window "delete_event" #'(lambda ()
							  (throw 'exit nil)))
	    (gtk-container-add window vbox)
	    (gtk-box-pack-start vbox hbox)
	    (gtk-box-set-spacing hbox 5)
	    (gtk-box-pack-start hbox label)
	    (when start
	      (gtk-combo-set-item-string combo start))
	    (gtk-combo-set-popdown-strings combo prompt-list)
	    (gtk-box-pack-end hbox combo)
	    (gtk-box-pack-end vbox bbox)
	    (setq button (gtk-button-new-with-label "Ok"))
	    (GTK-WIDGET-SET-FLAGS button '(can-default))
	    (gtk-box-pack-start bbox button)
	    (let
		((fun #'(lambda ()
			  (gtk-prompt-from-list-activate
			   combo dont-validate))))
	      (gtk-signal-connect button "clicked" fun)
	      (gtk-signal-connect (gtk-combo-entry combo) "activate" fun))
	    (setq button (gtk-button-new-with-label "Cancel"))
	    (GTK-WIDGET-SET-FLAGS button '(can-default))
	    (gtk-box-pack-start bbox button)
	    (gtk-signal-connect button "clicked" #'(lambda ()
						     (throw 'exit nil)))
	    (gtk-widget-show-all window)
	    (recursive-edit))
	(gtk-widget-destroy window)))))

(defun gtk-prompt-for-file (#!optional title initial default predicate)
  (let
      ((sel (catch 'exit
	      (let
		  ((fs (gtk-file-selection-new (or title "Select file")))
		   (file nil))
		(unwind-protect
		    (progn
		      (if initial
			  (gtk-file-selection-set-filename fs initial)
			(gtk-file-selection-set-filename fs *default-directory*))
		      (gtk-signal-connect (gtk-file-selection-cancel-button fs)
					  "clicked"
					  #'(lambda () (throw 'exit nil)))
		      (gtk-signal-connect fs "delete_event"
					  #'(lambda () (throw 'exit nil)))
		      (gtk-signal-connect
		       (gtk-file-selection-ok-button fs)
		       "clicked"
		       #'(lambda ()
			   (throw 'got (gtk-file-selection-get-filename fs))))
		      (gtk-widget-show fs)
		      (while t
			(setq file (catch 'got (recursive-edit)))
			(if (or (null? predicate)
				(funcall predicate file))
			    (throw 'exit file)
			  (message
			   (format nil "Filename must satisfy %s" predicate) t)
			  (beep))))
		  (gtk-widget-destroy fs))))))
    (when (and sel (string=? sel ""))
      (setq sel default))
    sel))


;; Overriding the standard Jade prompt functions

(defun gtk-prompt-with-gtk-p ()
  (or (eq? gtk-prompt-enable t)
      (and (eq? gtk-prompt-enable 'mouse) current-command-from-mouse)
      (and (pair? gtk-prompt-enable) (memq this-command gtk-prompt-enable))))

(defun yes-or-no-p (question #!rest args)
  (if (gtk-prompt-with-gtk-p)
      (gtk-prompt-dialog question '("Yes" . t) '("No" . nil))
    (apply gtk-prompt-old-yes-or-no-p question args)))

(defun y-or-n-p (question #!rest args)
  (if (gtk-prompt-with-gtk-p)
      (gtk-prompt-dialog question '("Yes" . t) '("No" . nil))
    (apply gtk-prompt-old-y-or-n-p question args)))

(defun map-y-or-n-p (question inputs callback #!rest args)
  (if (gtk-prompt-with-gtk-p)
      (let
	  ((all-t t))
	(when (eq? 'all-t (catch 'map
			   (while inputs
			     (let*
				 ((q (if (string? question)
					 (format nil question (car inputs))
				       (funcall question (car inputs))))
				  (a (gtk-prompt-dialog
				      q '("Yes" . t) '("No" . nil)
				      '("Yes to all" . all-t)
				      '("Quit" . quit))))
			       (cond ((or (eq? a 'all-t) (eq? a 'quit))
				      (throw 'map a))
				     (a
				      (funcall callback (car inputs)))
				     (t
				      (setq all-t nil)))
			       (setq inputs (cdr inputs))))))
	  ;; User answered with "!", so loop over all remaining inputs
	  (while inputs
	    (funcall callback (car inputs))
	    (setq inputs (cdr inputs))))
	all-t)
    (apply gtk-prompt-old-map-y-or-n-p question inputs callback args)))

(defun prompt-for-string (#!rest args)
  (apply (if (gtk-prompt-with-gtk-p)
	     gtk-prompt-for-string
	   gtk-prompt-old-prompt-for-string) args))

(defun prompt-from-list (#!rest args)
  (apply (if (gtk-prompt-with-gtk-p)
	     gtk-prompt-from-list
	   gtk-prompt-old-prompt-from-list) args))

(defun prompt-for-file (#!optional title existing start default #!rest args)
  (if (gtk-prompt-with-gtk-p)
      (gtk-prompt-for-file title start default
			   (if existing
			       #'(lambda (f)
				   (and (file-exists? f)
					(not (file-directory? f))))
			     #'(lambda (f)
				 (not (file-directory? f)))))
    (apply gtk-prompt-old-prompt-for-file title existing start default args)))

(defun prompt-for-directory (#!optional title existing start default #!rest args)
  (if (gtk-prompt-with-gtk-p)
      (gtk-prompt-for-file title start default
			   (if existing
			       'file-directory?
			     #'(lambda (f)
				 (or (not (file-exists? f))
				     (file-directory? f)))))
    (apply gtk-prompt-old-prompt-for-directory
	   title existing start default args)))
