;;;; rm-summary.jl -- Summary interface for mail reader
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

(require 'summary)
(require 'read-mail)
(provide 'rm-summary)

(defvar rm-summary-format "%a  %D %m  %-^16F  %l"
  "A string defining the format of lines in mail summary buffers. It is
copied verbatim except for formatting directives introduced by percent
signs (%). Each directive consists of a percent character, an optional
numeric argument, and a single character specifying what should be
inserted in place of the format directive. These characters include:

	a	A 3-character attribute string, showing the status of
		the message
	b	The name of the buffer containing the folder
	D	The numeric day of the month when the message was sent
	w	The day of the week, as a 3-character string
	f	The address of the first sender
	F	The name of address of the first sender
	m	The abbreviated month name of the date
	M	The numeric month of the message's date
	n	The index of the message in the folder
	N	The total number of messages in the folder
	l	The subject line
	t	The hour and minute at which the message was sent
	T	The hour, minute, and second of the date
	%	Insert a percent character
	r	The name of the first recipient of the message
	Y	The numeric year of the sending date
	z	The timezone, as a string

The list of formatting options can be extended by the variable
`rm-summary-print-functions'.")


;; Summary interface

(defmacro rm-command-with-folder (command)
  (list 'rm-with-folder
	(list 'call-command command 'current-prefix-arg)))

(defmacro rm-command-in-folder (command)
  (list 'rm-in-folder
	(list 'call-command command 'current-prefix-arg)))

(defvar rm-summary-keymap
  (bind-keys (make-sparse-keymap summary-keymap)
    "d" '(rm-command-with-folder 'rm-mark-message-deletion)
    "Ctrl-d" '(rm-command-with-folder 'rm-mark-message-deletion)
    "u" '(rm-command-with-folder 'rm-unmark-message)
    "x" '(rm-command-with-folder 'rm-expunge)
    "#" '(rm-command-with-folder 'rm-expunge)
    "n" '(rm-command-with-folder 'rm-next-undeleted-message)
    "p" '(rm-command-with-folder 'rm-previous-undeleted-message)
    "N" '(rm-command-with-folder 'rm-next-message)
    "P" '(rm-command-with-folder 'rm-previous-message)
    "SPC" '(rm-command-with-folder 'rm-next-page)
    "BS" '(rm-command-with-folder 'rm-previous-page)
    "t" '(rm-command-with-folder 'rm-toggle-all-headers)
    "g" '(rm-command-with-folder 'rm-get-mail)
    "k" '(rm-command-with-folder 'rm-kill-subject)
    "q" '(rm-command-with-folder 'rm-save-and-quit)
    "v" '(rm-command-with-folder 'read-mail-folder)
    "r" '(rm-command-in-folder 'rm-reply)
    "R" '(rm-command-in-folder '(rm-reply t))
    "f" '(rm-command-in-folder 'rm-followup)
    "F" '(rm-command-in-folder '(rm-followup t))
    "z" '(rm-command-in-folder 'rm-forward)
    "*" '(rm-command-with-folder 'rm-burst-message)
    "s" '(rm-command-with-folder 'rm-output)
    "Ctrl-t" '(rm-command-with-folder 'rm-toggle-threading)
    "Ctrl-s" '(rm-command-with-folder 'rm-sort-folder)
    "|" '(rm-command-with-folder 'rm-pipe-message)))

(defvar rm-summary-functions '((select . rm-summary-select-item)
			       (list . rm-summary-list)
			       (print . rm-summary-print-item)
			       (current . rm-summary-current-item)
			       (after-marking . rm-summary-after-marking)
			       (after-update . rm-summary-after-update))
  "Function vector for summary-mode.")

(defvar rm-summary-mail-buffer nil
  "The buffer whose folder is being summarised.")
(make-variable-buffer-local 'rm-summary-mail-buffer)


;; Summary mechanics

;; Create a summary buffer for the current buffer, and return it. Installs
;; it in rm-summary-buffer as well.
(defun rm-create-summary ()
  (unless rm-summary-buffer
    (setq rm-summary-buffer (make-buffer (concat "*summary of "
						 (buffer-name) ?*)))
    (let
	((mail-buf (current-buffer)))
      (with-buffer rm-summary-buffer
	(setq rm-summary-mail-buffer mail-buf
	      truncate-lines t)
	(call-hook 'read-mail-summary-mode-hook)
	(summary-mode "Mail-Summary" rm-summary-functions rm-summary-keymap)
	(setq major-mode 'read-mail-mode)))))

;;;###autoload
(defun rm-summary (&optional dont-update)
  "Display a summary of all messages in a separate view."
  (interactive)
  (unless rm-summary-buffer
    (rm-create-summary))
  (rm-configure-views rm-summary-buffer (current-buffer))
  (unless dont-update
    (summary-update)
    (rm-summary-update-current)))

;; Configure the window to display the summary in one view, and the
;; mail buffer in the other. Return the view displaying the mail buffer
;; SUMMARY-BUFFER and MAIL-BUFFER are the buffers to display in the two
;; views
(defun rm-configure-views (summary-buffer mail-buffer)
  (let
      (mail-view summary-view)
    (if (= (window-view-count) 2)
	;; Single view + minibuf
	(setq summary-view (current-view)
	      mail-view (split-view))
      (let
	  ((orig (window-view-list)))
	(setq summary-view (car orig)
	      mail-view (nth 1 orig))
	(when (> (window-view-count) 3)
	  (mapc #'(lambda (v)
		    (unless (minibuffer-view-p v)
		      (delete-view v)))
		(nthcdr 2 orig)))))
    (condition-case nil
	(let*
	    ((total-lines (cdr (window-dimensions)))
	     (summary-lines (/ (* (- total-lines 3)
				  mail-summary-percent) 100)))
	  (if (eq mail-display-summary 'bottom)
	      ;; Summary at bottom
	      (progn
		(setq mail-view (prog1 summary-view
				  (setq summary-view mail-view)))
		(set-view-dimensions mail-view nil
				     (- total-lines summary-lines 3)))
	    (set-view-dimensions summary-view nil summary-lines)))
      ;; In case there's not enough room
      (window-error))
    (set-current-view summary-view)
    (goto-buffer summary-buffer)
    (with-view mail-view
      (goto-buffer mail-buffer))
    mail-view))

(defun rm-summary-list ()
  (rm-with-folder
   (if (eq rm-cached-msg-list 'invalid)
       (setq rm-cached-msg-list
	     (if rm-current-msg
		 (nconc (reverse rm-before-msg-list)
			(cons rm-current-msg
			       (copy-sequence rm-after-msg-list)))
	       '()))
     rm-cached-msg-list)))

(defun rm-summary-print-item (item)
  ;; Cache the summary line with the summary buffer as the tag
  (insert (rm-cached-form item (current-buffer)
	    (let
		((arg-list (cons item nil))
		 (format-hooks-alist rm-format-alist))
	      ;; An infinite list of ITEMs
	      (rplacd arg-list arg-list)
	      (apply 'format nil rm-summary-format arg-list)))))

;; Delete all cached summary lines for MSG
(defun rm-invalidate-summary (msg)
  (rm-set-msg-field msg rm-msg-cache
		    (delete-if #'(lambda (x) (bufferp (car x)))
			       (rm-get-msg-field msg rm-msg-cache))))

;; Delete all cached summary lines
(defun rm-invalidate-summary-cache ()
  (mapc #'(lambda (l)
	    (mapc 'rm-invalidate-summary l))
	(list rm-before-msg-list
	      (and rm-current-msg (list rm-current-msg))
	      rm-after-msg-list)))

(defun rm-summary-select-item (item)
  (with-view (rm-configure-views (current-buffer) rm-summary-mail-buffer)
    (rm-display-message item)))
  
(defun rm-summary-current-item ()
  (with-buffer rm-summary-mail-buffer
    rm-current-msg-index))

(defun rm-summary-update-current ()
  (let
      (msg index)
    (with-buffer rm-summary-mail-buffer
      (setq index rm-current-msg-index
	    msg rm-current-msg))
    (if msg
	(progn
	  (summary-update-item msg)
	  (summary-goto-item index))
      ;; No messages, call update to clear everything
      (summary-update))))

(defun rm-summary-after-marking (msg)
  (rm-invalidate-summary msg)
  (if (and (eq (with-buffer rm-summary-mail-buffer rm-current-msg) msg)
	   rm-move-after-deleting)
      (when (with-buffer rm-summary-mail-buffer rm-after-msg-list)
	(rm-with-folder
	 (rm-next-undeleted-message 1)))
    (when (/= (summary-current-index)
	      (with-buffer rm-summary-mail-buffer rm-message-count))
      (summary-next-item 1))))

(defun rm-summary-after-update ()
  (let
      (msg index)
    (with-buffer rm-summary-mail-buffer
      (setq msg rm-current-msg
	    index rm-current-msg-index))
    (when msg
      (summary-highlight-index index))))
