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

(defvar rm-summary-format "%*%a  %2D %3m  %-^16F  %l"
  "A string defining the format of lines in mail summary buffers. It is
copied verbatim except for formatting directives introduced by percent
signs (%). Each directive consists of a percent character, an optional
numeric argument, and a single character specifying what should be
inserted in place of the format directive. See the variable 'rm-format-alist'
for the list of formatting options available.")


;; Summary interface

(defun rm-command-with-folder (command)
  (rm-with-folder
   (call-command command current-prefix-arg)))

(defun rm-command-in-folder (command)
  (rm-in-folder
   (call-command command current-prefix-arg)))

(defvar rm-summary-keymap
  (bind-keys (make-sparse-keymap summary-keymap)
    "d" '(rm-command-with-folder 'rm-mark-message-deletion)
    "Ctrl-d" '(rm-command-with-folder 'rm-mark-message-deletion)
    "u" '(rm-command-with-folder 'rm-unmark-message)
    "U" '(rm-command-with-folder 'rm-unmark-all-messages)
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
    "r" '(rm-command-in-folder 'rm-reply)
    "R" '(rm-command-in-folder '(rm-reply t))
    "f" '(rm-command-in-folder 'rm-followup)
    "F" '(rm-command-in-folder '(rm-followup t))
    "z" '(rm-command-in-folder 'rm-forward)
    "*" '(rm-command-with-folder 'rm-burst-message)
    "s" '(rm-command-with-folder 'rm-output)
    "a" '(rm-command-with-folder 'rm-archive-folder)
    "A" '(rm-command-with-folder 'rm-auto-archive-folder)
    "Ctrl-t" '(rm-command-with-folder 'rm-toggle-threading)
    "G" '(rm-command-with-folder 'rm-sort-folder)
    "+" '(rm-command-with-folder 'rm-add-mailbox)
    "-" '(rm-command-with-folder 'rm-subtract-mailbox)
    "=" '(rm-command-with-folder 'rm-replace-all-mailboxes)
    "Ctrl--" '(rm-command-with-folder 'rm-subtract-all-mailboxes)
    "!" '(rm-command-with-folder 'rm-change-rule)
    "@" '(rm-command-with-folder 'rm-null-rule)
    "|" '(rm-command-with-folder 'rm-pipe-message)))

(defvar rm-summary-functions '((select . rm-summary-select-item)
			       (list . rm-summary-list)
			       (print . rm-summary-print-item)
			       (current . rm-summary-current-item)
			       (after-marking . rm-summary-after-marking)
			       (after-update . rm-summary-after-update))
  "Function vector for summary-mode.")

(defvar rm-summary-folder nil
  "The folder currently being summarised by this buffer.")
(make-variable-buffer-local 'rm-summary-folder)


;; Summary mechanics

;;;###autoload
(defun rm-summarize (folder &optional dont-update)
  "Display a summary of mail folder FOLDER in a separate view."
  (interactive (list (rm-current-folder)))
  (let
      ((buffer (rm-get-folder-field folder rm-folder-summary)))
    (unless buffer
      (setq buffer (make-buffer "*mail-summary*"))
      (with-buffer buffer
	(setq rm-summary-folder folder
	      truncate-lines t
	      y-scroll-step-ratio 2
	      mode-line-format '("----Mail summary: message %l %(%p%)%-"))
	(call-hook 'rm-summary-mode-hook)
	(summary-mode "Mail-Summary" rm-summary-functions rm-summary-keymap)
	(setq major-mode 'read-mail-mode)))
    (rm-set-folder-field folder rm-folder-summary buffer)
    (with-view (rm-configure-views buffer folder)
      (rm-display-current-message folder))
    (unless dont-update
      (summary-update))))

(defun rm-kill-summary (folder)
  (let
      ((buffer (rm-get-folder-field folder rm-folder-summary)))
    (rm-invalidate-summary-cache folder)
    (when buffer
      (with-buffer buffer
	(kill-all-local-variables))
      (kill-buffer buffer))
    (when (= (window-view-count) 3)
      (delete-view))))
			
;; Returns a view of the actual mail folder, even if it has to create one.
(defun rm-summary-view ()
  (let*
      ((message (rm-get-folder-field rm-summary-folder rm-folder-current-msg))
       (buffer (and message (mark-file
			     (rm-get-msg-field message rm-msg-mark)))))
    (unless (and buffer (get-buffer-view buffer))
      ;; Couldn't find the view, try to create one
      (rm-configure-views (current-buffer) rm-summary-folder))))
  
;; When called from a summary buffer, installs the summary's mail buffer
;; and executes FORMS.
(defmacro rm-with-folder (&rest forms)
  `(let
       ((view (rm-summary-view)))
     (with-view view ,@forms)))

;; Switch to the buffer containing the folder and execute FORMS. Don't
;; switch back afterwards
(defmacro rm-in-folder (&rest forms)
  `(let
       ((view (rm-summary-view)))
     (set-current-view view)
     ,@forms))
      
;; Configure the window to display the summary in one view, and the
;; folder in the other. Return the view displaying the folder
(defun rm-configure-views (summary-buffer folder)
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
    ;; Ensure that rm-open-folders has the correct view
    (mapc #'(lambda (cell)
	      (when (and (eq (cdr cell) folder)
			 (or (not (viewp (car cell)))
			     (eq (car cell) summary-view)))
		(setcar cell mail-view))) rm-open-folders)
    mail-view))

(defun rm-summary-list ()
  (if (eq (rm-get-folder-field rm-summary-folder rm-folder-cached-list)
	  'invalid)
      (rm-set-folder-field
       rm-summary-folder rm-folder-cached-list
       (if (rm-get-folder-field rm-summary-folder rm-folder-current-msg)
	   (nconc (reverse (rm-get-folder-field
			    rm-summary-folder rm-folder-before-list))
		  (cons (rm-get-folder-field
			 rm-summary-folder rm-folder-current-msg)
			(copy-sequence (rm-get-folder-field
					rm-summary-folder
					rm-folder-after-list))))
	 '()))
    (rm-get-folder-field rm-summary-folder rm-folder-cached-list)))

(defun rm-summary-print-item (item)
  ;; Cache the summary line with the buffer as the tag
  (insert (rm-cached-form item (current-buffer)
	    (rm-format
	     ;; Look in the message's buffer. This allows buffer-local
	     ;; bindings of rm-summary-format to work correctly
	     (with-buffer (mark-file (rm-get-msg-field item rm-msg-mark))
	       rm-summary-format)
	     item)))
  ;; The summary-face property of the message allows the face of
  ;; the item to be set
  (let
      ((face (rm-message-get item 'summary-face)))
    (when face
      (make-extent (start-of-line) (cursor-pos) (list 'face face)))))

;; Delete all cached summary lines for MSG
(defun rm-invalidate-summary (msg)
  (rm-set-msg-field msg rm-msg-cache
		    (delete-if #'(lambda (x)
				   (bufferp (car x)))
			       (rm-get-msg-field msg rm-msg-cache))))

;; Delete all cached summary lines in FOLDER
(defun rm-invalidate-summary-cache (folder)
  (let
      ((summary (rm-get-folder-field folder rm-folder-summary)))
    (when summary
      (rm-map-messages #'(lambda (m)
			   (rm-invalidate-tag m summary)) folder))))

(defun rm-summary-select-item (item)
  (let
      ((folder rm-summary-folder))
    (with-view (rm-configure-views (current-buffer) folder)
      (rm-display-message folder item))))
  
(defun rm-summary-current-item ()
  (rm-get-folder-field rm-summary-folder rm-folder-current-index))

(defun rm-summary-update-current ()
  (let
      ((message (rm-get-folder-field rm-summary-folder rm-folder-current-msg)))
    (if message
	(progn
	  (summary-update-item message)
	  (summary-goto-item (rm-summary-current-item)))
      ;; No messages, call update to clear everything
      (summary-update))))

(defun rm-summary-after-marking (msg)
  (rm-invalidate-summary msg)
  (if (and rm-move-after-deleting
	   (eq (rm-get-folder-field rm-summary-folder rm-folder-current-msg)
	       msg))
      (when (rm-get-folder-field rm-summary-folder rm-folder-after-list)
	(rm-with-folder
	 (rm-next-undeleted-message 1)))
    (when (/= (summary-current-index)
	      (rm-get-folder-field rm-summary-folder rm-folder-message-count))
      (summary-next-item 1))))

(defun rm-summary-after-update ()
  (when (rm-get-folder-field rm-summary-folder rm-folder-current-msg)
    (summary-highlight-index
     (rm-get-folder-field rm-summary-folder rm-folder-current-index))))
