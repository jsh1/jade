;;;; rm-thread.jl -- Thread support for mail reader. Also sorting
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

;; Commentary:
;;
;; This is incredibly basic, all it really does is group messages into
;; unordered threads (by intersecting references or common subject lines)
;; then sort these threads by time of sending.
;;
;; But then again, I'm not sure if doing anything more is really required.
;; The only time I want threaded reading is for high volume mailing lists,
;; and it seems that so few messages have correct in-reply-to or (even more
;; rarely) references headers, it may not be possible to do this correctly
;; anyway..
;;
;; I'll probably fix this to do proper tree-oriented threading when I
;; get fed up with this method..
;;
;; This file also implements general sorting of display order. This is
;; so trivial it's almost embarassing ;-)

(require 'read-mail)
(provide 'rm-thread)

;; Suppress annoying compiler warnings
(eval-when-compile (require 'rm-summary))


;; Configuration

(defvar rm-thread-using-subject t
  "When t messages with the same subject will be put in the same thread.")

(defvar rm-intra-thread-sort-key 'date
  "Key to sort messages in each thread by.")

(defvar rm-inter-thread-sort-key 'date
  "Key to sort threads by.")

(defvar rm-auto-thread-new-messages t
  "When t, new messages incorporated into a threaded folder will also be
threaded.")

(defvar rm-sort-predicates
  (list (cons 'location
	      #'(lambda (x y)
		  (< (rm-get-msg-field x rm-msg-mark)
		     (rm-get-msg-field y rm-msg-mark))))
	(cons 'date
	      #'(lambda (x y)
		  (let
		      ((dx (rm-get-date-vector x))
		       (dy (rm-get-date-vector y)))
		    ;; Date-less messages earlier than dated messages
		    (if (and dx dy)
			(< (aref dx mail-date-epoch-time)
			   (aref dy mail-date-epoch-time))
		      dy))))
	(cons 'subject
	      #'(lambda (x y)
		  (< (rm-get-actual-subject x) (rm-get-actual-subject y))))
	(cons 'sender
	      #'(lambda (x y)
		  (< (rm-get-senders x) (rm-get-senders y))))
	(cons 'recipients
	      #'(lambda (x y)
		  (< (rm-get-recipients x) (rm-get-recipients y))))
	(cons 'lines
	      #'(lambda (x y)
		  (< (rm-get-msg-field x rm-msg-total-lines)
		     (rm-get-msg-field y rm-msg-total-lines)))))
  "List of (SORT-TYPE . PREDICATE) defining functions that can be used to
order messages. Each PREDICATE may be called with two arguments, the two
messages to be ordered. It should return t when the first message should
be shown before the second.")


;; Variables

;;;###autoload
(defun rm-thread-folder ()
  "Display messages in the current folder by thread."
  (interactive)
  (let*
      ((folder (rm-current-folder))
       (threads nil)
       (message-lists (list (rm-get-folder-field folder rm-folder-before-list)
			    (list (rm-get-folder-field
				   folder rm-folder-current-msg))
			    (rm-get-folder-field
			     folder rm-folder-after-list))))
   (unless (rm-get-folder-field folder rm-folder-current-msg)
    (error "No messages to thread!"))
   (message "Threading folder..." t)
    (mapc
     ;; Called for a list of messages
     #'(lambda (message-list)
	 (mapc
	  ;; Called for a single message
	  #'(lambda (message)
	      ;; Called for each message. Basic strategy is to
	      ;; keep creating new threads, trying to join them up
	      ;; as we go (to cope with disordered messages)
	      (let
		  ((message-id (rm-get-message-id message))
		   (references (cons (rm-get-in-reply-to message)
				     (rm-get-references message)))
		   (tied-threads nil))
		(mapc
		 ;; Called for a single thread of messages
		 #'(lambda (thread)
		     (catch 'thread
		       (mapc
			;; Called for a single message in the thread
			#'(lambda (thread-message)
			    ;; Should really look for an intersection in
			    ;; the References headers?
			    (when (or (and rm-thread-using-subject
					   (string= (rm-get-actual-subject
						     message)
						    (rm-get-actual-subject
						     thread-message)))
				      (memq (rm-get-message-id
					     thread-message) references)
				      (eq message-id (rm-get-in-reply-to
						      thread-message))
				      (memq message-id (rm-get-references
							thread-message)))
			      ;; Note where we should thread the MESSAGE
			      (setq tied-threads (cons thread tied-threads))
			      ;; Don't need to scan this THREAD anymore
			      (throw 'thread nil)))
			thread)))
		     threads)
		(if tied-threads
		    ;; Link all of TIED-THREADS into one, and add MESSAGE
		    (progn
		      ;; Delete the threads being tied..
		      (setq threads (delete-if #'(lambda (x)
						   (memq x tied-threads))
					       threads))
		      ;; ..then cons them onto the head as one
		      (setq threads (cons (apply 'nconc (list message)
						 tied-threads)
					  threads)))
		  ;; No thread for MESSAGE, start a new one
		  (setq threads (cons (list message) threads)))))
	  message-list))
     message-lists)
    ;; First sort the messages in each thread
    (let
	((rm-pred (cdr (assq rm-intra-thread-sort-key rm-sort-predicates))))
      (setq threads (mapcar
		     #'(lambda (thread)
			 (sort thread rm-pred))
		     threads)))
    ;; Then sort the threads themselves
    (let
	((rm-pred (cdr (assq rm-inter-thread-sort-key rm-sort-predicates))))
      (setq threads (sort threads #'(lambda (x y)
				      (funcall rm-pred (car x) (car y))))))
    ;; Ok, so we now have a list of THREADS, spit them out as the
    ;; list(s) of messages?
    (rm-set-folder-field folder rm-folder-sort-key 'thread)
    (rm-fix-msg-lists folder (apply 'nconc threads))
    (message "Threading folder...done" t)))

;; Install the list of messages ALL, preserving the current message, and
;; splitting ALL around it. Do this destructively
(defun rm-fix-msg-lists (folder all)
  (let
      ((before nil)
       (after all)
       (current (rm-get-folder-field folder rm-folder-current-msg))
       (index 0))
    (while (not (eq (car after) current))
      (setq after (prog1
		      (cdr after)
		    (rplacd after before)
		    (setq before after))
	    index (1+ index)))
    (rm-set-folder-field folder rm-folder-before-list before)
    (rm-set-folder-field folder rm-folder-after-list (cdr after))
    (rm-set-folder-field folder rm-folder-current-index index)
    (rm-set-folder-field folder rm-folder-cached-list 'invalid)
    (rm-invalidate-status-cache folder)
    (rm-display-current-message folder t)
    (when (rm-get-folder-field folder rm-folder-summary)
      (rm-invalidate-summary-cache folder)
      (rm-with-summary folder
       (summary-update)))))

;;;###autoload
(defun rm-toggle-threading ()
  (interactive)
  "Toggle threaded display of messages in the current folder."
  (let
      ((folder (rm-current-folder)))
    (if (eq (rm-get-folder-field folder rm-folder-sort-key) 'thread)
	(rm-sort-folder 'location)
      (rm-thread-folder))))


;; Folder sorting

;;;###autoload
(defun rm-sort-folder (key &optional reversed)
  "Select the order in which messages are displayed in the current folder
as that defined by the symbol KEY. Standard options for KEY include:

  location		Sort by physical location in the folder
  date			Sort by date of sending
  subject		Sort by subject line
  sender		Sort by name of sender
  recipients		Sort by names of recipients
  lines			Sort by the number of lines in the message

Extra sort options can be added by changing the `rm-sort-predicates'
variable.

If the REVERSED argument is non-nil the order of the sort is switched,
i.e. instead of sorting smallest to greatest, sort greatest to smallest.

When called interactively, KEY is prompted for, and REVERSED is taken from
the raw prefix argument."
  (interactive
   (let
       ((arg current-prefix-arg))
     (list (intern (prompt-from-list
		    (mapcar #'(lambda (p)
				(symbol-name (car p))) rm-sort-predicates)
		    "Sort key:"))
	   arg)))
  (let
      ((folder (rm-current-folder))
       (rm-sort-pred (cdr (assq key rm-sort-predicates))))
    (unless rm-sort-pred
      (error "Unknown sort key: %s" key))
    (unless (rm-get-folder-field folder rm-folder-current-msg)
      (error "No messages to sort!"))
    (rm-set-folder-field folder rm-folder-sort-key key)
    (rm-fix-msg-lists folder (sort (nconc (rm-get-folder-field
					   folder rm-folder-before-list)
					  (list (rm-get-folder-field
						 folder rm-folder-current-msg))
					  (rm-get-folder-field
					   folder rm-folder-after-list))
				   (if reversed
				       #'(lambda (x y)
					   (not (funcall rm-sort-pred x y)))
				     rm-sort-pred)))))
