;;;; rm-thread.jl -- Thread support for mail reader
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

(require 'read-mail)
(provide 'rm-thread)

;;;###autoload
(defun rm-thread-folder ()
  "Display messages in the current folder by thread."
  (interactive)
  (let
      ((threads nil)
       (message-lists (list rm-before-msg-list
			    (list rm-current-msg)
			    rm-after-msg-list)))
    (mapc
     #'(lambda (message-list)
	 (mapc
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
		 #'(lambda (thread)
		     (catch 'thread
		       (mapc
			#'(lambda (thread-message)
			    ;; Should really look for an intersection in
			    ;; the References headers?
			    (when (or (memq (rm-get-message-id
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
		    (setq threads (cons (apply 'nconc (list message)
					       tied-threads)
					(delete-if #'(lambda (t)
						       (memq t tied-threads))
						   threads)))
		  ;; No thread for MESSAGE, start a new one
		  (setq threads (cons (list message) threads)))))
	  message-list))
     message-lists)
    ;; Ok, so we now have a list of THREADS, spit them out as the
    ;; list(s) of messages? Really they need to be sorted first..
    ;;(format (stderr-file) "rm-thread-folder: %S\n" threads)
    (let
	((before nil)
	 (after (apply 'nconc threads)))
      (setq rm-current-msg-index 0)
      (while (not (eq (car after) rm-current-msg))
	(setq after (prog1
			(cdr after)
		      (rplacd after before)
		      (setq before after))
	      rm-current-msg-index (1+ rm-current-msg-index)))
      (setq rm-before-msg-list before
	    rm-after-msg-list (cdr after)
	    rm-cached-msg-list 'invalid)
      (rm-with-summary
       (summary-update)))))
