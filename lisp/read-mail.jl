;;;; read-mail.jl -- Simple mail reading mode
;;;  Copyright (C) 1997 John Harper <john@dcs.warwick.ac.uk>
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

(require 'maildefs)
(require 'mail-headers)
(provide 'read-mail)

;; Suppress annoying compiler warnings
(eval-when-compile (require 'rm-summary))


;; Configuration

(defvar rm-auto-next-message t
  "When t the next message will automatically be displayed when trying to
page past the limits of the current message.")

(defvar rm-move-after-deleting t
  "When t move to the next message after deleting the current message.")

(defvar rm-status-format "%b-%n/%N: %f"
  "A string defining the format of the text replacing the `Jade: BUFFER'
text in the mail buffer's status line. The format conversions available
are the same as those for the `rm-summary-format' variable.")

(defvar rm-format-alist nil
  "An alist of (CHARACTER . FUNCTION) defining formatting directives for the
rm-summary-format-string. The function is called as: (FUNCTION MESSAGE-
STRUCTURE); it should return the string to be inserted (no newlines).")

(defvar rm-saved-cache-tags '(sender-list recipient-list date-vector)
  "List of cache tags whose values should be saved in the headers of each
message (to improve performance when the folder is next loaded).")


;; Variables

(defvar rm-open-folders nil
  "List of (VIEW . FOLDER) describing all open folders.")

(defvar rm-open-mailboxes nil
  "List of buffers currently opened for mail reading.")

(defvar rm-keymap
  (bind-keys (make-sparse-keymap)
    "n" 'rm-next-undeleted-message
    "p" 'rm-previous-undeleted-message
    "N" 'rm-next-message
    "P" 'rm-previous-message
    "t" 'rm-toggle-all-headers
    "SPC" 'rm-next-page
    "BS" 'rm-previous-page
    "h" 'rm-summarize
    "d" 'rm-mark-message-deletion
    "Ctrl-d" 'rm-mark-message-deletion
    "x" 'rm-expunge
    "#" 'rm-expunge
    "g" 'rm-get-mail
    "k" 'rm-kill-subject
    "q" 'rm-save-and-quit
    "u" 'rm-unmark-message
    "v" 'read-mail-folder
    "r" 'rm-reply
    "R" '(rm-reply t)
    "f" 'rm-followup
    "F" '(rm-followup t)
    "z" 'rm-forward
    "*" 'rm-burst-message
    "s" 'rm-output
    "|" 'rm-pipe-message
    "+" 'rm-add-mailbox
    "-" 'rm-subtract-mailbox
    "Ctrl--" 'rm-subtract-all-mailboxes
    "!" 'rm-change-rule
    "Ctrl-t" 'rm-toggle-threading
    "Ctrl-s" 'rm-sort-folder)
  "Keymap for reading mail")
  
(defvar rm-last-mailbox mail-folder-dir
  "File name of the most recently opened folder. Used as a default value for
the next prompt.")

(defvar rm-buffer-messages 'invalid
  "A buffer-local variable, when not the symbol `invalid', the list of
messages in the buffer, in first to last order.")
(make-variable-buffer-local 'rm-buffer-messages)


;; Entry points

;;;###autoload
(defun read-mail (&optional rule)
  "Read mail."
  (interactive (list (and current-prefix-arg (rm-prompt-for-rule))))
  (read-mail-folder mail-default-folder rule))

;;;###autoload
(defun read-mail-folder (boxes &optional rule name)
  "Read mail stored in the files named by the list BOXES. When defined
RULE is the message restriction rule to apply."
  (interactive
   (let
       ((arg current-prefix-arg))
     (list (prompt-for-folder "Mailbox to open:" rm-last-mailbox)
	   (and arg (rm-prompt-for-rule)))))
  (let
      ((folder (rm-make-folder rule name)))
    (mapc #'(lambda (box)
	      (unless (file-exists-p box)
		(setq box (expand-file-name box mail-folder-dir)))
	      (rm-add-mailbox box folder))
	  (if (listp boxes) boxes (list boxes)))
    (rm-display-folder folder)))


;; Mailbox buffer handling

(defun rm-open-mailbox (mailbox)
  (let
      ((buffer (find-file-read-only mailbox t)))
    (unless buffer
      (error "Can't open mailbox %s" mailbox))
    (with-buffer buffer
      (read-mail-mode))
    (call-hook 'rm-open-mailbox-hook (list buffer))
    (rm-parse-buffer buffer)))

(defun rm-close-mailbox (mailbox &optional dont-update)
  (let
      ((buffer (get-file-buffer mailbox)))
    (when buffer
      (unless dont-update
	(rm-update-flags buffer)
	(save-file buffer))
      (when (buffer-modified-p buffer)
	(message (concat  "Folder " (buffer-name buffer)
			  " contains unsaved changes!")))
      (unless (> (rm-folders-with-box buffer) 1)
	(with-buffer buffer
	  (fundamental-mode))
	(kill-buffer buffer)))))

(defun read-mail-mode ()
  "Read-Mail Mode:\n
Major mode for viewing mail folders. Local bindings are:\n
\\{rm-keymap}"
  (unless (eq major-mode 'read-mail-mode)
    (when major-mode-kill
      (funcall major-mode-kill (current-buffer)))
    (setq rm-open-mailboxes (cons (current-buffer) rm-open-mailboxes)
	  mode-name "Mail"
	  major-mode 'read-mail-mode
	  major-mode-kill 'read-mail-mode-kill
	  keymap-path (cons 'rm-keymap keymap-path))
    (call-hook 'read-mail-mode-hook)))

(defun read-mail-mode-kill ()
  (setq rm-open-mailboxes (delq (current-buffer) rm-open-mailboxes))
  (kill-all-local-variables))


;; Internal folder structure

;; The message list is kept in three parts. The messages before the
;; current message in reverse order, the current message itself, and the
;; messages after the current message.

(defconst rm-folder-type 0)

(defconst rm-folder-before-list 1
  "The messages before the current message, in reverse order.")

(defconst rm-folder-current-msg 2
  "The currently displayed message, or nil.")

(defconst rm-folder-after-list 3
  "The messages after the current message.")

(defconst rm-folder-current-index 4
  "The index in the folder of the current message.")

(defconst rm-folder-message-count 5
  "The total number of messages in the current folder.")

(defconst rm-folder-cached-list 6
  "A cached copy of the full message list, used by the summary. Or the symbol
invalid to mark that it needs to be rebuilt.")

(defconst rm-folder-summary 7
  "The buffer summarizing this folder, or nil.")

(defconst rm-folder-boxes 8
  "The list of mailboxes from which this folder selects its messages.")

(defconst rm-folder-rule 9
  "The restriction rule defining the messages being displayed. Or nil to
show all messages.")

(defconst rm-folder-name 10
  "A string defining the name of this folder.")

(defconst rm-folder-sort-key 11
  "The name of the sort key, nil by default.")

(defconst rm-folder-struct-size 12)


(defmacro rm-set-folder-field (folder field value)
  (list 'aset folder field value))

(defmacro rm-get-folder-field (folder field)
  (list 'aref folder field))

(defun rm-make-folder (&optional rule name)
  (let
      ((folder (make-vector rm-folder-struct-size)))
    (rm-set-folder-field folder rm-folder-type 'folder)
    (rm-set-folder-field folder rm-folder-rule rule)
    (rm-set-folder-field folder rm-folder-name name)
    (rm-set-folder-field folder rm-folder-message-count 0)
    (rm-set-folder-field folder rm-folder-cached-list 'invalid)
    folder))

(defun rm-folder-p (object)
  (and (vectorp object) (eq (aref object rm-folder-type) 'folder)))

(defun rm-rebuild-folder (folder)
  (let
      ((original-index (rm-get-folder-field folder rm-folder-current-index)))
    (rm-set-folder-field folder rm-folder-before-list nil)
    (rm-set-folder-field folder rm-folder-current-msg nil)
    (rm-set-folder-field folder rm-folder-after-list nil)
    (rm-set-folder-field folder rm-folder-current-index 0)
    (rm-set-folder-field folder rm-folder-cached-list 'invalid)
    (let*
	((messages (rm-prune-messages
		    (rm-parse-mailboxes
		     (rm-get-folder-field folder rm-folder-boxes)) folder))
	 (count (length messages)))
      (let
	  (before current after)
	(cond
	 ((or (null original-index) (zerop original-index))
	  ;; Make the current message the first
	  (setq current (car messages)
		after (cdr messages)
		before nil))
	 ((> count original-index)
	  ;; Make the current message the one at ORIGINAL-INDEX
	  (setq after (nthcdr (1- original-index) messages)
		current (car (cdr after))
		after (prog1 (cdr (cdr after))
			(rplacd after nil))
		before (nreverse messages)))
	 (t
	  ;; Make the current message the last
	  (setq before (nreverse messages)
		after nil
		current (car before)
		before (cdr before))))
	(rm-set-folder-field folder rm-folder-current-msg current)
	(rm-set-folder-field folder rm-folder-before-list before)
	(rm-set-folder-field folder rm-folder-after-list after)
	(rm-set-folder-field folder rm-folder-message-count count)
	(rm-set-folder-field folder rm-folder-current-index
			     (length before))))
    (when (rm-get-folder-field folder rm-folder-summary)
      (rm-invalidate-summary-cache folder))
    (rm-invalidate-status-cache folder)))
    
(defun rm-add-mailbox (mailbox folder &optional redisplay)
  (interactive
   (list (prompt-for-folder "Mailbox to add" rm-last-mailbox)
	 (rm-current-folder) t))
  (rm-set-folder-field folder rm-folder-boxes
		       (cons mailbox
			     (rm-get-folder-field folder rm-folder-boxes)))
  (let
      ((messages (rm-prune-messages
		  (rm-parse-mailboxes (list mailbox)) folder)))
    (if (null (rm-get-folder-field folder rm-folder-current-msg))
	(let
	    ((count (length messages)))
	  (setq messages (nreverse messages))
	  (rm-set-folder-field folder rm-folder-current-index (1- count))
	  (rm-set-folder-field folder rm-folder-message-count count)
	  (rm-set-folder-field folder rm-folder-current-msg (car messages))
	  (rm-set-folder-field folder rm-folder-before-list (cdr messages))
	  (rm-set-folder-field folder rm-folder-after-list nil))
      (rm-set-folder-field folder rm-folder-message-count
			   (+ (rm-get-folder-field
			       folder rm-folder-message-count)
			      (length messages)))
      (rm-set-folder-field folder rm-folder-after-list
			   (nconc
			    (rm-get-folder-field folder rm-folder-after-list)
			    messages)))
    (rm-set-folder-field folder rm-folder-cached-list 'invalid)
    (setq rm-last-mailbox mailbox))
  (rm-invalidate-status-cache folder)
  (when redisplay
    (rm-display-current-message folder t))
  (when (rm-get-folder-field folder rm-folder-summary)
    (rm-invalidate-summary-cache folder)
    (when redisplay
      (rm-with-summary folder
	(summary-update)))))

(defun rm-subtract-mailbox (mailbox folder)
  (interactive
   (let
       ((folder (rm-current-folder)))
     (list (prompt-from-list (rm-get-folder-field folder rm-folder-boxes)
			     "Mailbox to subtract:")
	   folder)))
  (rm-set-folder-field folder rm-folder-boxes
		       (delete mailbox (rm-get-folder-field
					folder rm-folder-boxes)))
  (rm-rebuild-folder folder)
  (rm-close-mailbox mailbox)
  (rm-display-current-message folder t)
  (when (rm-get-folder-field folder rm-folder-summary)
    (rm-with-summary folder
      (summary-update))))

(defun rm-subtract-all-mailboxes (folder)
  (interactive (list (rm-current-folder)))
  (mapc 'rm-close-mailbox (rm-get-folder-field folder rm-folder-boxes))
  (rm-set-folder-field folder rm-folder-boxes nil)
  (rm-rebuild-folder folder)
  (rm-display-current-message folder t)
  (when (rm-get-folder-field folder rm-folder-summary)
    (rm-with-summary folder
      (summary-update))))

(defun rm-change-rule (folder rule)
  (interactive
   (list (rm-current-folder)
	 (rm-prompt-for-rule)))
  (rm-set-folder-field folder rm-folder-rule rule)
  (rm-rebuild-folder folder)
  (rm-display-current-message folder t)
  (when (rm-get-folder-field folder rm-folder-summary)
    (rm-with-summary folder
      (summary-update))))

(defun rm-display-folder (folder)
  (setq rm-open-folders (cons (cons (current-view) folder) rm-open-folders))
  (rm-display-current-message folder t)
  (when mail-display-summary
    (rm-summarize folder)))

;; Apply FOLDER's restriction rule to MESSAGE-LISTS, creating a new
;; list of any messages matching
(defun rm-prune-messages (message-lists folder)
  (let
      ((rule (rm-get-folder-field folder rm-folder-rule)))
    (if rule
	(progn
	  (require 'rm-restrict)
	  (let
	      (bits)
	    (mapc #'(lambda (msgs)
		      (setq bits (cons (rm-filter-by-rule msgs rule) bits)))
		  message-lists)
	    (apply 'nconc bits)))
      ;; Can't use append to join lists since that doesn't clone the last one
      (apply 'nconc (mapcar 'copy-sequence message-lists)))))

;; Return the folder being displayed in the current view
(defun rm-current-folder ()
  (or (and (boundp 'rm-summary-folder) rm-summary-folder)
      (cdr (assq (current-view) rm-open-folders))))

(defun rm-folders-with-box (buffer)
  (apply '+ (mapcar #'(lambda (cell)
			(if (memq buffer (rm-get-folder-field
					  (cdr cell) rm-folder-boxes))
			    1
			  0)) rm-open-folders)))

(defun rm-add-messages (buffer msgs)
  (unrestrict-buffer)
  (when msgs
    (with-buffer buffer
      (unless (eq rm-buffer-messages 'invalid)
       (setq rm-buffer-messages (nconc rm-buffer-messages msgs))))
    (mapc #'(lambda (cell)
             (when (memq buffer (rm-get-folder-field
                                 (cdr cell) rm-folder-boxes))
               (rm-rebuild-folder (cdr cell))))
         rm-open-folders)))


;; Internal message structure

;; FLAGS is a list of symbols, including replied, unread, filed,
;; forwarded, deleted, anything else?
(defconst rm-msg-mark 0)
(defconst rm-msg-total-lines 1)
(defconst rm-msg-header-lines 2)
(defconst rm-msg-flags 3)
(defconst rm-msg-cache 4)
(defconst rm-msg-struct-size 5)

(defmacro rm-set-msg-field (msg field value)
  (list 'aset msg field value))

(defmacro rm-get-msg-field (msg field)
  (list 'aref msg field))

;; Create an unused message structure
(defmacro rm-make-msg ()
  '(make-vector rm-msg-struct-size))

;; Set FLAG in MSG
(defun rm-set-flag (msg flag)
  (unless (memq flag (rm-get-msg-field msg rm-msg-flags))
    (rm-set-msg-field msg rm-msg-flags
		      (cons flag (rm-get-msg-field msg rm-msg-flags)))
    (rm-map-msg-folders #'(lambda (msg folder)
			    (when (rm-get-folder-field
				   folder rm-folder-summary)
			      (rm-invalidate-summary msg)
			      (rm-with-summary folder
			        (summary-update-item msg))))
			msg)))

;; Unset FLAG in MSG
(defun rm-clear-flag (msg flag)
  (when (memq flag (rm-get-msg-field msg rm-msg-flags))
    (rm-set-msg-field msg rm-msg-flags
		      (delq flag (rm-get-msg-field msg rm-msg-flags)))
    (rm-map-msg-folders #'(lambda (msg folder)
			    (when (rm-get-folder-field
				   folder rm-folder-summary)
			      (rm-invalidate-summary msg)
			      (rm-with-summary folder
			        (summary-update-item msg))))
			msg)))

;; Return t if FLAG is set in MSG
(defmacro rm-test-flag (msg flag)
  (list 'memq flag (list 'rm-get-msg-field msg rm-msg-flags)))

;; Call (FUNCTION MSG FOLDER) for all FOLDERS containing MSG
(defun rm-map-msg-folders (function msg)
  (mapc #'(lambda (cell)
	    (when (or (eq msg (rm-get-folder-field (cdr cell)
						   rm-folder-current-msg))
		      (memq msg (rm-get-folder-field (cdr cell)
						     rm-folder-before-list))
		      (memq msg (rm-get-folder-field (cdr cell)
						     rm-folder-after-list)))
	      (funcall function msg (cdr cell))))
	rm-open-folders))

;; Call like (rm-cached-form MSG TAG FORM) instead of just FORM, to
;; cache its value in MSG under key TAG
(defmacro rm-cached-form (msg tag form)
  `(let
       ((_tem_ (assq ,tag (rm-get-msg-field ,msg rm-msg-cache))))
     (if _tem_
	 (cdr _tem_)
       (setq _tem_ ,form)
       (rm-set-msg-field ,msg rm-msg-cache
			 (cons (cons ,tag _tem_)
			       (rm-get-msg-field ,msg rm-msg-cache)))
       _tem_)))
(put 'rm-cached-form 'lisp-indent 2)

;; Returns t if TAG is cached by MSG
(defmacro rm-tag-cached-p (msg tag)
  `(assq ,tag (rm-get-msg-field ,msg rm-msg-cache)))

;; If TAG is cached by MSG, remove its cached value
(defun rm-invalidate-tag (msg tag)
  (let
      ((cell (rm-tag-cached-p msg tag)))
    (when cell
      (rm-set-msg-field msg rm-msg-cache
			(delq cell (rm-get-msg-field msg rm-msg-cache))))))


;; Message structures and list manipulation

;; Twiddle the message lists so that the next message is current
(defun rm-move-forwards (folder)
  (let
      ((before (rm-get-folder-field folder rm-folder-before-list))
       (current (rm-get-folder-field folder rm-folder-current-msg))
       (after (rm-get-folder-field folder rm-folder-after-list)))
    (setq after (prog1
		    (cdr after)
		  (setq current
			(prog1
			    (car after)
			  (rplacd after before)
			  (rplaca after current)
			  (setq before after)))))
    (rm-set-folder-field folder rm-folder-before-list before)
    (rm-set-folder-field folder rm-folder-current-msg current)
    (rm-set-folder-field folder rm-folder-after-list after)
    (rm-set-folder-field folder rm-folder-current-index
			 (1+ (rm-get-folder-field folder
						  rm-folder-current-index)))))

;; Twiddle the message lists so that the previous message is current
(defun rm-move-backwards (folder)
  (let
      ((before (rm-get-folder-field folder rm-folder-before-list))
       (current (rm-get-folder-field folder rm-folder-current-msg))
       (after (rm-get-folder-field folder rm-folder-after-list)))
    (setq before (prog1
		     (cdr before)
		   (setq current
			 (prog1
			     (car before)
			   (rplacd before after)
			   (rplaca before current)
			   (setq after before)))))
    (rm-set-folder-field folder rm-folder-before-list before)
    (rm-set-folder-field folder rm-folder-current-msg current)
    (rm-set-folder-field folder rm-folder-after-list after)
    (rm-set-folder-field folder rm-folder-current-index
			 (1- (rm-get-folder-field folder
						  rm-folder-current-index)))))

;; Make MSG the current message, rejigging the message lists as necessary
;; Doesn't fix the variables defining some of the positions in the current
;; message.
(defun rm-make-message-current (folder msg)
  (unless (eq msg (rm-get-folder-field folder rm-folder-current-msg))
    (if (memq msg (rm-get-folder-field folder rm-folder-after-list))
	;; Move forwards
	(while (not (eq (rm-get-folder-field folder rm-folder-current-msg)
			msg))
	  (rm-move-forwards folder))
      ;; Move backwards
      (while (not (eq (rm-get-folder-field folder rm-folder-current-msg) msg))
	(rm-move-backwards folder)))))


;; Mailbox parsing

;; Parse the list of files named by BOXES, return a list of lists of
;; messages. The returned message lists _may_not_ be altered destructively
(defun rm-parse-mailboxes (boxes)
  (mapcar 'rm-open-mailbox boxes))

;; Return a list of message structures defining all messages found in
;; BUFFER. This list is sorted by buffer position, first to last. This
;; list _may_not_ be altered destructively
(defun rm-parse-buffer (buffer)
  (with-buffer buffer
    (when (eq rm-buffer-messages 'invalid)
      (save-restriction
	(unrestrict-buffer)
	(let
	    ;; If we scan the buffer from start to end we create the
	    ;; list in reverse order
	    ((pos (start-of-buffer))
	     (msgs nil)
	     (count 0))
	  (while (and pos (< pos (end-of-buffer)) (rm-message-start-p pos))
	    (setq msgs (cons (rm-parse-message pos) msgs)
		  count (1+ count)
		  pos (forward-line (rm-get-msg-field (car msgs)
						      rm-msg-total-lines)
				    pos)))
	  ;; Okay, we now have all messages
	  (setq rm-buffer-messages (nreverse msgs)))))
    rm-buffer-messages))

;; Parse one message in the current buffer at position START, and return a
;; message structure. The buffer should be unrestricted
(defun rm-parse-message (start)
  (let
      ((end (re-search-forward "^$" start))
       (msg (rm-make-msg))
       pos tem)
    (restrict-buffer start (or end (end-of-buffer)))
    (rm-set-msg-field msg rm-msg-mark (make-mark start))
    (rm-set-msg-field msg rm-msg-header-lines (- (pos-line (restriction-end))
						 (pos-line start)))
    (when (setq pos (mail-find-header "^X-Jade-Flags-v1" start))
      (rm-set-msg-field msg rm-msg-flags (read (cons (current-buffer) pos))))
    (when (setq pos (mail-find-header "^X-Jade-Cache-v1" start))
      (rm-set-msg-field msg rm-msg-cache (read (cons (current-buffer) pos))))
    (when (mail-find-header "Replied" start)
      ;; MH annotates replied messages like this
      (rm-set-flag msg 'replied))
    (unrestrict-buffer)
    ;; Find the total number of lines in the message
    (if (null end)
	(setq pos (end-of-buffer))
      ;; Find the start of the next message
      (setq pos (forward-line 1 start))
      (while (and pos (setq pos (re-search-forward mail-message-start pos))
		  (not (rm-message-start-p pos)))
	(setq pos (forward-line 1 pos))))
    (rm-set-msg-field msg rm-msg-total-lines (- (if pos
						    (pos-line pos)
						  (buffer-length))
						(pos-line start)))
    msg))

;; Returns t if POS is the start of a message. Munges the regexp history
(defun rm-message-start-p (pos)
  (and (looking-at mail-message-start pos)
       (or (equal pos (start-of-buffer))
	   (looking-at "\n\n" (forward-char -2 (match-start))))))

;; Returns the position of the start of the last line in MSG.
;; Works no matter what the restriction is set to.
(defun rm-message-end (msg)
  (pos 0 (+ (pos-line (mark-pos (rm-get-msg-field msg rm-msg-mark)))
	    (rm-get-msg-field msg rm-msg-total-lines) -1)))

;; Returns the position of the start of the body of MSG
;; Works no matter what the restriction is set to.
(defun rm-message-body (msg)
  (pos 0 (+ (pos-line (mark-pos (rm-get-msg-field msg rm-msg-mark)))
	    (rm-get-msg-field msg rm-msg-header-lines))))

;; Finds the first visible header in the current message
(defun rm-message-first-visible ()
  (let
      ((message (rm-get-folder-field (rm-current-folder)
				     rm-folder-current-msg)))
    (save-restriction
      (restrict-buffer (mark-pos (rm-get-msg-field message rm-msg-mark))
		       (rm-message-body message))
      (or (re-search-forward mail-visible-headers (start-of-buffer) nil t)
	  (restriction-start)))))

;; Updates the flags embedded in the message headers of BUFFER
(defun rm-update-flags (buffer)
  (with-buffer buffer
    (unless (eq rm-buffer-messages 'invalid)
      (save-restriction
	(let
	    ((inhibit-read-only t)
	     list msg start)
	  (mapc #'(lambda (msg)
		    (setq start (mark-pos (rm-get-msg-field msg rm-msg-mark)))
		    (unrestrict-buffer)
		    (when (re-search-forward "^$" start)
		      (restrict-buffer start (match-end)))
		    (let
			((lines-added 0)
			 (print-escape t)
			 tem)
		      ;; First put flags into X-Jade-Flags-v1 header
		      (if (re-search-forward
			   "^X-Jade-Flags-v1[\t ]*:[\t ]*(.*)$"
			   start nil t)
			  (progn
			    (setq tem (match-start 1))
			    (delete-area (match-start 1) (match-end 1)))
			(setq tem (forward-char -1 (insert
						    "X-Jade-Flags-v1: \n"
						    (mail-unfold-header
						     start)))
			      lines-added (1+ lines-added)))
		      (prin1 (rm-get-msg-field msg rm-msg-flags)
			     (cons buffer tem))
		      ;; Then selected cache items into X-Jade-Cache-v1 header
		      (if (re-search-forward
			   "^X-Jade-Cache-v1[\t ]*:[\t ]*(.*)$"
			   start nil t)
			  (progn
			    (setq tem (match-start 1))
			    (delete-area (match-start 1) (match-end 1)))
			(setq tem (forward-char -1 (insert
						    "X-Jade-Cache-v1: \n"
						    (mail-unfold-header
						     start)))
			      lines-added (1+ lines-added)))
		      (prin1 (delete-if
			      #'(lambda (x)
				  (null (memq (car x) rm-saved-cache-tags)))
			      (copy-sequence
			       (rm-get-msg-field msg rm-msg-cache)))
			     (cons (current-buffer) tem))
		      ;; Adjust total-lines and header-lines message attrs
		      (rm-set-msg-field
		       msg rm-msg-header-lines
		       (+ (rm-get-msg-field msg rm-msg-header-lines)
			  lines-added))
		      (rm-set-msg-field
		       msg rm-msg-total-lines
		       (+ (rm-get-msg-field msg rm-msg-total-lines)
			  lines-added))))
		rm-buffer-messages))))))

;; Call (mail-get-header HEADER LISTP NO-COMMA-SEP), with the current
;; restriction set to the headers of MSG
(defun rm-get-msg-header (msg header &optional listp no-comma-sep)
  (with-buffer (mark-file (rm-get-msg-field msg rm-msg-mark))
    (save-restriction
      (restrict-buffer (mark-pos (rm-get-msg-field msg rm-msg-mark))
		       (pos 0 (+ (rm-get-msg-field msg rm-msg-header-lines)
				 (pos-line (mark-pos (rm-get-msg-field
						      msg rm-msg-mark))))))
      (mail-get-header header listp no-comma-sep))))

;; Shortcuts to some common headers
(defun rm-get-senders (msg)
  (rm-cached-form msg 'sender-list
    (mapcar 'mail-parse-address (rm-get-msg-header msg "(From|Sender)" t))))

(defun rm-get-recipients (msg)
  (rm-cached-form msg 'recipient-list
    (mapcar 'mail-parse-address (rm-get-msg-header msg "(To|Cc|Bcc)" t))))

(defun rm-get-subject (msg)
  (rm-cached-form msg 'subject (rm-get-msg-header msg "Subject")))

;; subject with any re:'s stripped
(defun rm-get-actual-subject (msg)
  (rm-cached-form msg 'raw-subject
    (mail-get-actual-subject (rm-get-subject msg))))

(defun rm-get-date-vector (msg)
  (rm-cached-form msg 'date-vector
    (let
	((string (rm-get-msg-header msg "Date")))
      (if string
	  (mail-parse-date string)
	nil))))

(defun rm-get-message-id (msg)
  (rm-cached-form msg 'message-id
    ;; Read as a list; this is the easiest way to lose trailing spaces
    (car (rm-get-msg-header msg "Message-Id" t t))))

(defun rm-get-in-reply-to (msg)
  (rm-cached-form msg 'in-reply-to
    ;; Read as a list; this is the easiest way to lose trailing spaces
    (car (rm-get-msg-header msg "In-Reply-To" t t))))

(defun rm-get-references (msg)
  (rm-cached-form msg 'references
    (rm-get-msg-header msg "References" t t)))

;; Map FUNCTION over all messages in FOLDER
(defun rm-map-messages (function folder)
    (mapc #'(lambda (msg-list)
	      (mapc function msg-list))
	  (list (rm-get-folder-field folder rm-folder-before-list)
		(and (rm-get-folder-field folder rm-folder-current-msg)
		     (list (rm-get-folder-field folder rm-folder-current-msg)))
		(rm-get-folder-field folder rm-folder-after-list))))


;; Displaying messages

;; Display the current message in FOLDER
(defun rm-display-current-message (folder &optional no-summary-update)
  (let
      ((current (rm-get-folder-field folder rm-folder-current-msg))
       mark)
    (when current
      (setq mark (rm-get-msg-field current rm-msg-mark))
      (goto-buffer (mark-file mark))
      (unrestrict-buffer)
      (let
	  ((header-start (mark-pos mark))
	   visible-start)
	(unless (looking-at mail-message-start header-start)
	  (error "Position isn't start of header: %s" header-start))
	(let
	    ((end-of-hdrs (rm-message-body current))
	     (inhibit-read-only t))
	  ;; Just operate on the headers
	  (restrict-buffer header-start end-of-hdrs)
	  ;; First of all, move all visible headers after non-visible ones
	  (setq visible-start (rm-coalesce-visible-headers))
	  ;; Look for a header to highlight
	  (when (re-search-forward mail-highlighted-headers header-start nil t)
	    (mark-block (match-start 1)
			(forward-char -1 (or (mail-unfold-header
					      (match-start 1))
					     (end-of-buffer)))))
	  (unrestrict-buffer)
	  (goto end-of-hdrs)
	  (restrict-buffer visible-start (rm-message-end current))
	  (rm-clear-flag current 'unread)
	  (rm-fix-status-info current)
	  ;; Called when the current restriction is about to be
	  ;; displayed
	  (call-hook 'rm-display-message-hook (list current folder)))))
    (unless (or (not (rm-get-folder-field folder rm-folder-summary))
		no-summary-update)
      ;; Fix the summary buffer if it exists
      (rm-with-summary folder
       (rm-summary-update-current)))))

;; Set the minor-mode-names list to reflect the status of MESSAGE
(defun rm-fix-status-info (message)
  (setq buffer-status-id (rm-cached-form message 'status-id
			   (let
			       ((arg-list (cons message nil))
				(format-hooks-alist rm-format-alist))
			     (rplacd arg-list arg-list)
			     (apply 'format nil rm-status-format arg-list))))
  (setq minor-mode-names (mapcar 'symbol-name
				 (rm-get-msg-field message rm-msg-flags))))

(defun rm-invalidate-status-cache (folder)
  (rm-map-messages #'(lambda (m)
		       (rm-invalidate-tag m 'status-id)) folder))

;; Display an arbitrary MSG in FOLDER
(defun rm-display-message (folder msg)
  (rm-make-message-current folder msg)
  (rm-display-current-message folder))

;; Ensure that all headers matching mail-visible-headers are at the end
;; of the header section so that the non-visible headers can be left out
;; of the display restriction. The buffer should currently be restricted
;; to the header section of the message. Returns the position of the first
;; visible header.
(defun rm-coalesce-visible-headers ()
  (let*
      ((first-visible (re-search-forward mail-visible-headers
					(start-of-buffer) nil t))
       (current-hdr first-visible)
       (next-hdr first-visible))
    (while (and current-hdr (< current-hdr (end-of-buffer)))
      ;; Move over all visible headers from CURRENT-HDR until
      ;; reaching an invisible one
      (setq next-hdr current-hdr)
      (while (and next-hdr (< next-hdr (end-of-buffer))
		  (looking-at mail-visible-headers next-hdr nil t))
	(setq next-hdr (mail-unfold-header next-hdr)))
      ;; Now we have a block of visible headers from CURRENT-HDR to
      ;; NEXT-HDR (but not including NEXT-HDR). Next find the following
      ;; visible header to delimit the block of invisible ones
      (setq current-hdr next-hdr)
      (while (and next-hdr (< next-hdr (end-of-buffer))
		  (not (looking-at mail-visible-headers next-hdr nil t)))
	(setq next-hdr (mail-unfold-header next-hdr)))
      ;; Now we have a block of invisible headers, CURRENT-HDR to NEXT-HDR
      ;; Move them to before the FIRST-VISIBLE-HDR, updating this to
      ;; point to the end of the insertion
      (unless next-hdr
	(setq next-hdr (end-of-buffer)))
      (when (and current-hdr
	       (> current-hdr first-visible)
	       (> next-hdr current-hdr))
	(setq first-visible (insert (cut-area current-hdr next-hdr)
				    first-visible))
	(if (not (zerop (pos-col first-visible)))
	    ;; Last insertion didn't add a trailing newline. This
	    ;; means that the end of the headers has been reached!
	    (setq first-visible (insert "\n" first-visible)
		  current-hdr nil)
	  (setq current-hdr next-hdr))))
    first-visible))


;; Deleting messages

;; Delete the current message. Unless GO-BACKWARDS-P is t the next
;; message is displayed (unless there is no next message).
(defun rm-delete-message (message &optional go-backwards-p)
  ;; When this hook returns t the message isn't deleted.
  (unless (call-hook 'rm-vet-deletion-hook (list message) 'or)
    (with-buffer (mark-file (rm-get-msg-field message rm-msg-mark))
      (unrestrict-buffer)
      (let
	  ((inhibit-read-only t)
	   (start (mark-pos (rm-get-msg-field message rm-msg-mark)))
	   (end (rm-message-end message)))
	(if (not (equal end (end-of-buffer)))
	    ;; Now this points to the first character of the next message
	    (setq end (forward-line 1 end))
	  ;; If there's a message preceding this one, we want to delete the
	  ;; newline between them as well.
	  (unless (equal start (start-of-buffer))
	    (setq start (forward-line -1 start))))
	(delete-area start end)
	(unless (eq rm-buffer-messages 'invalid)
	  (setq rm-buffer-messages (delq message rm-buffer-messages)))
	;; Delete this message from any folders containing it
	(rm-map-msg-folders
	 #'(lambda (unused-message folder)
	     (rm-set-folder-field folder rm-folder-message-count
				  (1- (rm-get-folder-field
				       folder rm-folder-message-count)))
	     (rm-invalidate-status-cache folder)
	     (let*
		 ((after (rm-get-folder-field folder rm-folder-after-list))
		  (current (rm-get-folder-field folder rm-folder-current-msg))
		  (before (rm-get-folder-field folder rm-folder-before-list))
		  (index (rm-get-folder-field folder rm-folder-current-index)))
	       (cond
		((eq message current)
		 ;; Deleting the current message
		 (if (and before (or go-backwards-p (null after)))
		     (setq current (car before)
			   before (cdr before)
			   index (1- index))
		   (setq current (car after)
			 after (cdr after))))
		((memq message before)
		 ;; Deleting from before the current message
		 (setq before (delq message before)
		       index (1- index)))
		(t
		 ;; Deleting from after the current message
		 (setq after (delq message after))))
	       (rm-set-folder-field folder rm-folder-before-list before)
	       (rm-set-folder-field folder rm-folder-current-msg current)
	       (rm-set-folder-field folder rm-folder-after-list after)
	       (rm-set-folder-field folder rm-folder-current-index index))
	     (rm-set-folder-field folder rm-folder-cached-list 'invalid))
	 message)))))

;; Delete all messages in the list DEL-MSGS as efficiently as possible
;; rm-display-current-msg should be called after this has returned
(defun rm-delete-messages (del-msgs)
  (mapc 'rm-delete-message del-msgs))


;; Getting mail from inbox

;; Insert the contents of file INBOX at the end of the current buffer,
;; if the buffer's rm-buffer-messages variable was set, then it's updated
;; with the new messages.
;; Returns the number of messages read if it's okay to try and read more
;; inboxes, nil if it's best not to.
;; Doesn't rebuild the folder(s) message list
(defun rm-append-inbox (inbox)
  (cond
   ((not (file-exists-p inbox))
    (error "Inbox file doesn't exist" inbox))
   ((zerop (file-size inbox))
    (message (concat "No new mail in " inbox))
    nil)
   ((or (null movemail-program)
	(not (file-exists-p movemail-program)))
    (error "The variable `movemail-program' is invalid"))
   (t
    (let*
	((tofile (local-file-name (concat ".newmail-"
					  (file-name-nondirectory inbox))))
	 (temp-buffer (make-buffer "*movemail-output*"))
	 (proc (make-process temp-buffer)))
      (if (zerop (call-process proc nil movemail-program
			       (local-file-name inbox) tofile))
	  ;; No errors
	  (progn
	    (unrestrict-buffer)
	    (let
		((inhibit-read-only t)
		 (keep-going t)
		 (count 0)
		 start pos msgs)
	      ;; Ensure that there's a blank line at the end of the buffer..
	      (goto (end-of-buffer))
	      (unless (zerop (1- (buffer-length)))
		;; Unless this is going to be the first message
		(insert "\n"))
	      (setq start (cursor-pos))
	      (insert-file-contents tofile)
	      (condition-case nil
		  (progn
		    ;; Try to save the folder..
		    (save-file)
		    ;; Don't delete the temporary file unless the folder
		    ;; was saved properly
		    (delete-file tofile))
		(error
		 ;; Make sure this error message is seen
		 (beep)
		 (message (concat "Couldn't save folder; new messages left in "
				  tofile) t)
		 (sleep-for 1)
		 (setq keep-going nil)))
	      (unless (eq rm-buffer-messages 'invalid)
		;; Parse all new messages. Work backwards
		(setq pos (end-of-buffer))
		(while (and pos (>= pos start)
			    (setq pos (re-search-backward mail-message-start
							  pos)))
		  (when (rm-message-start-p pos)
		    (setq msgs (cons (rm-parse-message pos) msgs)
			  count (1+ count))
		    (rm-set-flag (car msgs) 'unread))
		  (setq pos (forward-line -1 pos)))
		(setq rm-buffer-messages (nconc rm-buffer-messages msgs)))
	      (and keep-going count)))
	;; Errors
	(goto-buffer temp-buffer)
	(error "Couldn't move mail" inbox tofile))))))

;; Try and get new mail for the current folder. Returns the number of
;; messages actually read
(defun rm-get-mail ()
  (interactive)
  (let*
      ((folder (rm-current-folder))
       (old-message (rm-get-folder-field folder rm-folder-current-msg))
       (count 0))
    (mapc #'(lambda (box)
	      (let
		  ((inboxes (mail-find-inboxes box))
		   (this-ret 0)
		   this)
		(while (and inboxes (numberp this-ret))
		  (setq this (car inboxes)
			inboxes (cdr inboxes))
		  (unless (or (file-name-absolute-p this)
			      (file-exists-p this))
		    (setq this (expand-file-name this mail-folder-dir)))
		  (if (file-exists-p this)
		      (progn
			(setq this-ret (rm-append-inbox this))
			(when (and (numberp this-ret) (> this-ret 0))
			  (when (zerop count)
			    ;; If this is the first new message we want
			    ;; to display it afterwards
			    (setq old-message
				  (rm-get-folder-field folder
						       rm-folder-current-msg)))
			  (setq count (+ count this-ret))))
		    (format t "Spool file %s doesn't exist" this)))))
	  (rm-get-folder-field folder rm-folder-boxes))
    (when (> count 0)
      (mapc #'(lambda (cell)
		(let
		    ((folder (cdr cell)))
		  (rm-rebuild-folder folder)
		  (when (rm-get-folder-field folder rm-folder-summary)
		    (rm-with-summary folder
		      (summary-update)))))
	    rm-open-folders)
      (format t "Got %d new messages" count))
    (rm-display-current-message folder t)
    (call-hook 'rm-after-import-hook)
    count))


;; Macros for switching between the summary and mail views

;; When called from a folder buffer, will execute FORMS in the summary
;; buffer. If a view of the summary exists will be in that. Note that
;; FORMS should be as small as poss. since it's expanded twice.
(defmacro rm-with-summary (folder &rest forms)
  `(let
       ((view (get-buffer-view (rm-get-folder-field
				,folder rm-folder-summary))))
     (if view
	 (with-view view
	   ,@forms)
       (with-buffer (rm-get-folder-field ,folder rm-folder-summary)
	 ,@forms))))


;; Add standard format conversions; they're appended in case the user
;; added some of their own. The progn is to ensure compilation; this is
;; also why the alist is built dynamically
(progn
  (setq rm-format-alist
	(nconc
	 rm-format-alist
	 (list
	  (cons ?a #'(lambda (m)
		       (concat (cond
				((rm-test-flag m 'unread) ?U)
				((rm-test-flag m 'deleted) ?D)
				(t ? ))
			       (cond
				((rm-test-flag m 'replied) ?R)
				((rm-test-flag m 'forwarded) ?Z)
				(t ? ))
			       (if (rm-test-flag m 'filed) ?F ?\ ))))
	  (cons ?b #'(lambda (m)
		       (buffer-name (mark-file
				     (rm-get-msg-field m rm-msg-mark)))))
	  (cons ?D #'(lambda (m)
		       (let
			   ((date (rm-get-date-vector m)))
			 (when date
			   (format nil "%02d" (aref date mail-date-day) "")))))
	  (cons ?w #'(lambda (m)
		       (let
			   ((date (rm-get-date-vector m)))
			 (when date
			   (aref date mail-date-day-abbrev)))))
	  (cons ?f #'(lambda (m)
		       (car (car (rm-get-senders m)))))
	  (cons ?F #'(lambda (m)
		       (let
			   ((from (car (rm-get-senders m))))
			 (or (cdr from) (car from)))))
	  (cons ?M #'(lambda (m)
		       (let
			   ((date (rm-get-date-vector m)))
			 (when date
			   (format nil "%02d" (aref date mail-date-month))))))
	  (cons ?m #'(lambda (m)
		       (let
			   ((date (rm-get-date-vector m)))
			 (when date
			   (aref date mail-date-month-abbrev)))))
	  (cons ?n #'(lambda (m)
		       (let
			   ((folder (rm-current-folder)))
			 (with-buffer (mark-file
				       (rm-get-msg-field m rm-msg-mark))
			   (1+ (rm-get-folder-field
				folder rm-folder-current-index))))))
	  (cons ?N #'(lambda (m)
		       (let
			   ((folder (rm-current-folder)))
			 (with-buffer (mark-file
				       (rm-get-msg-field m rm-msg-mark))
			   (rm-get-folder-field
			    folder rm-folder-message-count)))))
	  (cons ?l #'(lambda (m)
		       (rm-get-subject m)))
	  (cons ?t #'(lambda (m)
		       (let
			   ((date (rm-get-date-vector m)))
			 (when date
			   (format nil "%02d:%02d"
				   (aref date mail-date-hour)
				   (aref date mail-date-minute))))))
	  (cons ?T #'(lambda (m)
		       (let
			   ((date (rm-get-date-vector m)))
			 (when date
			   (format nil "%02d:%02d:%02d"
				   (aref date mail-date-hour)
				   (aref date mail-date-minute)
				   (aref date mail-date-second))))))
	  (cons ?r #'(lambda (m)
		       (let
			   ((to (car (rm-get-recipients m))))
			 (or (cdr to) (car to)))))
	  (cons ?Y #'(lambda (m)
		       (let
			   ((date (rm-get-date-vector m)))
			 (when date
			   (format nil "%d" (aref date mail-date-year))))))
	  (cons ?z #'(lambda (m)
		       (let
			   ((date (rm-get-date-vector m)))
			 (when date
			   (aref date mail-date-timezone)))))))))
			 

;; Commands, these must only be called from the folder buffer, *not*
;; from the summary.

(defun rm-next-message (&optional count skip-deleted)
  "Display the next message in the current mail folder."
  (interactive "p")
  (unless count
    (setq count 1))
  (let*
      ((folder (rm-current-folder))
       (original-message (rm-get-folder-field folder rm-folder-current-msg)))
    (while (> count 0)
      (unless (rm-get-folder-field folder rm-folder-after-list)
	(when original-message
	  (rm-make-message-current folder original-message))
	(error "No more messages"))
      (rm-move-forwards folder)
      (when (or (not skip-deleted)
		(not (rm-test-flag (rm-get-folder-field
				    folder rm-folder-current-msg) 'deleted)))
	(setq count (1- count))))
    (while (< count 0)
      (unless (rm-get-folder-field folder rm-folder-before-list)
	(when original-message
	  (rm-make-message-current folder original-message))
	(error "No previous message"))
      (rm-move-backwards folder)
      (when (or (not skip-deleted)
		(not (rm-test-flag (rm-get-folder-field
				    folder rm-folder-current-msg) 'deleted)))
	(setq count (1+ count))))
    (rm-display-current-message folder)))

(defun rm-previous-message (&optional count skip-deleted)
  "Display the previous message in the current mail folder."
  (interactive "p")
  (rm-next-message (- (or count 1)) skip-deleted))

(defun rm-next-undeleted-message (&optional count)
  "Display the next undeleted message."
  (interactive "p")
  (rm-next-message count t))

(defun rm-previous-undeleted-message (&optional count)
  "Display the previous undeleted message."
  (interactive "p")
  (rm-previous-message count t))

(defun rm-next-page ()
  "Display the next page in the current message."
  (interactive)
  (let
      ((char (display-to-char-pos (pos 0 (1- (cdr (view-dimensions)))))))
    (if (or (not char) (>= char (end-of-buffer)))
	(when rm-auto-next-message
	  (rm-next-undeleted-message))
      (next-screen))))

(defun rm-previous-page ()
  "Display the previous page in the current message."
  (interactive)
  (if (<= (display-to-char-pos (pos 0 0)) (start-of-buffer))
      (when rm-auto-next-message
	(rm-previous-message)
	(goto (end-of-buffer)))
    (prev-screen)))

(defun rm-toggle-all-headers ()
  "Toggles between showing invisible headers and not showing them for the
current message."
  (interactive)
  (let*
      ((folder (rm-current-folder))
       (visible-start (rm-message-first-visible))
       (current (rm-get-folder-field folder rm-folder-current-msg)))
    (restrict-buffer (if (equal (restriction-start) visible-start)
			 (mark-pos (rm-get-msg-field current rm-msg-mark))
		       visible-start)
		     (rm-message-end current))
    (goto (start-of-buffer))))

(defun rm-mark-message-deletion ()
  "Marks that the current message should be deleted."
  (interactive)
  (let*
      ((folder (rm-current-folder))
       (current (rm-get-folder-field folder rm-folder-current-msg)))
    (rm-set-flag current 'deleted)
    (rm-fix-status-info current)
    (when (and rm-move-after-deleting
	       (rm-get-folder-field folder rm-folder-after-list))
      (rm-next-message))))

(defun rm-expunge ()
  "Actually delete all messages that have been marked as unwanted. This is
nonrecoverable."
  (interactive)
  (let
      ((folder (rm-current-folder))
       (deletions nil))
    (rm-map-messages #'(lambda (m)
			 (when (rm-test-flag m 'deleted)
			   (setq deletions (cons m deletions)))) folder)
    (rm-delete-messages deletions)
    (rm-display-current-message folder t)
    (when (rm-get-folder-field folder rm-folder-summary)
      (rm-with-summary folder
	(summary-update)))))

(defun rm-unmark-message ()
  "Unmarks the current message."
  (interactive)
  (let*
      ((folder (rm-current-folder))
       (current (rm-get-folder-field folder rm-folder-current-msg)))
    (rm-clear-flag current 'deleted)
    (rm-fix-status-info current)
    (when (and rm-move-after-deleting
	       (rm-get-folder-field folder rm-folder-after-list))
      (rm-next-message))))

(defun rm-kill-subject ()
  "Marks all messages with the same subject as the current message as being
ready for deletion."
  (interactive)
  (let*
      ((folder (rm-current-folder))
       (current (rm-get-folder-field folder rm-folder-current-msg))
       (kill-subject (rm-get-actual-subject current)))
    (rm-map-messages #'(lambda (m)
			 (when (string= kill-subject (rm-get-actual-subject m))
			   (rm-set-flag m 'deleted))) folder)))

(defun rm-pipe-message (command &optional ignore-headers)
  "Pipes all of the current message through the shell command COMMAND (unless
IGNORE-HEADERS is non-nil, in which case only the body of the message is
used). All output is left in the `*shell output*' buffer. When called
interactively, COMMAND is prompted for, and IGNORE-HEADERS takes its value
from the prefix argument."
  (interactive "sShell command on message:\nP")
  (save-restriction
    (let*
	((folder (rm-current-folder))
	 (current (rm-get-folder-field folder rm-folder-current-msg))
	 (start (if ignore-headers
		    (rm-message-body current)
		  (mark-pos (rm-get-msg-field current rm-msg-mark)))))
      (shell-command-on-area command start (rm-message-end current)))))

(defun rm-save-and-quit ()
  "Quit from the mail reading subsystem. The current folder will be saved
automatically."
  (interactive)
  (rm-quit-no-save t))

(defun rm-quit-no-save (&optional really-save)
  "Quit from the mail reading subsystem without saving the current folder. The
buffer will not be deleted, so it may be saved later."
  (interactive)
  (let
      ((folder (rm-current-folder)))
    (when (rm-get-folder-field folder rm-folder-summary)
      (rm-kill-summary folder))
    (mapc #'(lambda (box)
	      (rm-close-mailbox box (not really-save)))
	  (rm-get-folder-field folder rm-folder-boxes))
    (setq rm-open-folders (delete-if #'(lambda (cell)
					 (eq (cdr cell) folder))
				     rm-open-folders))))
