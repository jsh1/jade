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
text in the mail buffer's status line. See the `rm-format-alist' variable
for the format conversions available.")

(defvar rm-format-alist nil
  "An alist of (CHARACTER . FUNCTION) defining formatting directives for the
rm-summary-format-string. The function is called as: (FUNCTION MESSAGE-
STRUCTURE); it should return the string to be inserted (no newlines).

Each format directive consists of a percent character, an optional
numeric argument, and a single character specifying what should be
inserted in place of the format directive. These characters may include
by default:

	a	A 3-character attribute string, showing the status of
		the message
	A	A 5-character attribute string
	*	A `*' character if the message is marked
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
	z	The timezone, as a string")

(defvar rm-saved-cache-tags '(sender-list recipient-list date-vector)
  "List of cache tags whose values should be saved in the headers of each
message (to improve performance when the folder is next loaded).")

(defvar rm-saved-flags '(deleted filed forwarded replied unread)
  "Flags that will be saved in messages.")

(defvar rm-status-alist '((deleted . " Deleted")
			  (replied . " Replied")
			  (forwarded . " Forwarded")
			  (filed . " Filed"))
  "Map message flags to status line strings.")

(defvar rm-auto-rule-alist nil
  "List of (REGEXP . RULE). This list is only consulted when a mailbox is
added to an otherwise empty folder. If the name of the mailbox matches
REGEXP, then its restriction rule is initialised as RULE.")

(defvar rm-auto-sort-key-alist nil
  "List of (REGEXP . KEY). This list is only consulted when a mailbox is
added to an otherwise empty folder. If the name of the mailbox matches
REGEXP, then its sort key is initialised as KEY.")

(defvar rm-after-import-rules nil
  "A list of message restriction rules. Called for all messages appended
to a mailbox file. The rules are expected to have side-effects, the value
of each rule is ignored.")

(defvar rm-after-parse-rules nil
  "A list of message restriction rules. Called for all messages read from
a mailbox file. The rules are expected to have side-effects, the value
of each rule is ignored.")


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
    "U" 'rm-unmark-all-messages
    "r" 'rm-reply
    "R" '(rm-reply t)
    "f" 'rm-followup
    "F" '(rm-followup t)
    "z" 'rm-forward
    "*" 'rm-burst-message
    "s" 'rm-output
    "a" 'rm-archive-folder
    "A" 'rm-auto-archive-folder
    "|" 'rm-pipe-message
    "+" 'rm-add-mailbox
    "-" 'rm-subtract-mailbox
    "=" 'rm-replace-all-mailboxes
    "Ctrl--" 'rm-subtract-all-mailboxes
    "!" 'rm-change-rule
    "@" 'rm-null-rule
    "Ctrl-t" 'rm-toggle-threading
    "G" 'rm-sort-folder)
  "Keymap for reading mail")
  
(defvar rm-last-mailbox mail-folder-dir
  "File name of the most recently opened folder. Used as a default value for
the next prompt.")

(defvar rm-buffer-messages 'invalid
  "A buffer-local variable, when not the symbol `invalid', the list of
messages in the buffer, in first to last order.")
(make-variable-buffer-local 'rm-buffer-messages)

(defvar rm-buffer-read-only nil
  "When t, this mailbox buffer won't be modified.")
(make-variable-buffer-local 'rm-buffer-read-only)


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
    ;; Clean up the rm-open-folders list in case it contains
    ;; folders that are no longer viewable
    (setq rm-open-folders (delete-if #'(lambda (cell)
					 (when (not (viewp (car cell)))
					   ;; Dead folder
					   (mapc #'(lambda (box)
						     (rm-close-mailbox box))
						 (rm-get-folder-field
						  (cdr cell) rm-folder-boxes))
					   t))
				     rm-open-folders))
    ;; Add the new folder
    (setq rm-open-folders (cons (cons (current-view) folder) rm-open-folders))
    (mapc #'(lambda (box)
	      (unless (file-exists-p box)
		(setq box (expand-file-name box mail-folder-dir)))
	      (rm-add-mailbox box folder t))
	  (if (listp boxes) boxes (list boxes)))
    (rm-display-current-message folder t)
    (when mail-display-summary
      (rm-summarize folder))))


;; Mailbox buffer handling

;; Ensure that the mailbox named MAILBOX is in a buffer. Return that buffer
(defun rm-open-mailbox (mailbox)
  (let
      ((buffer (let
		   ;; This stops a possible security risk. Maybe
		   ;; I should just disable enable-local-eval?
		   ((enable-local-variables nil))
		 (find-file-read-only mailbox t))))
    (unless buffer
      (error "Can't open mailbox %s" mailbox))
    (with-buffer buffer
      (read-mail-mode))
    buffer))

;; The mailbox named MAILBOX is not required any longer. If DONT-UPDATE
;; is t, the cached information won't be updated and the buffer won't
;; be saved
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

;; Install the major-mode of mailbox buffers
(defun read-mail-mode ()
  "Read-Mail Mode:\n
Major mode for viewing mail folders. Local bindings are:\n
\\{rm-keymap}"
  (unless (eq major-mode 'read-mail-mode)
    (when major-mode-kill
      (funcall major-mode-kill (current-buffer)))
    (setq rm-open-mailboxes (cons (current-buffer) rm-open-mailboxes)
	  major-mode 'read-mail-mode
	  major-mode-kill 'read-mail-mode-kill
	  keymap-path (cons 'rm-keymap keymap-path))
    (call-hook 'read-mail-mode-hook)))

;; Remove the major-mode of mailbox buffers
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
  "The name of the sort key, nil by default. If a cons cell, the cdr is the
key, the car the order to sort in, a positive or negative integer.")

(defconst rm-folder-struct-size 12)


(defmacro rm-set-folder-field (folder field value)
  (list 'aset folder field value))

(defmacro rm-get-folder-field (folder field)
  (list 'aref folder field))

;; Create a new unused folder object
(defun rm-make-folder (&optional rule name)
  (let
      ((folder (make-vector rm-folder-struct-size)))
    (rm-set-folder-field folder rm-folder-type 'folder)
    (rm-set-folder-field folder rm-folder-rule rule)
    (rm-set-folder-field folder rm-folder-name name)
    (rm-set-folder-field folder rm-folder-message-count 0)
    (rm-set-folder-field folder rm-folder-cached-list 'invalid)
    folder))

;; Return t if OBJECT is a folder
(defun rm-folder-p (object)
  (and (vectorp object) (eq (aref object rm-folder-type) 'folder)))

(defun rm-rebuild-folder (folder)
  (rm-install-messages
   folder (rm-prune-messages
	   (rm-parse-mailboxes
	    (rm-get-folder-field folder rm-folder-boxes)) folder)))
    
;; Install the list of messages ALL, preserving the current message, and
;; splitting ALL around it. Do this destructively
(defun rm-install-messages (folder all)
  (let
      ((before nil)
       (after all)
       (current (rm-get-folder-field folder rm-folder-current-msg))
       (index 0))
    (rm-set-folder-field folder rm-folder-message-count (length all))
    (rm-set-folder-field folder rm-folder-cached-list 'invalid)
    (if (or (null current) (not (memq current all)))
	(setq before (nreverse all)
	      current (car before)
	      before (cdr before)
	      after nil
	      index (length before))
      (while (not (eq (car after) current))
	(setq after (prog1
			(cdr after)
		      (rplacd after before)
		      (setq before after))
	      index (1+ index))))
    (rm-set-folder-field folder rm-folder-current-msg current)
    (rm-set-folder-field folder rm-folder-before-list before)
    (rm-set-folder-field folder rm-folder-after-list (cdr after))
    (rm-set-folder-field folder rm-folder-current-index index)
    (when (rm-get-folder-field folder rm-folder-summary)
      (rm-invalidate-summary-cache folder))
    (rm-invalidate-status-cache folder)
    (when (rm-get-folder-field folder rm-folder-sort-key)
      (rm-sort-folder
       folder (rm-get-folder-field folder rm-folder-sort-key) nil t))))

;; Add MAILBOX to FOLDER. NO-REDISPLAY does at it suggests
(defun rm-add-mailbox (mailbox folder &optional no-redisplay)
  (interactive
   (list (prompt-for-folder "Mailbox to add:" rm-last-mailbox)
	 (rm-current-folder)))
  (unless mailbox
    (error "Null mailbox"))
  (unless (rm-get-folder-field folder rm-folder-current-msg)
    (let
	(cell)
      (setq cell (assoc-regexp mailbox rm-auto-rule-alist))
      (rm-set-folder-field folder rm-folder-rule (cdr cell))
      (setq cell (assoc-regexp mailbox rm-auto-sort-key-alist))
      (rm-set-folder-field folder rm-folder-sort-key (cdr cell))))
  (rm-set-folder-field folder rm-folder-boxes
		       (nconc (rm-get-folder-field folder rm-folder-boxes)
			      (list mailbox)))
  (rm-open-mailbox mailbox)
  (let
      ((new-messages (rm-get-mail-for-box mailbox)))
    (if (> new-messages 0)
	;; Display the first new message
	(while (> new-messages 1)
	  (rm-move-backwards folder)
	  (setq new-messages (1- new-messages)))
      (rm-rebuild-folder folder)))
  (unless no-redisplay
    (rm-redisplay-folder folder)))

;; Remove MAILBOX from FOLDER
(defun rm-subtract-mailbox (mailbox folder)
  (interactive
   (let
       ((folder (rm-current-folder)))
     (list (prompt-from-list (rm-get-folder-field folder rm-folder-boxes)
			     "Mailbox to subtract:")
	   folder)))
  (unless mailbox
    (error "Null mailbox"))
  (rm-set-folder-field folder rm-folder-boxes
		       (delete mailbox (rm-get-folder-field
					folder rm-folder-boxes)))
  (rm-rebuild-folder folder)
  (rm-close-mailbox mailbox)
  (rm-display-current-message folder t)
  (rm-redisplay-folder folder))

;; Remove all MAILBOXES from FOLDER
(defun rm-subtract-all-mailboxes (folder &optional no-redisplay)
  (interactive (list (rm-current-folder)))
  (mapc 'rm-close-mailbox (rm-get-folder-field folder rm-folder-boxes))
  (rm-set-folder-field folder rm-folder-boxes nil)
  (rm-rebuild-folder folder)
  (unless no-redisplay
    (rm-redisplay-folder folder)))

;; Install MAILBOX as the sole one in FOLDER
(defun rm-replace-all-mailboxes (folder mailbox)
  (interactive
   (list (rm-current-folder)
	 (prompt-for-folder "Mailbox to open:" rm-last-mailbox)))
  (unless mailbox
    (error "Null mailbox"))
  (rm-subtract-all-mailboxes folder)
  (rm-add-mailbox mailbox folder))

;; Install the restriction rule named RULE in FOLDER  
(defun rm-change-rule (folder rule)
  (interactive
   (list (rm-current-folder)
	 (rm-prompt-for-rule)))
  (rm-set-folder-field folder rm-folder-rule rule)
  (rm-rebuild-folder folder)
  (rm-redisplay-folder folder))

(defun rm-null-rule (folder)
  (interactive (list (rm-current-folder)))
  (rm-change-rule folder nil))

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

;; Return the number of folders containing the mailbox contained by BUFFER
(defun rm-folders-with-box (buffer)
  (apply '+ (mapcar #'(lambda (cell)
			(if (memq buffer (rm-get-folder-field
					  (cdr cell) rm-folder-boxes))
			    1
			  0)) rm-open-folders)))

;; Add MSGS to BUFFER. Rebuilding any folders referencing BUFFER
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

;; Assuming FOLDER is displayed in the current view, redisplay it and
;; completely redisplay any associated summary
(defun rm-redisplay-folder (folder)
  (rm-display-current-message folder t)
  (when (rm-get-folder-field folder rm-folder-summary)
    (rm-with-summary folder
      (summary-update))))


;; Internal message structure

;; FLAGS is a list of symbols, including replied, unread, filed,
;; forwarded, deleted, anything else?
(defconst rm-msg-mark 0)
(defconst rm-msg-total-lines 1)
(defconst rm-msg-header-lines 2)
(defconst rm-msg-plist 3)
(defconst rm-msg-cache 4)
(defconst rm-msg-struct-size 5)

(defmacro rm-set-msg-field (msg field value)
  (list 'aset msg field value))

(defmacro rm-get-msg-field (msg field)
  (list 'aref msg field))

;; Create an unused message structure
(defmacro rm-make-msg ()
  '(make-vector rm-msg-struct-size))

;; Set PROP in MSG to VALUE
(defun rm-message-put (msg prop value &optional undisplayed)
  (let
      ((cell (assq prop (rm-get-msg-field msg rm-msg-plist)))
       (modified t))
    (if cell
	(if (eq (cdr cell) value)
	    (setq modified nil)
	  (rplacd cell value))
      (rm-set-msg-field msg rm-msg-plist
			(cons (cons prop value)
			      (rm-get-msg-field msg rm-msg-plist))))
    (unless (or undisplayed (not modified))
      (rm-map-msg-folders #'(lambda (msg folder)
			      (when (rm-get-folder-field
				     folder rm-folder-summary)
				(rm-invalidate-summary msg)
				(rm-with-summary folder
				  (summary-update-item msg))))
			  msg))
    value))

;; Get PROP in MSG
(defmacro rm-message-get (msg prop)
  `(cdr (assq ,prop (rm-get-msg-field ,msg rm-msg-plist))))

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
  (mapcar 'rm-parse-mailbox boxes))

;; Parse a single mailbox named BOX. Return a _read_only_ list of messages
(defun rm-parse-mailbox (box)
  (let
      ((buffer (get-file-buffer box)))
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
      rm-buffer-messages)))

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
      (rm-set-msg-field msg rm-msg-plist
			(mapcar #'(lambda (f)
				    (cons f t))
				(read (cons (current-buffer) pos)))))
    (when (setq pos (mail-find-header "^X-Jade-Cache-v1" start))
      (rm-set-msg-field msg rm-msg-cache (read (cons (current-buffer) pos))))
    (when (mail-find-header "Replied" start)
      ;; MH annotates replied messages like this
      (rm-message-put msg 'replied t))
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
    (when rm-after-parse-rules
      (mapc #'(lambda (r)
		(rm-apply-rule r msg)) rm-after-parse-rules))
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
    (unless (or (eq rm-buffer-messages 'invalid)
		rm-buffer-read-only)
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
		      (let
			  ((flags
			    (mapcar 'car
				    (filter #'(lambda (cell)
						(and (cdr cell)
						     (memq (car cell)
							   rm-saved-flags)))
					    (rm-get-msg-field
					     msg rm-msg-plist)))))
			(catch 'flags
			  (if (re-search-forward
			       "^X-Jade-Flags-v1[\t ]*:[\t ]*(.*)$"
			       start nil t)
			      (progn
				(setq tem (match-start 1))
				(when (equal (read (cons (current-buffer) tem))
					     flags)
				  ;; don't bother
				  (throw 'flags nil))
				(delete-area (match-start 1) (match-end 1)))
			    (setq tem (forward-char -1 (insert
							"X-Jade-Flags-v1: \n"
							(mail-unfold-header
							 start)))
				  lines-added (1+ lines-added)))
			  (prin1 flags (cons buffer tem))))
		      ;; Then selected cache items into X-Jade-Cache-v1 header
		      (let
			  ((cache-items
			    (delete-if
			     #'(lambda (x)
				 (null (memq (car x) rm-saved-cache-tags)))
			     (copy-sequence
			      (rm-get-msg-field msg rm-msg-cache)))))
			(catch 'cache
			  (if (re-search-forward
			       "^X-Jade-Cache-v1[\t ]*:[\t ]*(.*)$"
			       start nil t)
			      (progn
				(setq tem (match-start 1))
				(when (equal cache-items
					     (read
					      (cons (current-buffer) tem)))
				  ;; don't bother
				  (throw 'cache nil))
				(delete-area (match-start 1) (match-end 1)))
			    (setq tem (forward-char -1 (insert
							"X-Jade-Cache-v1: \n"
							(mail-unfold-header
							 start)))
				  lines-added (1+ lines-added)))
			  (prin1 cache-items (cons (current-buffer) tem))))
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
    (if current
	(progn
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
		 tem)
	      ;; Just operate on the headers
	      (restrict-buffer header-start end-of-hdrs)
	      ;; First of all, move all visible headers after non-visible ones
	      (setq visible-start (rm-coalesce-visible-headers))
	      ;; Look for a header to highlight
	      (delete-all-extents)
	      (setq tem header-start)
	      (while (re-search-forward mail-highlighted-headers tem nil t)
		(when (looking-at "^[^:]+:[\t ]*" (match-start))
		  (let
		      ((end (match-end)))
		    (setq tem (forward-char -1 (or (mail-unfold-header
						    (match-end))
						   (end-of-buffer))))
		    (make-extent end tem (list 'face mail-highlight-face)))))
	      (unrestrict-buffer)
	      (goto end-of-hdrs)
	      (restrict-buffer visible-start (rm-message-end current))
	      (rm-message-put current 'unread nil)
	      (rm-fix-status-info current)
	      ;; Called when the current restriction is about to be
	      ;; displayed
	      (call-hook 'rm-display-message-hook (list current folder)))))
      ;; No message to display. Try for an empty buffer
      (let
	  ((box (car (rm-get-folder-field folder rm-folder-boxes))))
	(when box
	  (goto-buffer (get-file-buffer box)))))
    (unless (or (not (rm-get-folder-field folder rm-folder-summary))
		no-summary-update)
      ;; Fix the summary buffer if it exists
      (rm-with-summary folder
	(rm-summary-update-current)))))

;; Set the minor-mode-names list to reflect the status of MESSAGE
(defun rm-fix-status-info (message)
  (setq buffer-status-id (rm-cached-form message 'status-id
			   (rm-format rm-status-format message)))
  (setq mode-name (apply 'concat
			 (mapcar #'(lambda (cell)
				     (and (cdr cell)
					  (cdr (assq (car cell)
						     rm-status-alist))))
				 (rm-get-msg-field message rm-msg-plist)))))

;; Invalidate all cached status messages in FOLDER
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
       (next-hdr first-visible)
       (inhibit-read-only t))
    (if rm-buffer-read-only
	;; Don't try to reorder headers
	first-visible
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
      first-visible)))


;; Deleting messages

;; Delete the current message. Unless GO-BACKWARDS-P is t the next
;; message is displayed (unless there is no next message).
(defun rm-delete-message (message &optional go-backwards-p)
  ;; When this hook returns t the message isn't deleted.
  (unless (call-hook 'rm-vet-deletion-hook (list message) 'or)
    (with-buffer (mark-file (rm-get-msg-field message rm-msg-mark))
      (when rm-buffer-read-only
	(error "Read-only mailbox"))
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

;; Append FILE to the current buffer, attempt to save the current buffer
;; then delete FILE. If an error occurs whilst saving, don't delete anything,
;; and display a message noting that messages are left in FILE.
;; Returns t if everything went ok, nil otherwise.
(defun rm-append-save-and-delete-file (file)
  (unrestrict-buffer)
  (goto (end-of-buffer))
  (unless (zerop (1- (buffer-length)))
    ;; Unless this is going to be the first message
    (insert "\n"))
  (and (insert-file file)
       (condition-case nil
	   (progn
	     (save-file)
	     (delete-file file)
	     t)
	 (error
	  (beep)
	  (message (concat "Couldn't save mailbox; messages left in " file) t)
	  (sit-for 1)
	  nil))))

;; Insert the contents of file INBOX at the end of the current buffer,
;; if the buffer's rm-buffer-messages variable was set, then it's updated
;; with the new messages.
;; Returns the number of messages read if it's okay to try and read more
;; inboxes, nil if it's best not to.
;; Doesn't rebuild the folder(s) message list
(defun rm-append-inbox (inbox)
  (cond
   ((or (null movemail-program)
	(not (file-exists-p movemail-program)))
    (error "The variable `movemail-program' is invalid"))
   ((not (eq major-mode 'read-mail-mode))
    (error "Current buffer isn't a mailbox"))
   (rm-buffer-read-only
    (error "Read-only mailbox"))
   (t
    ;; Ensure the buffer is already parsed. We need this later
    (when (eq rm-buffer-messages 'invalid)
      (rm-parse-mailbox (buffer-file-name)))
    (let*
	((tofile (local-file-name (expand-file-name
				   (concat ".newmail-"
					   (file-name-nondirectory inbox))
				   (file-name-directory (buffer-file-name)))))
	 (temp-buffer (make-buffer "*movemail-output*"))
	 (proc (make-process temp-buffer))
	 (need-to-parse nil)
	 (keep-going t)
	 (start (end-of-buffer))
	 (count 0)
	 (inhibit-read-only t))
      ;; First look for messages left in the .newmail-FOO file from last time
      (when (and (file-exists-p tofile)
		 (> (file-size tofile) 0))
	(message (concat "Using messages left in " tofile) t)
	(setq keep-going (or (rm-append-save-and-delete-file tofile)
			     keep-going)
	      need-to-parse t))
      ;; Then if that didn't fail horrendously, look for mail in the INBOX
      (when keep-going
	(cond
	 ((not (file-exists-p inbox))
	  (error "Inbox file doesn't exist" inbox))
	 ((> (file-size inbox) 0)
	  (if (zerop (call-process proc nil movemail-program
				   (local-file-name inbox) tofile))
	      ;; Now the temporary file contains the new messages
	      (setq keep-going (or (rm-append-save-and-delete-file tofile)
				   keep-going)
		    need-to-parse t)
	    ;; Errors
	    (goto-buffer temp-buffer)
	    (error "Couldn't move mail: %s, %s" tofile inbox)))))
      ;; If necessary, parse all new messages. Work backwards
      (when need-to-parse
	(let
	    (pos msgs)
	  (setq pos (end-of-buffer))
	  (while (and pos
		      (setq pos (re-search-backward mail-message-start pos))
		      (>= pos start))
	    (when (rm-message-start-p pos)
	      (setq msgs (cons (rm-parse-message pos) msgs)
		    count (1+ count))
	      (rm-message-put (car msgs) 'unread t t)
	      (when rm-after-import-rules
		(mapc #'(lambda (r)
			  (rm-apply-rule r (car msgs)))
		      rm-after-import-rules)))
	    (setq pos (forward-line -1 pos)))
	  (setq rm-buffer-messages (nconc rm-buffer-messages msgs))))
      (and keep-going count)))))

;; Returns the number of messages read
(defun rm-get-mail-for-box (mailbox)
  (let
      ((buffer (get-file-buffer mailbox))
       (inboxes (mail-find-inboxes mailbox))
       (count 0)
       file
       (file-count 0))
    (with-buffer buffer
      (while (and inboxes (numberp file-count))
	(setq file (car inboxes)
	      inboxes (cdr inboxes))
	(unless (or (file-name-absolute-p file)
		    (file-exists-p file))
	  (setq file (expand-file-name file mail-folder-dir)))
	(if (file-exists-p file)
	    (progn
	      (setq file-count (rm-append-inbox file))
	      (when (numberp file-count)
		(setq count (+ count file-count))))
	  (format t "Spool file %s doesn't exist" file)))
      (call-hook 'rm-after-import-hook))
    (if (> count 0)
	;; Any folders referencing this mailbox get rebuilt.
	(mapc #'(lambda (cell)
		  (let
		      ((folder (cdr cell)))
		    (when (member mailbox (rm-get-folder-field
					   folder rm-folder-boxes))
		      (rm-rebuild-folder folder)
		      (when (rm-get-folder-field folder rm-folder-summary)
			(rm-with-summary folder
			  (summary-update))))))
	      rm-open-folders)
      (message (concat "No new messages for " mailbox) t))
    count))

;; Try and get new mail for the current folder. Returns the number of
;; messages actually read
(defun rm-get-mail ()
  (interactive)
  (let*
      ((folder (rm-current-folder))
       (old-message (rm-get-folder-field folder rm-folder-current-msg))
       (count 0)
       (boxes (rm-get-folder-field folder rm-folder-boxes)))
    (while boxes
      (setq count (+ count (rm-get-mail-for-box (car boxes)))
	    boxes (cdr boxes)))
    (if (> count 0)
	(progn
	  (message (format nil "Got %d new message%s"
			   count (if (> count 1) "s" "")))
	  (rm-display-current-message folder t))
      (message "No new messages"))
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
				((rm-message-get m 'deleted) ?D)
				((rm-message-get m 'unread) ?U)
				(t ? ))
			       (cond
				((rm-message-get m 'replied) ?R)
				((rm-message-get m 'forwarded) ?Z)
				(t ? ))
			       (if (rm-message-get m 'filed) ?F ?\ ))))
	  (cons ?A #'(lambda (m)
		       (concat (if (rm-message-get m 'unread) ?U ? )
			       (if (rm-message-get m 'deleted) ?D ? )
			       (if (rm-message-get m 'replied) ?R ? )
			       (if (rm-message-get m 'forwarded) ?Z ? )
			       (if (rm-message-get m 'filed) ?F ? ))))
	  (cons ?* #'(lambda (m)
		       (let
			   ((summary (rm-get-folder-field (rm-current-folder)
							  rm-folder-summary)))
			 (if (and summary (with-buffer summary
					    (summary-item-marked-p m)))
			     "*" " "))))
	  (cons ?b #'(lambda (m)
		       (buffer-name (mark-file
				     (rm-get-msg-field m rm-msg-mark)))))
	  (cons ?D #'(lambda (m)
		       (let
			   ((date (rm-get-date-vector m)))
			 (when date
			   (format nil "%d" (aref date mail-date-day) "")))))
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
			   (format nil "%d" (aref date mail-date-month))))))
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

;; Format the string FORMAT for MESSAGE
(defun rm-format (format message)
  (let
      ((arg-list (cons message nil))
       (format-hooks-alist rm-format-alist))
    (rplacd arg-list arg-list)
    (apply 'format nil format arg-list)))


;; Commands, these must only be called from the folder buffer, *not*
;; from the summary.

;; Return the list of messages that the current command should operate on
(defun rm-command-items (folder)
  (if (rm-get-folder-field folder rm-folder-summary)
      (rm-with-summary folder
        (summary-command-items))
    (list (rm-get-folder-field folder rm-folder-current-msg))))

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
		(not (rm-message-get (rm-get-folder-field
				      folder rm-folder-current-msg) 'deleted)))
	(setq count (1- count))))
    (while (< count 0)
      (unless (rm-get-folder-field folder rm-folder-before-list)
	(when original-message
	  (rm-make-message-current folder original-message))
	(error "No previous message"))
      (rm-move-backwards folder)
      (when (or (not skip-deleted)
		(not (rm-message-get (rm-get-folder-field
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
    (rm-message-put current 'deleted t)
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
			 (when (rm-message-get m 'deleted)
			   (setq deletions (cons m deletions)))) folder)
    (rm-delete-messages deletions)
    (rm-redisplay-folder folder)))

(defun rm-unmark-message ()
  "Unmarks the current message."
  (interactive)
  (let*
      ((folder (rm-current-folder))
       (current (rm-get-folder-field folder rm-folder-current-msg)))
    (rm-message-put current 'deleted nil)
    (when (rm-get-folder-field folder rm-folder-summary)
      (rm-with-summary folder
        (summary-unmark-item current)))
    (rm-fix-status-info current)))

(defun rm-unmark-all-messages ()
  "Unmarks all messages in the current folder."
  (interactive)
  (let
      ((folder (rm-current-folder)))
    (rm-map-messages #'(lambda (m)
			 (rm-message-put m 'deleted nil)) folder)
    (when (rm-get-folder-field folder rm-folder-summary)
      (rm-with-summary folder
        (summary-unmark-all)))))


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
			   (rm-message-put m 'deleted t))) folder)))

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
