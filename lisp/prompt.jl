;;;; prompt.jl -- Prompt in a buffer with completion
;;;  Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
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

(require 'ring)
(provide 'prompt)

(defvar prompt-keymap (make-keylist))

(defvar prompt-buffer-list '()
  "Stack of buffers which can be used for prompts.")

(defvar completion-buffer-list '()
  "Stack of buffers used to display completion results.")

(bind-keys prompt-keymap
  "TAB"		'prompt-complete
  "RET"		'prompt-enter-line
  "LMB-CLICK2"	'prompt-select-completion
  "RMB-CLICK1"	'prompt-complete
  "Meta-?"	'prompt-list-completions
  "Meta-/"	'prompt-list-completions
  "Meta-n"	'prompt-next-history
  "Meta-p"	'prompt-previous-history
  "Ctrl-g"	'prompt-cancel)


;; Configuration variables

(defvar prompt-completion-function nil
  "Optional function taking one argument, the string to be completed. It
should return a list of all matches.")

(defvar prompt-validate-function nil
  "Optional function taking one argument, the string which has been entered.
Should return non-nil when this string may be accepted (and therefore the
prompt will end). If it returns the symbol t the string is returned as-is,
if some other non-nil value is returned *that* is the value returned by
the prompt.")

(defconst prompt-def-regexps ["[a-zA-Z0-9]" "[^a-zA-Z0-9]|$"]
  "Default value of `prompt-word-regexps'")

(defvar prompt-word-regexps prompt-def-regexps
  "Vector of two regexps; the values of `word-regexp' and `word-not-regexp'
for the prompt.")

(defvar prompt-list nil
  "Used by the `prompt-complete-from-list' and `prompt-validate-from-list'
to supply possible completions.")

(defvar prompt-list-fold-case nil
  "When t `prompt-complete-from-list' and `prompt-validate-from-list' ignore
case.")

(defvar prompt-symbol-predicate nil
  "Predicate used when prompting for symbols.")

(defvar amiga-use-file-req-p t
  "*AMIGA ONLY*
When non-nil the normal ASL file requester is used when file names are
prompted for.")

(defvar prompt-default-history (make-ring)
  "Catch-all history list for prompt.")

(defvar prompt-file-history (make-ring)
  "File history list for prompt.")

(defvar prompt-symbol-history (make-ring)
  "Symbol history list for prompt.")

(defvar prompt-history prompt-default-history
  "A ring buffer used to record history information for the minibuffer.")


;; Working variables used by more than one function

(defvar prompt-buffer nil
  "The buffer being used for the prompt.")

(defvar prompt-completion-buffer nil
  "The buffer being used to display completions.")

(defvar prompt-new-completion-view nil
  "t if a new view was created to display the completion buffer.")

(defvar prompt-completion-view nil
  "View used to display completion buffer.")

(defvar prompt-title-view nil
  "View whose status line is co-opted into displaying the prompt's title.")

(defvar prompt-old-title-msg nil
  "Original title.")

(defvar prompt-title nil
  "Title string of the prompt.")

(defvar prompt-reset-title nil
  "t when the title string needs to be reinstalled.")

(defvar prompt-original-buffer nil
  "The active buffer when the prompt was called.")

(defvar prompt-original-view nil
  "The active view when the prompt was called.")

(defvar prompt-history-index nil
  "The index of the current history item being looked at.")

(defvar prompt-history-top nil
  "The string at the `top' of the history list; that is the one being
entered currently.")

(defvar prompt-default-value nil
  "When non-nil the default value (in the history at position -1).")


;; Main entrypoint

;;;###autoload
(defun prompt (&optional prompt-title start)
  "Prompts for a string using completion. PROMPT-TITLE is the optional
title to print in the buffer, START the original contents of the buffer.
The string entered is returned, or nil if the prompt is cancelled (by Ctrl-g)."
  (let*
      ((prompt-buffer (get-prompt-buffer))
       prompt-completion-buffer
       prompt-new-completion-view
       prompt-completion-view
       (prompt-title-view (nth (- (window-view-count) 2) (window-view-list)))
       prompt-old-title-msg
       (prompt-original-buffer (current-buffer))
       (prompt-original-view (current-view))
       (prompt-history-index 0)
       (prompt-history-top nil)
       result)
    (set-buffer-special prompt-buffer t)
    (unless (stringp prompt-title)
      (setq prompt-title "Enter string:"))
    (setq prompt-old-title-msg (set-status-message prompt-title
						   prompt-title-view))
    (unwind-protect
	(with-view (minibuffer-view)
	  (with-buffer prompt-buffer
	    (setq word-regexp (aref prompt-word-regexps 0) 
		  word-not-regexp (aref prompt-word-regexps 1))
	    (when (stringp start)
	      (insert start))
	    (make-local-variable 'pre-command-hook)
	    (setq keymap-path '(prompt-keymap global-keymap)
		  buffer-undo-list nil
		  result (catch 'prompt (recursive-edit)))
	    (when (and result prompt-history
		       (not (string= result ""))
		       (or (zerop (ring-size prompt-history))
			   (not (equal (get-from-ring prompt-history)
				       result))))
	      (add-to-ring prompt-history result))))
      (return-prompt-buffer prompt-buffer)
      (prompt-remove-completion-buffer)
      (set-status-message prompt-old-title-msg prompt-title-view))
    result))


;; Subroutines

(defun get-prompt-buffer ()
  (if prompt-buffer-list
      (prog1
	  (car prompt-buffer-list)
	(setq prompt-buffer-list (cdr prompt-buffer-list)))
    (make-buffer "*prompt*")))

(defun return-prompt-buffer (buf)
  (setq prompt-buffer-list (cons buf prompt-buffer-list))
  (clear-buffer buf))

(defun prompt-enter-line ()
  (interactive)
  (let
      ((line (copy-area (buffer-start) (buffer-end))))
    (if (or (not prompt-validate-function)
	    (let
		((res (funcall prompt-validate-function line)))
	      (when (and res (not (eq res t)))
		(setq line res))
	      res))
	(throw 'prompt line)
      (beep))))

(defun prompt-message (string)
  (set-status-message (concat prompt-title "  " string) prompt-title-view)
  (setq pre-command-hook (list #'(lambda ()
				   (set-status-message prompt-title
						       prompt-title-view)
				   (setq pre-command-hook nil)))))

(defun prompt-select-completion ()
  (interactive))

(defun prompt-setup-completion-buffer ()
  (unless prompt-completion-buffer
    (if completion-buffer-list
	(setq prompt-completion-buffer (car completion-buffer-list)
	      completion-buffer-list (cdr completion-buffer-list))
      (setq prompt-completion-buffer (make-buffer "*completions*")))
    (if (= (window-view-count) 2)
	(progn
	  (setq prompt-new-completion-view t
		prompt-completion-view (open-view))
	  (set-status-message prompt-old-title-msg prompt-title-view)
	  (setq prompt-title-view prompt-completion-view
		prompt-old-title-msg (set-status-message prompt-title
							 prompt-title-view)))
      (setq prompt-new-completion-view nil
	    prompt-completion-view (nth (- (window-view-count) 2)
					(window-view-list)))))
  (with-view prompt-completion-view
    (setq buffer-list (cons prompt-completion-buffer 
			    (delq prompt-completion-buffer buffer-list)))
    (clear-buffer prompt-completion-buffer)
    (set-current-buffer prompt-completion-buffer)))

(defun prompt-remove-completion-buffer ()
  (when prompt-completion-buffer
    (if prompt-new-completion-view
	(close-view prompt-completion-view)
      (with-view prompt-completion-view
	(setq buffer-list (delq prompt-completion-buffer buffer-list))
	(set-current-buffer (car buffer-list))))
    (clear-buffer prompt-completion-buffer)
    (setq completion-buffer-list (cons prompt-completion-buffer
				       completion-buffer-list))))

;; Returns the number of completions found.
(defun prompt-complete ()
  (interactive)
  (if (not prompt-completion-function)
      (progn
	(prompt-message "[No completion function]")
	0)
    (let*
	((word (copy-area (buffer-start) (cursor-pos)))
	 ;; Before making the list of completions, try to
	 ;; restore the original context
	 (comp-list (with-view prompt-original-view
		      (with-buffer prompt-original-buffer
			(funcall prompt-completion-function word))))
	 (num-found (length comp-list))
	 (buffer-record-undo nil))
      (cond
       ((= num-found 0)
	(when prompt-completion-buffer
	  (clear-buffer prompt-completion-buffer))
	(prompt-message "[No completions]"))
       ((= num-found 1)
	(goto-char (replace-string word (car comp-list) (buffer-start)))
	(when prompt-completion-buffer
	  (clear-buffer prompt-completion-buffer))
	(prompt-message "[Unique completion]"))
       (t
	(prompt-print-completions comp-list)
	(when (not (string-head-eq (car comp-list) word))
	  ;; Completions don't match their source at all.
	  (delete-area (buffer-start) (cursor-pos))
	  (setq word ""))
	(goto-char (replace-string word
				   (make-completion-string word comp-list)
				   (buffer-start)))
	(prompt-message (format nil "[%d completions]" num-found))))
      num-found)))

(defun prompt-list-completions ()
  (interactive)
  (if (not prompt-completion-function)
      (progn
	(prompt-message "[No completion function]")
	0)
    (let*
	((word (copy-area (buffer-start) (cursor-pos)))
	 ;; Before making the list of completions, try to
	 ;; restore the original context
	 (comp-list (with-view prompt-original-view
		      (with-buffer prompt-original-buffer
			(funcall prompt-completion-function word)))))
      (prompt-print-completions comp-list)
      (prompt-message (format nil "[%d completions]" (length comp-list))))))

(defun prompt-print-completions (comp-list)
  (let*
      ((ipos (buffer-start))
       ;; Don't want to record undo information for the completion list
       (buffer-record-undo nil))
    (prompt-setup-completion-buffer)
    (with-view prompt-completion-view
      (insert "\n" ipos)
      (while (consp comp-list)
	(format (cons (current-buffer) ipos) "%s\n" (car comp-list))
	(setq comp-list (cdr comp-list)))
      (goto-buffer-start))))

(defun prompt-cancel ()
  (interactive)
  (message "Quit!")
  (throw 'prompt nil))

(defun prompt-next-history ()
  (interactive)
  (cond
   ((or (= prompt-history-index -1)
	(and (= prompt-history-index 0) (null prompt-default-value)))
    (error "No next item"))
   ((= prompt-history-index 0)
    (setq prompt-history-top (copy-area (buffer-start) (buffer-end)))
    (clear-buffer)
    (insert prompt-default-value))
   ((>= prompt-history-index 1)
    (clear-buffer)
    (insert (if (= prompt-history-index 1)
		prompt-history-top
	      (get-from-ring prompt-history (1- prompt-history-index))))))
  (setq prompt-history-index (1- prompt-history-index)))

(defun prompt-previous-history ()
  (interactive)
  (cond
   ((= prompt-history-index -1)
    (clear-buffer)
    (insert prompt-history-top))
   ((and (>= prompt-history-index 0)
	 (> (ring-size prompt-history) prompt-history-index))
    (when (zerop prompt-history-index)
      (setq prompt-history-top (copy-area (buffer-start) (buffer-end))))
    (clear-buffer)
    (insert (get-from-ring prompt-history (1+ prompt-history-index))))
   (t
    (error "No previous item")))
  (setq prompt-history-index (1+ prompt-history-index)))


;; Various completion/validation functions

(defun prompt-complete-symbol (word)
  (mapcar 'symbol-name (apropos (concat ?^ (regexp-quote word))
				prompt-symbol-predicate)))

(defun prompt-validate-symbol (name)
  (and (find-symbol name)
       (or (not prompt-symbol-predicate)
	   (funcall prompt-symbol-predicate (find-symbol name)))))

(defun prompt-complete-buffer (word)
  (delete-if-not #'(lambda (b)
		     (string-head-eq b word))
		 (mapcar 'buffer-name buffer-list)))

(defun prompt-validate-buffer (name)
  (if (equal name "")
      t
    (get-buffer name)))

(defvar prompt-file-exclude '"\\.(o|jlc|x)$|~$|^#.*#$"
  "A regexp, if it matches the file being considered for completion, the file
is rejected.")

;; Don't want .info files (WB icons) on Amigas, everywhere else they're okay
;; though.
(when (amiga-p)
  (setq prompt-file-exclude (concat prompt-file-exclude "|\\.info$")))

;; Ignore the `.' and `..' directory entries in UNIX
(when (unix-p)
  (setq prompt-file-exclude (concat prompt-file-exclude "|^\\.(\\.|)$")))

(defun prompt-complete-filename (word)
  (setq word (expand-file-name word))
  (let*
      ((path (file-name-directory word))
       (file (file-name-nondirectory word))
       (files (directory-files path)))
    (mapcar #'(lambda (x &aux y) 
		(when (file-directory-p (setq y (concat path x)))
		  (setq y (concat y ?/)))
		y)
	    (delete-if #'(lambda (f)
			   (or (not (string-head-eq f file))
			       (regexp-match prompt-file-exclude f)))
		       files))))

(defun prompt-validate-filename (name)
  (file-exists-p name))

(defun prompt-complete-directory (word)
  (setq word (expand-file-name word))
  (let
      ((path (file-name-directory word))
       (file (file-name-nondirectory word)))
    (delq 'nil
	  (mapcar #'(lambda (x)
		      (when (file-directory-p (concat path x))
			(concat path x ?/)))
		  (delete-if #'(lambda (f)
				 (not (string-head-eq f file)))
			     (directory-files path))))))

(defun prompt-validate-directory (name)
  (file-directory-p name))

(defun prompt-complete-from-list (word)
  (let
      ((src prompt-list)
       (dst ()))
    (while src
      (when (regexp-match (concat ?^ (regexp-quote word))
			  (car src) prompt-list-fold-case)
	(setq dst (cons (car src) dst)))
      (setq src (cdr src)))
    dst))

(defun prompt-validate-from-list (name)
  (if (null prompt-list-fold-case)
      ;; Make sure it returns the *symbol* t
      (and (member name prompt-list) t)
    (let
	((list prompt-list))
      (while list
	(when (regexp-match (concat ?^ (regexp-quote name) ?$)
			    (car list) t)
	  (return t))
	(setq list (cdr list))))))


;; High-level entrypoints; prompt for a specific type of object

;;;###autoload
(defun prompt-for-file (&optional prompt existing start default history-list)
  "Prompt for a file, if EXISTING is t only files which exist are
allowed to be entered."
  (unless (stringp prompt)
    (setq prompt "Enter filename:"))
  (unless (stringp start)
    (setq start (file-name-directory (buffer-file-name))))
  (if (and (amiga-p) amiga-use-file-req-p)
      (if existing
	  (let
	      (file)
	    (while (null file)
	      (unless (setq file (file-req prompt start))
		(return))
	      (unless (file-exists-p file)
		(beep)
		(req "That file doesn't exist!" "Continue")
		(setq file nil)))
	    file)
	(file-req prompt start))
    (let*
	((prompt-completion-function 'prompt-complete-filename)
	 (prompt-validate-function (if existing
				       'prompt-validate-filename
				     nil))
	 (prompt-word-regexps prompt-def-regexps)
	 (prompt-history (or history-list prompt-file-history))
	 (prompt-default-value default)
	 (str (prompt prompt start)))
      (when (and (string= str "") default)
	(setq str default))
      (when str
	(expand-file-name str)))))

;;;###autoload
(defun prompt-for-directory (&optional prompt existing start default)
  "Prompt for a directory, if EXISTING is t only files which exist are
allowed to be entered."
  (unless (stringp prompt)
    (setq prompt "Enter filename:"))
  (unless (stringp start)
    (setq start (file-name-directory (buffer-file-name))))
  (let*
      ((prompt-completion-function 'prompt-complete-directory)
       (prompt-validate-function (if existing
				     'prompt-validate-directory
				   nil))
       (prompt-word-regexps prompt-def-regexps)
       (prompt-history prompt-file-history)
       (str (prompt prompt start)))
    (when (and (string= str "") default)
      (setq str default))
    (when str
      (expand-file-name str))))

;;;###autoload
(defun prompt-for-buffer (&optional prompt existing default)
  "Prompt for a buffer, if EXISTING is t the buffer selected must exist,
otherwise if EXISTING is nil the buffer will be created if it doesn't
exist already. DEFAULT is the value to return if the user enters the null
string, if nil the current buffer is returned."
  (unless (stringp prompt)
    (setq prompt "Enter buffer name:"))
  (let*
      ((prompt-completion-function 'prompt-complete-buffer)
       (prompt-validate-function (if existing
				     'prompt-validate-buffer
				   nil))
       (prompt-word-regexps prompt-def-regexps)
       (prompt-default-value (buffer-name (or default (current-buffer))))
       (buf (prompt prompt)))
    (if (equal buf "")
	(or default (current-buffer))
      (unless (get-buffer buf)
	(when (not existing)
	  (open-buffer buf))))))

;; borrowed from lisp-mode.jl
(defvar symbol-word-regexps ["[^][()?'\"#; ]" "[][()?'\"#; ]|$"])

;;;###autoload
(defun prompt-for-symbol (&optional prompt prompt-symbol-predicate)
  "Prompt for an existing symbol. If PROMPT-SYMBOL-PREDICATE is given the
symbol must agree with it."
  (unless (stringp prompt)
    (setq prompt "Enter name of symbol:"))
  (let
      ((prompt-completion-function 'prompt-complete-symbol)
       (prompt-validate-function 'prompt-validate-symbol)
       (prompt-word-regexps symbol-word-regexps)
       (prompt-history prompt-symbol-history))
    (intern (prompt prompt))))

;;;###autoload
(defun prompt-for-lisp (&optional prompt)
  "Prompt for a lisp object."
  (unless (stringp prompt)
    (setq prompt "Enter a Lisp object:"))
  (let
      ((prompt-completion-function 'prompt-complete-symbol)
       (prompt-validate-function nil)
       (prompt-word-regexps symbol-word-regexps)
       (prompt-symbol-predicate nil))
    (read-from-string (prompt prompt))))

;;;###autoload
(defun prompt-for-function (&optional prompt)
  "Prompt for a function."
  (prompt-for-symbol (or prompt "Enter name of function:")
		     'fboundp))

;;;###autoload
(defun prompt-for-variable (&optional prompt)
  "Prompt for a variable."
  (prompt-for-symbol (or prompt "Enter name of variable:")
		     'boundp))

;;;###autoload
(defun prompt-for-command (&optional prompt)
  "Prompt for a command."
  (prompt-for-symbol (or prompt "Enter name of command:")
		     'commandp))

;;;###autoload
(defun prompt-from-list (prompt-list prompt &optional start)
  "Return a selected choice from the list of options (strings) PROMPT-LIST.
PROMPT is the title displayed, START the starting choice."
  (let
      ((prompt-completion-function 'prompt-complete-from-list)
       (prompt-validate-function 'prompt-validate-from-list)
       (prompt-word-regexps prompt-def-regexps))
  (prompt prompt start)))

;;;###autoload
(defun prompt-for-string (&optional prompt start)
  (prompt (or prompt "Enter string: ") start))

;;;###autoload
(defun prompt-for-number (&optional prompt)
  (let
      (num)
    (while (not (numberp num))
      (setq num (read-from-string (prompt (or prompt "Enter number: ")))))
    num))

;; Compatibility
;;;###autoload
(defmacro prompt2 (&rest args)
  (cons 'prompt args))


;; ask functions

;;;###autoload
(defun yes-or-no-p (question)
  "Prompts the user for a yes or no answer to QUESTION, returns t for yes."
  (let*
      ((answer (prompt (concat question " (yes or no) "))))
    (string= "yes" answer)))

(defvar y-or-n-keymap (make-keylist))
(bind-keys y-or-n-keymap
  "n" '(throw 'ask nil)
  "BS" '(throw 'ask nil)
  "y" '(throw 'ask t)
  "SPC" '(throw 'ask t))

;;;###autoload
(defun y-or-n-p (question)
  "Prompts the user for a single keypress response, either `y' or `n' to the
string QUESTION, returns t for `y'."
  (let
      ((buf (current-buffer))
       (view (current-view))
       (prompt-buffer (get-prompt-buffer)))
    (with-view (minibuffer-view)
      (with-buffer prompt-buffer
	(let
	    ((old-u-k-h unbound-key-hook)
	     (old-k-p keymap-path))
	  (setq unbound-key-hook '(beep)
		keymap-path '(y-or-n-keymap))
	  (insert (concat question " (y or n) ") (buffer-start))
	  (unwind-protect
	      (catch 'ask
		(recursive-edit))
	    (with-buffer prompt-buffer
	      (setq keymap-path old-k-p
		    unbound-key-hook old-u-k-h
		    status-line-cursor nil))
	    (return-prompt-buffer prompt-buffer)))))))
