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
(require 'completion)
(provide 'prompt)

(defvar prompt-keymap
  (bind-keys (make-sparse-keymap)
    "TAB"	'prompt-complete
    "RET"	'prompt-enter-line
    "LMB-CLICK2" 'prompt-select-completion
    "RMB-CLICK1" 'prompt-complete
    "Meta-?"	'prompt-list-completions
    "Meta-n"	'prompt-next-history
    "Meta-p"	'prompt-previous-history
    "Ctrl-g"	'prompt-cancel
    "Ctrl-a"	'prompt-start-of-line))
  

;; Configuration variables

(defvar prompt-completion-function nil
  "Optional function taking one argument, the string to be completed. It
should return a list of all matches. If the symbol `t', the command
complete-at-point will be used instead.")

(defvar prompt-validate-function nil
  "Optional function taking one argument, the string which has been entered.
Should return non-nil when this string may be accepted (and therefore the
prompt will end). If it returns the symbol t the string is returned as-is,
if some other non-nil value is returned *that* is the value returned by
the prompt.")

(defvar prompt-list nil
  "Used by the `prompt-complete-from-list' and `prompt-validate-from-list'
to supply possible completions.")

(defvar prompt-list-fold-case nil
  "When t `prompt-complete-from-list' and `prompt-validate-from-list' ignore
case.")

(defvar prompt-symbol-predicate nil
  "Predicate used when prompting for symbols.")

(defvar prompt-default-history (make-ring)
  "Catch-all history list for prompt.")

(defvar prompt-file-history (make-ring)
  "File history list for prompt.")

(defvar prompt-symbol-history (make-ring)
  "Symbol history list for prompt.")

(defvar prompt-history prompt-default-history
  "A ring buffer used to record history information for the minibuffer.")

(defvar prompt-glyph-table nil
  "When non-nil the glyph table to use for prompts.")

(defvar prompt-title-face bold-face
  "Face used for titles of prompts.")

(defvar before-prompt-hook nil
  "Prompt called before entering a prompt.")


;; Working variables used by more than one function

(defvar prompt-buffer nil
  "The buffer being used for the prompt.")

(defvar prompt-title-extent nil
  "Extent covering the title of the prompt.")

(defvar prompt-title nil
  "Title string of the prompt.")

(defvar prompt-original-buffer nil
  "The active buffer when the prompt was called.")

(defvar prompt-original-view nil
  "The active view when the prompt was called.")

(defvar prompt-original-size nil
  "The number of lines in the minibuffer view originally.")

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
      ((prompt-buffer (make-buffer "*prompt*"))
       (prompt-original-buffer (current-buffer))
       (prompt-original-view (current-view))
       (prompt-history-index 0)
       (prompt-history-top nil)
       prompt-title-extent prompt-original-size result)
    (unless (stringp prompt-title)
      (setq prompt-title "Enter string:"))
    (unless (string-match " $" prompt-title)
      (setq prompt-title (concat prompt-title ? )))
    (unwind-protect
	(with-view (minibuffer-view)
	  (setq prompt-original-size (cdr (view-dimensions)))
	  (when (= prompt-original-size 1)
	    (condition-case nil
		(enlarge-view 1)
	      (error)))
	  (with-buffer prompt-buffer
	    (setq prompt-title-extent
		  (make-extent (start-of-buffer) (insert prompt-title)
			       (list 'face prompt-title-face)))
	    (extent-set prompt-title-extent 'read-only t)
	    (extent-set prompt-title-extent 'glyph-table glyph-table)
	    (extent-put prompt-title-extent 'front-sticky t)
	    (setq buffer-undo-list nil)
	    (when prompt-glyph-table
	      (setq glyph-table prompt-glyph-table))
	    (when (stringp start)
	      (insert start)
	      ;; Make this a separate undo operation
	      (setq buffer-undo-list (cons nil buffer-undo-list)))
	    (make-local-variable 'pre-command-hook)
	    (setq local-keymap prompt-keymap)
	    (call-hook 'before-prompt-hook)
	    (setq result (catch 'prompt (recursive-edit)))
	    (when (and result prompt-history
		       (stringp result)
		       (not (string= result ""))
		       (or (zerop (ring-size prompt-history))
			   (not (equal (get-from-ring prompt-history)
				       result))))
	      (add-to-ring prompt-history result)))
	  (with-view (minibuffer-view)
	    (when (> (cdr (view-dimensions)) prompt-original-size)
	      (shrink-view (- (cdr (view-dimensions)) prompt-original-size))))
	  (kill-all-local-variables prompt-buffer))
      (completion-remove-view))
    result))


;; Subroutines

(defun prompt-enter-line ()
  (interactive)
  (let
      ((line (copy-area (extent-end prompt-title-extent) (end-of-buffer))))
    (if (or (not prompt-validate-function)
	    (let
		((res (funcall prompt-validate-function line)))
	      (when (and res (not (eq res t)))
		(setq line res))
	      res))
	(throw 'prompt line)
      (beep))))

(defun prompt-message (string)
  (message string))

(defun prompt-complete (&optional only-display)
  (interactive)
  (cond ((null prompt-completion-function)
	 (prompt-message "[No completion function]"))
	((eq prompt-completion-function t)
	 (complete-at-point only-display))
	(t
	 (let*
	     ((w-start (extent-end prompt-title-extent))
	      (word (copy-area w-start (cursor-pos)))
	      ;; Before making the list of completions, try to
	      ;; restore the original context
	      (comp-list (with-view prompt-original-view
			   (with-buffer prompt-original-buffer
			     (funcall prompt-completion-function word)))))
	   (completion-insert comp-list word only-display)))))

(defun prompt-list-completions ()
  (interactive)
  (prompt-complete t))

(defun prompt-cancel ()
  (interactive)
  (message "Quit!")
  (throw 'prompt nil))

(defun prompt-next-history ()
  (interactive)
  (unless prompt-history
    (error "No history for this prompt"))
  (cond
   ((or (= prompt-history-index -1)
	(and (= prompt-history-index 0) (null prompt-default-value)))
    (error "No next item"))
   ((= prompt-history-index 0)
    (setq prompt-history-top (copy-area (extent-end prompt-title-extent)
					(end-of-buffer)))
    (delete-area (extent-end prompt-title-extent) (end-of-buffer))
    (insert prompt-default-value))
   ((>= prompt-history-index 1)
    (delete-area (extent-end prompt-title-extent) (end-of-buffer))
    (insert (if (= prompt-history-index 1)
		prompt-history-top
	      (get-from-ring prompt-history (1- prompt-history-index))))))
  (setq prompt-history-index (1- prompt-history-index)))

(defun prompt-previous-history ()
  (interactive)
  (unless prompt-history
    (error "No history for this prompt"))
  (cond
   ((= prompt-history-index -1)
    (clear-buffer)
    (insert prompt-history-top))
   ((and (>= prompt-history-index 0)
	 (> (ring-size prompt-history) prompt-history-index))
    (when (zerop prompt-history-index)
      (setq prompt-history-top (copy-area (extent-end prompt-title-extent)
					  (end-of-buffer))))
    (delete-area (extent-end prompt-title-extent) (end-of-buffer))
    (insert (get-from-ring prompt-history (1+ prompt-history-index))))
   (t
    (error "No previous item")))
  (setq prompt-history-index (1+ prompt-history-index)))

(defun prompt-start-of-line ()
  (interactive "@")
  (if (zerop (pos-line (cursor-pos)))
      (extent-end prompt-title-extent)
    (start-of-line)))


;; Various completion/validation functions

(defun prompt-complete-symbol (word)
  (mapcar symbol-name (apropos (concat ?^ (quote-regexp word))
			       prompt-symbol-predicate)))

(defun prompt-validate-symbol (name)
  (and (or (not prompt-symbol-predicate)
	   (funcall prompt-symbol-predicate (intern name)))
       name))

(defun prompt-complete-buffer (word)
  (delete-if-not #'(lambda (b)
		     (string-head-eq b word))
		 (mapcar buffer-name buffer-list)))

(defun prompt-validate-buffer (name)
  (and (or (equal name "")
	   (get-buffer name)) t))

(defvar prompt-file-exclude '"\\.(o|jlc|x)$|~$|^#.*#$"
  "A regexp, if it matches the file being considered for completion, the file
is rejected.")

;; Ignore the `.' and `..' directory entries in UNIX
(when (eq operating-system 'unix)
  (setq prompt-file-exclude (concat prompt-file-exclude "|^\\.(\\.|)$")))

(defun prompt-complete-filename (word)
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
			       (string-match prompt-file-exclude f)))
		       files))))

(defun prompt-validate-filename (name)
  (and (file-exists-p name) t))

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
  (and (file-directory-p name) t))

(defun prompt-abbreviate-filename (name)
  (let
      ((abbrev (file-name-nondirectory name)))
    (if (string= abbrev "")
	(file-name-as-directory
	 (file-name-nondirectory (directory-file-name name)))
      abbrev)))

(defun prompt-complete-from-list (word)
  (let
      ((src prompt-list)
       (dst ()))
    (while src
      (when (string-match (concat ?^ (quote-regexp word))
			  (car src) nil prompt-list-fold-case)
	(setq dst (cons (car src) dst)))
      (setq src (cdr src)))
    dst))

(defun prompt-validate-from-list (name)
  (if (null prompt-list-fold-case)
      ;; Make sure it returns the *symbol* t
      (and (member name prompt-list) t)
    (let
	((lst prompt-list))
      (catch 'exit
	(while lst
	  (when (string-match (concat ?^ (quote-regexp name) ?$)
			      (car lst) nil t)
	    (throw 'exit t))
	  (setq lst (cdr lst)))))))


;; High-level entrypoints; prompt for a specific type of object

;;;###autoload
(defun prompt-for-file (&optional title existing start default history-list)
  "Prompt for a file, if EXISTING is t only files which exist are
allowed to be entered."
  (unless (stringp title)
    (setq title "Enter filename:"))
  (setq start (if (stringp start)
		  (expand-file-name start)
		(file-name-as-directory default-directory)))
  (let*
      ((prompt-completion-function prompt-complete-filename)
       (prompt-validate-function (if existing
				     prompt-validate-filename
				   nil))
       (prompt-history (or history-list prompt-file-history))
       (prompt-default-value (and default (expand-file-name default)))
       (completion-abbrev-function prompt-abbreviate-filename)
       (str (prompt title start)))
    (when (and (string= str "") default)
      (setq str default))
    str))

;;;###autoload
(defun prompt-for-directory (&optional title existing start default)
  "Prompt for a directory, if EXISTING is t only files which exist are
allowed to be entered."
  (unless (stringp title)
    (setq title "Enter filename:"))
  (unless (stringp start)
    (setq start (file-name-as-directory default-directory)))
  (let*
      ((prompt-completion-function prompt-complete-directory)
       (prompt-validate-function (if existing
				     prompt-validate-directory
				   nil))
       (prompt-history prompt-file-history)
       (prompt-default-value (and default (expand-file-name default)))
       (completion-abbrev-function prompt-abbreviate-filename)
       (str (prompt title start)))
    (when (and (string= str "") default)
      (setq str default))
    str))

;;;###autoload
(defun prompt-for-buffer (&optional title existing default)
  "Prompt for a buffer, if EXISTING is t the buffer selected must exist,
otherwise if EXISTING is nil the buffer will be created if it doesn't
exist already. DEFAULT is the value to return if the user enters the null
string, if nil the current buffer is returned."
  (unless (stringp title)
    (setq title "Enter buffer name:"))
  (let*
      ((prompt-completion-function prompt-complete-buffer)
       (prompt-validate-function (if existing
				     prompt-validate-buffer
				   nil))
       (prompt-default-value (buffer-name (or default (current-buffer))))
       (buf (prompt title)))
    (if (equal buf "")
	(or default (current-buffer))
      (unless (get-buffer buf)
	(when (not existing)
	  (open-buffer buf))))))

;;;###autoload
(defun prompt-for-symbol (&optional title prompt-symbol-predicate start)
  "Prompt for an existing symbol. If PROMPT-SYMBOL-PREDICATE is given the
symbol must agree with it."
  (unless (stringp title)
    (setq title "Enter name of symbol:"))
  (let
      ((prompt-completion-function prompt-complete-symbol)
       (prompt-validate-function prompt-validate-symbol)
       (prompt-history prompt-symbol-history))
    (intern (prompt title start))))

;;;###autoload
(defun prompt-for-lisp (&optional title start)
  "Prompt for a lisp object."
  (unless (stringp title)
    (setq title "Enter a Lisp object:"))
  (let
      ((prompt-completion-function t)
       (prompt-validate-function nil)
       (prompt-symbol-predicate nil)
       (before-prompt-hook
	(cons #'(lambda ()
		  (lisp-mode)
		  ;; This is something of a kludge
		  (setq local-keymap 'prompt-keymap))
	      before-prompt-hook)))
    (read-from-string (prompt title start))))

;;;###autoload
(defun prompt-for-function (&optional title start)
  "Prompt for a function."
  (prompt-for-symbol (or title "Enter name of function:")
		     (lambda (x)
		       (and (boundp x)
			    (functionp (symbol-value x)))) start))

;;;###autoload
(defun prompt-for-variable (&optional title start)
  "Prompt for a variable."
  (prompt-for-symbol (or title "Enter name of variable:") boundp start))

;;;###autoload
(defun prompt-for-command (&optional title start)
  "Prompt for a command."
  (prompt-for-symbol (or title "Enter name of command:") commandp))

;;;###autoload
(defun prompt-from-list (prompt-list title &optional start dont-validate)
  "Return a selected choice from the list of options (strings) PROMPT-LIST.
PROMPT is the title displayed, START the starting choice.
Unless DONT-VALIDATE is t, only a member of PROMPT-LIST will be returned."
  (let
      ((prompt-completion-function prompt-complete-from-list)
       (prompt-validate-function (if dont-validate
				     nil
				   prompt-validate-from-list))
       (completion-fold-case prompt-list-fold-case))
  (prompt title start)))

;;;###autoload
(defun prompt-for-string (&optional title start)
  (let
      ((prompt-completion-function prompt-complete-filename)
       (prompt-validate-function nil))
    (prompt (or title "Enter string: ") start)))

;;;###autoload
(defun prompt-for-number (&optional title)
  (let
      (num)
    (while (not (numberp num))
      (setq num (read-from-string (prompt (or title "Enter number: ")))))
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

(defvar y-or-n-keymap
  (bind-keys (make-sparse-keymap)
    "n" '(throw 'ask nil)
    "BS" '(throw 'ask nil)
    "y" '(throw 'ask t)
    "SPC" '(throw 'ask t)))

;;;###autoload
(defun y-or-n-p (question &optional keymap help-string)
  "Prompts the user for a single keypress response, either `y' or `n' to the
string QUESTION, returns t for `y'."
  (let
      ((buf (current-buffer))
       (view (current-view))
       (prompt-buffer (make-buffer "*y-or-n*")))
    (with-view (minibuffer-view)
      (with-buffer prompt-buffer
	(setq unbound-key-hook '(beep)
	      local-keymap (or keymap 'y-or-n-keymap))
	(insert (concat question ?  (or help-string "(y or n)") ? )
		(start-of-buffer))
	(unwind-protect
	    (catch 'ask
	      (recursive-edit))
	  (with-buffer prompt-buffer
	    (kill-all-local-variables)))))))

(defvar map-y-or-n-keymap
  (bind-keys (make-sparse-keymap y-or-n-keymap)
    "!" '(throw 'map 'all-t)
    "q" '(throw 'map 'quit)))

;;;###autoload
(defun map-y-or-n-p (question inputs callback)
  "Ask the user a yes-or-no question for each object in the list of INPUTS.
QUESTION defines the question asked, either a string that will be passed to
the format function with the current input object as an argument, or a
function that will be called with a single argument, the input, that will
return a string.

CALLBACK is a function of a single argument that will be called (with the
input as its parameter), when the user answers `yes' for that input. If the
answer is `no', no function is called.

The function returns t only if _all_ of the inputs were answered with yes."
  (let
      ((all-t t))
    (when (eq 'all-t (catch 'map
		       (while inputs
			 (let*
			     ((q (if (stringp question)
				     (format nil question (car inputs))
				   (funcall question (car inputs))))
			      (a (y-or-n-p q map-y-or-n-keymap
					   "(y, n, !, q)")))
			   (if a
			       (funcall callback (car inputs))
			     (setq all-t nil))
			   (setq inputs (cdr inputs))))))
      ;; User answered with "!", so loop over all remaining inputs
      (while inputs
	(funcall callback (car inputs))
	(setq inputs (cdr inputs))))
    all-t))
