;;;; rm-restrict.jl -- Generic methods for restricting over messages
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
(provide 'rm-restrict)

;; Commentary:
;;
;; This module allows complex message filters to be created intuitively
;; (if you understand Lisp syntax, anyway). Rules are defined using the
;; defrule macro, a corresponding function is created that instantiates
;; the rule, so that it can be called to test if a specified message
;; matches.
;;
;; Enough jargon. Here are a couple of examples:
;;
;;	(defrule recently-from-jsh ()
;;	  (and (header "from" "john@dcs")
;;	       (sent-after "last week")))
;;
;;	(defrule from-jsh-to-djke ()
;;	  (and (recently-from-jsh)
;;	       (header "to" "djke@dcs")))
;;
;; As can be seen, complex structures can be created (the `and', `or',
;; and `not' boolean operators work as normal), with rules calling
;; sub-rules (possibly with parameters).
;;
;; Speed of matching is achieved in two ways: (1) some of the primitive
;; rules expand inline to the code needed to do the matching, and (2),
;; rules can be easily compiled to bytecode:
;;
;;	(compile-rule 'recently-from-jsh)
;;	(compile-rule 'from-jsh-to-djke)
;;
;; Primitive rules include:
;;
;;	(header HEADER-REGEXP CONTENTS-REGEXP)
;;	(sent-after DATE-STRING)
;;	(sent-before DATE-STRING)
;;	(mailbox FOLDER-REGEXP)
;;	(recipient REGEXP)
;;	(sender REGEXP)
;;	(subject REGEXP)
;;	(lines NUMBER)
;;	(attribute FLAG-SYMBOL)
;;	(body REGEXP)
;;
;; TODO:
;;  + Make the date syntax a lot friendlier. Currently we accept:
;;	last (week|fortnight|month|year)
;;	N (day|week|fortnight|month|year)s? ago
;;	yesterday
;;	DD Month YYYY HH:MM:SS TTTT	(time optional)



;; Configuration

(defvar rm-rule-message nil
  "The message being examined by the called restriction rule.")

(defvar rm-rule-fold-case t
  "When t, all string comparison in rules is case-insensitive.")

(defvar rm-rule-history (make-ring)
  "The ring containing recently used restriction rules.")


;; Entry points

;; Can't autoload macros..
(defmacro defrule (name args body)
  "Define a rule for restricting the set of messages considered. The new rule
is called NAME (a symbol), it takes the lambda list of arguments ARGS, and
is defined by the single form BODY.

For example, to define a rule accepting only messages sent by me (that's
`john@dcs.warwick.ac.uk') the following would suffice:

(defrule from-jsh ()
  (header \"from\" \"john@dcs\\\\.warwick\\\\.ac\\\\.uk\"))"
  (let
      ((symbol (rm-rule-symbol name)))
    `(progn
       (put ',name 'rm-rule-symbol ',symbol)
       (defun ,symbol ,args ,(rm-make-rule-body body)))))
(put 'defrule 'lisp-indent 'defun)

;;;###autoload
(defun define-rule (name args body)
  "Define a rule for restricting the set of messages considered. The new rule
is called NAME (a symbol), it takes the lambda list of arguments ARGS, and
is defined by the single form BODY.

For example, to define a rule accepting only messages sent by me (that's
`john@dcs.warwick.ac.uk') BODY could be the form

	(header \"from\" \"john@dcs\\\\.warwick\\\\.ac\\\\.uk\")"
  (interactive
   (list (prompt-for-symbol "Name of rule:" 'identity)
	 nil
	 (prompt-for-lisp "Body of rule:")))
  (let
      ((symbol (rm-rule-symbol name)))
    (fset symbol (list 'lambda args (rm-make-rule-body body)))
    (put name 'rm-rule-function symbol)
    name))

(defun compile-rule (name)
  "Compile the message selection rule called NAME (a symbol) to bytecode."
  (interactive "SRule to compile:")
  (let
      ((symbol (or (get name 'rm-rule-symbol)
		   (error "No rule called %s" name))))
    (compile-function symbol)))

(defmacro rm-rule-symbol (name)
  "For a rule called NAME (a symbol), return the symbol that is used to
contain its definition as a function."
  `(intern (concat "rm-rule:" (symbol-name ,name))))

;; Translate all functions called in INPUT to their rule-based versions
(defun rm-make-rule-body (input)
  (if (listp input)
      (let
	  ((function (car input)))
	(if (symbolp function)
	    (let
		((compiler (get function 'rm-rule-compiler)))
	      (if compiler
		  (macroexpand (funcall compiler input))
		(cons (or (get function 'rm-rule-function)
			  (error "Unknown operator in rule: %s" function))
		      (mapcar #'(lambda (x)
				  (macroexpand (rm-make-rule-body x)))
			      (cdr input)))))
	  input))
    input))

;; Filter the list MESSAGES, by RULE
;;;###autoload
(defun rm-filter-by-rule (messages rule)
  (let
      ((function (or (get rule 'rm-rule-symbol)
		     (error "No rule called %s" rule))))
    (filter #'(lambda (rm-rule-message)
		(funcall function)) messages)))

;; Apply the message RM-RULE-MESSAGE to RULE, returning t if it matches
;;;###autoload
(defun rm-apply-rule (rule rm-rule-message)
  (funcall (or (get rule 'rm-rule-symbol)
	       (error "No rule called %s" rule))))

;; Apply the message MESSAGE to the list of rules RULE-LIST. Return t if
;; any rule matches (without testing any remaining rules)
;;;###autoload
(defun rm-apply-rules (rule-list message)
  (catch 'exit
    (mapc #'(lambda (r)
	      (and (rm-apply-rule r message)
		   (throw 'exit t))) rule-list)))

;; Standard rules

(put 'and 'rm-rule-function 'and)
(put 'or 'rm-rule-function 'or)
(put 'not 'rm-rule-function 'not)

;; (header HEADER-REGEXP CONTENTS-REGEXP)
(put 'header 'rm-rule-function 'rm-rule:header)
(defmacro rm-rule:header (name pattern)
  `(let
       ((contents (rm-get-msg-header rm-rule-message ,name)))
     (when contents
       (string-match ,pattern contents nil rm-rule-fold-case))))

;; (subject REGEXP)
(put 'subject 'rm-rule-function 'rm-rule:subject)
(defmacro rm-rule:subject (regexp)
  `(let
       ((subject (rm-get-subject rm-rule-message)))
     (when subject
       (string-match ,regexp subject nil rm-rule-fold-case))))

;; (sent-after DATE-STRING)
;; (sent-before DATE-STRING)
(put 'sent-after 'rm-rule-compiler 'rm-compile-sent-x)
(put 'sent-before 'rm-rule-compiler 'rm-compile-sent-x)
(defun rm-compile-sent-x (form)
  (let
      ((date (rm-parse-date (nth 1 form))))
    (list 'rm-rule-sent-date
	  (list 'quote date)
	  (eq (car form) 'sent-after))))

;; DATE is (absolute DAYS . SECONDS) or (relative DAYS . SECONDS)
(defun rm-rule-sent-date (date after)
  (let
      ((msg-date (rm-get-date-vector rm-rule-message)))
    (when msg-date
      (setq msg-date (aref msg-date mail-date-epoch-time))
      (funcall (if after '> '<)
	       msg-date
	       (if (eq (car date) 'absolute)
		   (cdr date)
		 (let
		     ((current (current-time)))
		   (rplaca current (- (car current) (nth 1 date)))
		   (rplacd current (- (cdr current) (nthcdr 2 date)))
		   (fix-time current)))))))

;; (mailbox FOLDER-REGEXP)
(put 'mailbox 'rm-rule-function 'rm-rule:mailbox)
(defmacro rm-rule:mailbox (mailbox)
  `(string-match ,mailbox (buffer-file-name
			   (mark-file (rm-get-msg-field rm-rule-message
							rm-msg-mark)))
		 nil rm-rule-fold-case))

;; (recipient REGEXP)
(put 'recipient 'rm-rule-function 'rm-rule:recipient)
(defun rm-rule:recipient (name)
  (catch 'return
    (mapc #'(lambda (cell)
	      (when (or (and (car cell) (string-match name (car cell)
						      nil rm-rule-fold-case))
			(and (cdr cell) (string-match name (cdr cell)
						      nil rm-rule-fold-case)))
		(throw 'return t)))
	  (rm-get-recipients rm-rule-message))
    nil))

;; (sender REGEXP)
(put 'sender 'rm-rule-function 'rm-rule:sender)
(defun rm-rule:sender (name)
  (catch 'return
    (mapc #'(lambda (cell)
	      (when (or (and (car cell) (string-match name (car cell)
						      nil rm-rule-fold-case))
			(and (cdr cell) (string-match name (cdr cell)
						      nil rm-rule-fold-case)))
		(throw 'return t)))
	  (rm-get-senders rm-rule-message))
    nil))

;; (lines NUMBER)
(put 'lines 'rm-rule-function 'rm-rule:lines)
(defmacro rm-rule:lines (lines)
  `(> (rm-get-msg-field rm-rule-message rm-msg-total-lines) ,lines))

;; (attribute FLAG-SYMBOL)
(put 'attribute 'rm-rule-function 'rm-rule:attribute)
(defmacro rm-rule:attribute (attr)
  `(memq ',attr (rm-get-msg-field rm-rule-message rm-msg-flags)))

;; (body REGEXP)
(put 'body 'rm-rule-function 'rm-rule:body)
(defun rm-rule:body (regexp)
  (with-buffer (mark-file (rm-get-msg-field rm-rule-message rm-msg-mark))
    (save-restriction
      (unrestrict-buffer)
      (restrict-buffer (rm-message-body rm-rule-message)
		       (rm-message-end rm-rule-message))
      (re-search-forward regexp (start-of-buffer) nil rm-rule-fold-case))))


;; More intuitive date parsing

;; Returns either (absolute DAYS . SECONDS) absolute time, or
;; (relative DAYS . SECONDS)
;; TODO: `20 years ago' overflows an integer, etc..
(defun rm-parse-date (string)
  (let
      (seconds-ago abs-time)
    (cond
     ((string-match
       "^(an?|[0-9]+) *(seconds|minute|hour|day|week|fortnight|month|year)s? +ago$"
       string nil t)
      (setq seconds-ago (* (if (= (aref string 0) ?a)
			       1
			     (read-from-string (expand-last-match "\\1")))
			   (if (= (aref string (match-start 2)) ?m)
			       (if (= (aref string (1+ (match-start 2))) ?i)
				   60
				 2419200)	;28*24*60*60
			     (cdr (assq (aref string (match-start 2))
					'((?s . 1) (?h . 3600) (?d . 86400)
					  (?w . 604800) (?f . 1209600)
					  (?m . 2419200) (?y . 31536000))))))))
     ((string-match "^last +(week|fortnight|month|year)$" string nil t)
      (setq seconds-ago (cdr (assq (aref string (match-start 1))
				   '((?w . 604800) (?f . 1209600)
				     (?m . 2419200) (?y . 31536000))))))
     ((string-match "^yesterday$" string nil t)
      (setq seconds-ago 86400))
     (t
      (let
	  ((date (mail-parse-date string)))
	(unless date
	  (error "Invalid date specification: %s" string))
	(setq abs-time (aref date mail-date-epoch-time)))))
    (if seconds-ago
	(list* 'relative (/ seconds-ago 86400) (mod seconds-ago 86400))
      (cons 'absolute abs-time))))


;; Prompting

;;;###autoload
(defun rm-prompt-for-rule (&optional prompt)
  (let
      ((prompt-history rm-rule-history)
       (rule (prompt-for-symbol (or prompt
				    "Restriction rule (`new' to define rule):")
				#'(lambda (sym)
				    (or (eq sym 'new)
					(eq sym 'nil)
					(let
					    ((fun (rm-rule-symbol sym)))
					  (fboundp fun)))))))
    (if (eq rule 'new)
	(call-command 'define-rule)
      rule)))
