;;; python-mode.jl -- a basic Python major mode for Jade
;; Author: Andrew Rodionoff <arnost@mail.ru>
;; Jade is Copyright (C) John Harper
;; 
;; This mode is a work in progress. It is likely to indent lines incorrectly,
;; slow down editing and do other weird things to your sources.
;; Proceed with caution!!
;;
;; TODO (in order of importance):
;; - Ability to run Python files and buffer regions (and jump to errors)
;; - Ability to retrieve docstrings for Python functions
;; - Cleanup multiquote/escaped quotes/continuations issues
;; - Python shell mode


(provide 'python-mode)

(defvar python-indent-step 3)

(defvar python-blank-or-comment-re "[ \t]*($|#)"
  "Regular expression matching a blank or comment line.")

(defvar python-dedent-stmt-re
      "[ \t]*(else:|except(\ +.*)?:|finally:|elif\ +.*:)"
      "Regular expression matching statements to be dedented one level.")

(defvar python-indent-stmt-re
      (concat
       "[ \t]*(if\ +.*:|def\ +.*:$|try:|for\ +.*:|class\ +.*(\(.*\))?:)"
       "|"
       python-dedent-stmt-re)
      "Regular expression matching statements that start indented block")

(defvar python-context-tokens
;; standard arithmetical expressions
  '(("[\[\{\(]" 'expression (lambda (token-pos)
			      (let ((pos (condition-case nil
				  (find-matching-bracket token-pos)
				  (error))))
				(when pos
				  (forward-char 1 pos)))))
;; special Python long string literal (DQTQ)
    ("\"\"\"" 'long-dqtq (lambda (token-pos)
			   (let ((pos (re-search-forward
				       "\"\"\""
				       (forward-char 3 token-pos))))
			     (when pos
			       (match-end)))))
;; special Python long string literal (SQTQ)
    ("\'\'\'" 'long-sqtq (lambda (token-pos)
			   (let ((pos (re-search-forward
				       "\'\'\'"
				       (forward-char 3 token-pos))))
			     (when pos
			       (match-end)))))
;; basic doublequoted string
;; XXX: we've got to handle escaped \" and \''s somehow
    ("\"" 'short-dq (lambda (token-pos)
			   (let ((pos (re-search-forward
				       "\""
				       (forward-char 1 token-pos))))
			     (when pos
			       (match-end)))))
;; basic singlequoted string
    ("\'" 'short-sq (lambda (token-pos)
			   (let ((pos (re-search-forward
				       "\'"
				       (forward-char 1 token-pos))))
			     (when pos
			       (match-end)))))
    ))

(defvar python-context-open-re
  (mapconcat (lambda (x) (car x))
	     python-context-tokens "|")
  "Regexp used to detect syntactic context")

(defvar python-current-context-start nil
  "Cached upper boundary of syntactic context")
(make-variable-buffer-local 'python-current-context-start)

(defvar python-current-context-end nil
  "Cached lower boundary of sntactic context")
(make-variable-buffer-local 'python-current-context-end)

(defvar python-current-syntactic-context nil
  "Cached value of the last `python-syntactic-context' call")
(make-variable-buffer-local 'python-current-syntactic-context)

(defun python-syntactic-context (p)
  "1\) scan current buffer to determine syntactic context of position `p'\n\
   2\) store context info in appropriate cache variables"
  (letrec
      ((iter
	(lambda (upper context)
	  (if (< upper p)
	      (let 
		  ((token-pos
		    (re-search-forward python-context-open-re upper))
		   start end current-context)
		(when token-pos
		  (setq start (match-end)))
		(cond
		 ((or 
		   (null token-pos)
		   (> token-pos p))
		  context)
		 (t
		  (setq end 
			(letrec
			    ((iter
			      (lambda (x)
				(cond
				 ((null x) 
					(error "Unknown context found"))
				 ((looking-at (caar x) token-pos)
				  (setq current-context (eval (cadar x)))
				  (funcall (eval (caddar x)) token-pos))
				 (t 
				  (iter (cdr x)))))))
			  (iter python-context-tokens)))
		  (cond
		   ((null end)
		    (setq python-current-context-start start)
		    (setq python-current-context-end nil)
		    (iter start current-context))
		   ((< end p)
		    (iter end context))
		   (t (setq python-current-context-start start)
		      (setq python-current-context-end end)
		      (iter start current-context))))))
	    context))))
    (setq python-current-syntactic-context (iter (start-of-buffer) nil))))

(defun python-indent-line (#!optional p)
  "Indent line at position `p' or at cursor position."
  (interactive)
  (unless p
    (setq p (cursor-pos)))
  (if
      (python-syntactic-context (start-of-line p))
    (cond
     ((= python-current-syntactic-context 'expression)
      (set-indent-pos 
       (pos
	(pos-col python-current-context-start)
	(pos-line p))))
     (t 
      nil))
    (set-indent-pos (python-indent-pos p))))
     
(defun python-backward-stmt (#!optional number p)
  "Return buffer position of NUMBER's previous Python statement"
  (unless number
    (setq number 1))
  (unless p
    (setq p (cursor-pos)))
  (letrec 
      ((iter (lambda (x safe)
	       (cond       
		((or
		  (null x)
		  (= x (start-of-buffer))
		  (>= x safe))
		 (start-of-buffer))
		((not
		  (looking-at python-blank-or-comment-re (start-of-line x)))
		 (if (python-syntactic-context (start-of-line x))
		     (iter python-current-context-start x)
		   (setq number (1- number))
		   (if (> number 0)
		       (iter (backward-line 1 x) x)
		     (start-of-line x))))
		(t (iter (backward-line 1 x) x))))))
    (iter (backward-line 1 p) p)))

(defun python-indent-pos (p)
  "Determine (hopefully) correct indentation for line pointed by pos"
  (let*
      ((prev-stmt-pos (python-backward-stmt 1 p))
       (base-indent (indent-pos prev-stmt-pos)))
    (cond
     ((looking-at python-indent-stmt-re prev-stmt-pos)
      (pos (+                                      
	    (pos-col base-indent)
	    python-indent-step)
	   (pos-line p)))
     ((looking-at python-dedent-stmt-re (pos 0 (pos-line p)))
      (pos (-
	    (pos-col base-indent)
	    python-indent-step)
	   (pos-line p)))
     (t (pos
	 (pos-col base-indent)
	 (pos-line p))))))
	  
(defun python-modify-indent (p delta)
  (unless p
    (setq p (cursor-pos)))
  (set-indent-pos (let
		      ((old-indent (indent-pos p)))
		    (pos
		     (+
		      (pos-col old-indent)
		      (* delta python-indent-step))
		     (pos-line old-indent)))))

(defun python-incr-indent (#!optional p)
  "Indent `p' or cursor line"
  (interactive)
  (python-modify-indent p +1))

(defun python-decr-indent (#!optional p)
  "Dedent `p' or cursor line"
  (interactive)
  (python-modify-indent p -1))

(defun python-ret ()
  (interactive)
  (insert "\n")
  (python-indent-line))

(defun python-colon ()
  (interactive)
  (insert ":")
  (when
      (looking-at python-dedent-stmt-re (start-of-line))
    (python-indent-line)))

(defvar python-mode-keymap
  (bind-keys (make-sparse-keymap)
    ":" 'python-colon
    "RET" 'python-ret
    "TAB" 'indent-line))

(defvar python-mode-ctrl-c-keymap
  (bind-keys (make-sparse-keymap)
    "<" 'python-decr-indent
    ">" 'python-incr-indent))

;;;###autoload
(defun python-mode ()
  "Python Mode:\n
Simple mode for editing Python source code. Its main feature is to be able to
indent lines to their (probably) correct depth.\n
Commands defined by this mode are:\n
\\{python-mode-keymap}\\{python-mode-ctrl-c-keymap,Ctrl-c}"
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "Python"
	major-mode 'python-mode
	major-mode-kill kill-all-local-variables
;;	mode-comment-fun c-insert-comment
	mode-indent-line python-indent-line
;;	mode-forward-exp c-forward-exp
;;	mode-backward-exp c-backward-exp
	mode-defun-header "^[ \t]*def\ +([a-zA-Z_])\ ?(\(.*\))?:[ \t]*"
;;	mode-defun-footer "^}"
	paragraph-separate "^[\n\t\f ]*\n"
	paragraph-start paragraph-separate
	local-ctrl-c-keymap python-mode-ctrl-c-keymap
	local-keymap python-mode-keymap
	indent-tabs-mode nil)
  (make-local-variable 'info-documentation-files)
  (setq info-documentation-files '("python"))
  (call-hook 'python-mode-hook))
