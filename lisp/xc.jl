;;;; xc-mode.jl -- new indentation for C mode

;;; Needless to say this doesn't work 100% yet :-{
;;; Require this file (`xc') to use it.

(provide 'xc)


;; User options to define the indentation style

;; Example settings:
;;                   BSD  GNU  K&R
;; c-body-indent      4    2    5
;; c-brace-indent    -4    0   -5
;; c-case-indent     -4   -2   -5
;; c-label-indent    -4   -2   -5

(defvar c-body-indent 4
  "Indentation of code with respect to its containing block.")
(defvar c-brace-indent -4
  "Extra indentation of braces relative to the body of the code they
contain.")
(defvar c-case-indent -4
  "Extra indentation for case statements.")
(defvar c-label-indent -4
  "Extra indentation for labels.")


;; Installation; hooks into c-mode, installing its own indentation function
(add-hook 'c-mode-hook #'(lambda ()
			   (setq mode-indent-line 'xc-indent-line
				 mode-name "XC")
			   (eval-hook 'xc-mode-hook)
			   nil))


;; The business

(defun xc-indent-line (&optional pos)
  "Indent the line at POS (or the cursor) assuming that it's C source code."
  (set-indent-pos (xc-indent-pos pos)))

;; Attempt to find the previous statement
(defun xc-backward-stmt (pos)
  (let*
      (stmt-pos
       back-1-pos)
    (error-protect
	(while (setq pos (c-backward-exp 1 pos t))
	  (cond
	   ((null back-1-pos)
	    (setq back-1-pos pos))
	   ((/= (pos-line back-1-pos) (pos-line pos))
	    ;; Gone past the start of this line
	    (error "Ignored")))		; break the loop
	  (setq stmt-pos pos))
      (error))
    stmt-pos))

;; POS should point to an `else' keyword, the position of it's matching `if'
;; will be returned.
(defun xc-balance-ifs (pos &optional depth)
  (unless depth
    (setq depth 1))
  (while (and (/= depth 0)
	      (setq pos (xc-backward-stmt pos)))
    (cond
     ((and (looking-at "else[\t ]*" pos)
	   (not (looking-at "[\t ]*if[\t ]*\\(" (match-end))))
      (setq depth (1+ depth)))
     ((looking-at "if" pos)
      (setq depth (1- depth)))))
  (when (zerop depth)
    pos))

;; Work out where to indent LINE-POS to.
(defun xc-indent-pos (&optional line-pos)
  (setq line-pos (if line-pos
		     (line-start line-pos)
		   (line-start)))
  ;; Check for cpp op
  (if (regexp-match-line "^[\t ]*#" line-pos)
      (pos 0 (pos-line line-pos))
    (let*
	((pos (copy-pos line-pos))
	 (exp-pos (xc-backward-stmt pos))
	 exp-ind)
      ;; Find the beginning of the expression to indent relative to
      (unless exp-pos
	;; Start of the containing expression
	(when (find-prev-regexp "[\{\(]" pos)
	  (setq exp-pos (match-start))))
      (setq exp-ind (char-to-glyph-pos exp-pos))
      (unless (equal (indent-pos exp-pos) exp-ind)
	(when (and (regexp-match-line "^[\t ]*([^][(){}\"'a-zA-Z0-9_\t ]+)"
				      (line-start exp-pos))
		   (< (match-start 1) exp-pos))
	  ;; Back up over the bits of punctuation
	  (setq exp-ind (char-to-glyph-pos (match-start 1)))))
      ;; First look at previous line and see how it affects the one we're
      ;; trying to indent
      (cond
       ((= (get-char exp-pos) ?\})
	(unless (zerop (pos-col exp-pos))
	  (left-char (+ c-body-indent c-brace-indent) exp-ind)))
       ((looking-at ".*{" exp-pos)
	(setq exp-ind (right-char c-body-indent (indent-pos exp-pos))))
       ((looking-at "(if|for|while|switch)[\t ]*\\(.*$|(else|do)([^a-zA-Z0-9_]|$)"
		    exp-pos)
	(right-char c-body-indent exp-ind))
       ((= (get-char exp-pos) ?\}))
       ((looking-at ".*\;" exp-pos)
	(let
	    ((prev (xc-backward-stmt exp-pos)))
	  ;; *Need to loop here searching back to the correct level*
	  (when (and prev (/= (pos-col prev) (pos-col exp-pos))
		     (not (looking-at "case .*:|default[\t ]*:|.*;" prev)))
	    ;; A continuation?
	    (when (and (looking-at "else[\t ]*" prev)
		       (not (looking-at "[\t ]*if[\t ]*\\(" (match-end))))
	      (unless (setq prev (xc-balance-ifs prev))
		(error "Beginning of buffer"))
	      (let
		  ((tmp (xc-backward-stmt prev)))
		(while (and tmp (looking-at "if[\t ]*\\(" tmp))
		  (setq prev tmp)
		  (unless (setq tmp (xc-backward-stmt tmp))
		    (error "Beginning of buffer")))))
	    (set-pos-col exp-ind (pos-col (char-to-glyph-pos prev))))))
       ((looking-at "case .*:|default[\t ]*:" exp-pos)
	(left-char c-case-indent exp-ind))
       ((looking-at "[a-zA-Z_][a-zA-Z0-9_]+:([\t ]|$)" exp-pos)
	(unless (left-char c-label-indent exp-ind)
	  (setq exp-ind (pos 0 (pos-line exp-pos))))))
      ;; Next, look at the contents of this line and see if it needs any
      ;; special treatment
      (unless (empty-line-p line-pos)
	(when (regexp-match-line "^[\t\f ]+" line-pos)
	  (setq line-pos (match-end)))
	(cond
	 ((= (get-char line-pos) ?\{)
	  (if (< (pos-col exp-ind) (- c-brace-indent))
	      (set-pos-col exp-ind 0)
	    (right-char c-brace-indent exp-ind)))
	 ((= (get-char line-pos) ?\})
	  (left-char c-body-indent exp-ind))
	 ((looking-at "case .*:|default[\t ]*:" line-pos)
	  (right-char c-case-indent exp-ind))
	 ((looking-at "[a-zA-Z_]+[a-zA-Z0-9_]*:([\t ]|$)" line-pos)
	  (right-char c-label-indent exp-ind))))
      (set-pos-line exp-ind (pos-line line-pos))
      exp-ind)))
