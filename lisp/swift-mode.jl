#| swift-mode.jl -- Major mode for editing Swift source

   Copyright (C) 1993-2015 John Harper <jsh@unfactored.org>

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(require 'c-mode)
(provide 'swift-mode)

(defvar swift-indent-width 4
  "Indentation of code with respect to its containing block.")

(make-variable-buffer-local 'swift-indent-width)

;;;###autoload
(defun swift-mode ()
  "Swift Mode:\n
Major mode for editing Swift source code. Local keybindings are:\n
\\{c-mode-keymap}"
  (interactive)
  (and major-mode-kill (major-mode-kill))
  (set! mode-name "Swift")
  (set! major-mode 'swift-mode)
  (set! major-mode-kill kill-all-local-variables)
  (set! mode-comment-header "// ")
  (set! mode-indent-line swift-indent-line)
  (set! mode-forward-exp c-forward-exp)
  (set! mode-backward-exp c-backward-exp)
  (set! mode-defun-header
	"^\\s*(static\\s+|public\\s+|mutating\\s+)*(func|init)\\b")
  (set! mode-defun-footer c-end-of-defun)
  (set! paragraph-separate "^[\n\t\f ]*\n")
  (set! paragraph-start paragraph-separate)
  (set! local-ctrl-c-keymap c-mode-ctrl-c-keymap)
  (set! local-keymap c-mode-keymap)
  (set! indent-tabs-mode nil)
  (call-hook 'swift-mode-hook))

(defun swift-indent-line (#!optional p)
  "Indent the line at POS (or the cursor) assuming that it's C source code."
  (set-indent-pos (swift-indent-pos p)))

;; POS should point to an `else' keyword, the position of it's matching `if'
;; will be returned.
(defun swift-balance-ifs (p #!optional depth)
  (unless depth
    (set! depth 1))
  (let loop ()
    (when (/= depth 0)
      (set! p (c-backward-stmt p t))
      (when p
	(cond
	 ((and (looking-at "else[\t ]*" p)
	       (not (looking-at "\\bif\\b" (match-end))))
	  (set! depth (1+ depth)))
	 ((looking-at "if" p)
	  (set! depth (1- depth))))
	(loop))))
  (when (zero? depth)
    p))

(define swift-open-body-chars '(#\( #\{ #\[))
(define swift-indent-body-chars '(#\{ #\[))
(define swift-close-body-chars '(#\) #\} #\]))

(define swift-stmt-re "[{([]")

(define swift-indenting-stmt-re
  "\\b(if|else|switch|guard|for|while|repeat|do|catch)\\b|else([^a-zA-Z0-9_]|$)")

(define swift-case-stmt-re "case .*:|default[\t ]*:")

;; Work out where to indent LINE-POS to.
(defun swift-indent-pos (#!optional line-pos)
  (set! line-pos (start-of-line line-pos))

  (let (inside-stmt exp-pos exp-ind)

    (set! exp-pos (c-backward-stmt line-pos))

    (let* ((stmt (c-backward-stmt line-pos))
	   (tem (c-skip-backward-whitespace-comment (or stmt line-pos))))
      ;; parenthesized lists still indent flush to start, unless
      ;; opening paren is at end of line.
      (if (and tem (or (null? stmt)
		       (memq (get-char tem) swift-indent-body-chars)))
	  (let ((start (c-backward-stmt tem)))
	    (when start
	      (set! tem start)
	      (set! inside-stmt t)))
	(set! tem stmt))
      (set! exp-pos (or tem (start-of-buffer))))

    (set! exp-ind (char-to-glyph-pos exp-pos))

    (unless (or (equal? (indent-pos exp-pos) exp-ind)
		(memq (get-char (forward-char -1 exp-pos))
		      swift-open-body-chars))
      (when (and (looking-at "^[\t ]*([^][(){}\"'a-zA-Z0-9_\t ]+)"
			     (start-of-line exp-pos))
		 (< (match-start 1) exp-pos))
	;; Back up over the bits of punctuation
	(set! exp-ind (char-to-glyph-pos (match-start 1)))))

    (when inside-stmt
      (set! exp-ind (right-char swift-indent-width exp-ind)))

    ;; First look at previous line and see how it affects the one we're
    ;; trying to indent
    (cond
     ((and (not inside-stmt) (looking-at ".*{" exp-pos))
      ;; An opening brace. FIXME: still needed?
      (set! exp-ind (right-char swift-indent-width (indent-pos exp-pos))))

     ((and (not inside-stmt) (looking-at swift-indenting-stmt-re exp-pos))
      ;; Something that causes the next statement to be indented
      (set! exp-ind (right-char swift-indent-width exp-ind)))

     ((looking-at swift-case-stmt-re exp-pos)
      ;; A switch-statement label
      (set! exp-ind (right-char swift-indent-width exp-ind))))

    ;; Next, look at the contents of this line and see if it needs any
    ;; special treatment
    (unless (empty-line-p line-pos)
      ;; Skip leading whitespace
      (when (looking-at "^[\t\f ]+" line-pos)
	(set! line-pos (match-end)))

      (cond
       ((memq (get-char line-pos) swift-open-body-chars)
	;; An opening brace at the start of the line, indent back by
	;; -swift-indent-width
	(set! exp-ind (pos (max 0 (- (pos-col exp-ind) swift-indent-width)))))

       ((memq (get-char line-pos) swift-close-body-chars)
	;; A closing brace, indent inwards by swift-indent-width
	(set! exp-ind (left-char swift-indent-width exp-ind)))

       ((looking-at swift-case-stmt-re line-pos)
	;; A switch label
	(set! exp-ind (left-char swift-indent-width exp-ind)))))

    ;; Finished
    (pos (pos-col exp-ind) (pos-line line-pos))))
