;;;; perl-mode.jl -- Major mode for editing Perl source
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

(require 'c-mode)
(provide 'perl-mode)

;; Commentary:
;;
;; This borrows a lot of code from c-mode, sometimes simply by using
;; the c- prefixed functions, sometimes by reimplementing with small
;; changes. See c-mode.jl for more comments.


;; Configuration

(defvar perl-body-indent 4
  "Indentation of Perl code with respect to its containing block.")
(make-variable-buffer-local 'perl-body-indent)

(defvar perl-brace-indent -4
  "Extra indentation of braces in relative to the body of the Perl code they
contain.")
(make-variable-buffer-local 'perl-brace-indent)

(defvar perl-label-indent -4
  "Extra indentation for labels in Perl code.")
(make-variable-buffer-local 'perl-label-indent)


;; Code

(defvar perl-mode-keymap
  (bind-keys (make-sparse-keymap)
    "{" 'c-open-brace
    "}" 'c-close-brace
    ":" 'c-colon
    "TAB" 'indent-line))

;;;###autoload
(defun perl-mode ()
  "Perl Mode:\n
Major mode for editing Perl source code. Local keybindings are:\n
\\{perl-mode-keymap}"
  (interactive)
  (and major-mode-kill (funcall major-mode-kill))
  (setq mode-name "Perl"
	major-mode 'perl-mode
	major-mode-kill 'kill-all-local-variables
	mode-comment-header "#"
	mode-indent-line 'perl-indent-line
	mode-forward-exp 'c-forward-exp
	mode-backward-exp 'c-backward-exp
	mode-defun-header "^sub ([a-zA-Z0-9_]+)[\t ]*{"
	mode-defun-footer "^}"
	paragraph-separate "^[\n\t\f ]*\n"
	paragraph-start paragraph-separate
	keymap-path (cons 'perl-mode-keymap keymap-path))
  (call-hook 'perl-mode-hook))

(defun perl-indent-line (&optional pos)
  (let*
      ((line-pos (start-of-line pos))
       (exp-pos (c-backward-stmt line-pos))
       exp-ind)

    ;; Find the beginning of the expression to indent relative to
    (unless exp-pos
      ;; Start of the containing expression
      (when (re-search-backward "[\{\(\[]" line-pos)
	(setq exp-pos (match-start))))
    (setq exp-ind (char-to-glyph-pos exp-pos))

    (unless (or (equal (indent-pos exp-pos) exp-ind)
		(memq (get-char (forward-char -1 exp-pos)) '(?\( ?\{ ?\[)))
      (when (and (looking-at "^[\t ]*([^][(){}$@\"'a-zA-Z0-9_\t ]+)"
			     (start-of-line exp-pos))
		 (< (match-start 1) exp-pos))
	;; Back up over the bits of punctuation
	(setq exp-ind (char-to-glyph-pos (match-start 1)))))

    ;; First look at previous line and see how it affects the one we're
    ;; trying to indent
    (cond
     ((= (get-char exp-pos) ?\})
      ;; A closing brace
      (unless (zerop (pos-col exp-pos))
	(setq exp-ind (left-char (+ perl-body-indent
				    perl-brace-indent) exp-ind))))

     ((looking-at ".*{" exp-pos)
      ;; An opening brace
      (setq exp-ind (right-char perl-body-indent (indent-pos exp-pos))))

     ((looking-at ".*\;" exp-pos)
      ;; A full expression, indent to the level of the first
      ;; line in the expression
      (let
	  ((prev (c-backward-stmt exp-pos)))
	;; *Need to loop here searching back to the correct level*
	(when (and prev (/= (pos-col prev) (pos-col exp-pos))
		   (not (looking-at "[a-zA-Z_][a-zA-Z0-9_]+:|.*;" prev)))
	  (setq exp-ind (pos (pos-col (char-to-glyph-pos prev))
			     (pos-line exp-ind))))))
     
     ((looking-at "[a-zA-Z_][a-zA-Z0-9_]+:([\t ]|$)" exp-pos)
      ;; A goto label, indented back by perl-label-indent
      (setq exp-ind (or (left-char perl-label-indent exp-ind)
			(pos 0 (pos-line exp-pos))))))

    ;; Next, look at the contents of this line and see if it needs any
    ;; special treatment
    (unless (empty-line-p line-pos)
      ;; Skip leading whitespace
      (when (looking-at "^[\t\f ]+" line-pos)
	(setq line-pos (match-end)))

      (cond
       ((= (get-char line-pos) ?\{)
	;; An opening brace at the start of the line, indent back by
	;; perl-brace-indent
	(setq exp-ind (pos (max 0 (+ (pos-col exp-ind) perl-brace-indent)))))

       ((= (get-char line-pos) ?\})
	;; A closing brace, indent outwards by perl-brace-indent
	(setq exp-ind (left-char perl-body-indent exp-ind)))

       ((looking-at "[a-zA-Z_]+[a-zA-Z0-9_]*:([\t ]|$)" line-pos)
	;; A goto label
	(setq exp-ind (right-char perl-label-indent exp-ind)))))

    ;; Finished
    (set-indent-pos (pos (pos-col exp-ind) (pos-line line-pos)))))
