;;;; modes.jl -- Code for handling editing modes.
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


;;; Notes:

;;; Major Modes:
;;;
;;; Each major mode has a function which is called to install the mode,
;;; the first thing such a function should do is check if a mode is
;;; already installed and if so remove it:
;;;
;;;  (when major-mode-kill
;;;    (funcall major-mode-kill))
;;;
;;; Now the new mode is free to install itself; generally this entails
;;; setting at least the `mode-name' and `major-mode-kill' variables
;;; and installing a local keymap. For example `lisp-mode' does this:
;;;
;;;   (setq mode-name "Lisp"
;;;	    major-mode 'lisp-mode
;;;	    major-mode-kill 'lisp-mode-kill
;;;	    mode-comment-fun 'lisp-mode-insert-comment
;;;	    keymap-path (cons 'lisp-mode-keymap keymap-path))
;;;   (eval-hook 'lisp-mode-hook)
;;;
;;; The function to be called when the mode is removed should remove the
;;; effects of the above, for example:
;;;
;;;   (setq keymap-path (delq 'lisp-mode-keymap keymap-path)
;;;	    major-mode nil
;;;	    major-mode-kill nil
;;;	    mode-comment-fun nil
;;;	    mode-name nil)

;;; Minor Modes:
;;;
;;; These are usually harder to implement than major modes since they
;;; have to coexist with all other modes which are installed.
;;;
;;; Generally each minor mode maintains a buffer-local variable saying
;;; whether or not it's installed in the buffer. The minor mode's
;;; function usually toggles the mode on or off depending on the state of
;;; this variable.
;;;
;;; There are two functions which *must* be used to install/remove a
;;; minor mode -- `add-minor-mode' and `remove-minor-mode', see there
;;; documentation for details.
;;;
;;; Each buffer has a keymap for the bindings of all the minor modes
;;; active in the buffer (called `minor-mode-keymap'). These bindings
;;; have to be added when the mode is enabled and removed when it
;;; is disabled.


(defvar mode-alist '(
;;; ::mode-alist-start::
  ("\\.(c|h)$|^c(|-mode)$" . c-mode)
  ("\\.jl$|^.jaderc$|^lisp(|-mode)$" . lisp-mode)
  ("\\.(te?xt|doc|article|letter)$" . text-mode)
  ("^(text(|-mode)|(.*/|)draft)$" . text-mode)
  ("^indented-text(|-mode)$" . indented-text-mode)
  ("\\.[s]$|^asm(|-mode)$" . asm-mode)
  ("\\.[S]$|^asm-cpp(|-mode)$" . asm-cpp-mode)
  ("\\.texi(|nfo)|^texinfo(|-mode)$" . texinfo-mode)
  ("\\.tex$|^(La)?TeX$" . tex-mode)
;;; ::mode-alist-end::
  ) "List of all major modes which can be enabled by loading a file into
a buffer. List is made of `(REGEXP . MODE)' cells; the REGEXP is matched
against the mode specification (i.e. the filename), if it matches the
function MODE is called to install the mode.")


;; Variables

(defvar major-mode nil
  "The function which was used to initialise the buffer's major mode.")
(make-variable-buffer-local 'major-mode)

(defvar major-mode-kill nil
  "The function which should be called to remove the buffer's major mode.")
(make-variable-buffer-local 'major-mode-kill)

(defvar comment-column 41
  "Buffer-local variable containing the canonical column number which
comments should begin at. If the line extends past this column the next
tab stop after the end of the line is used instead.")
(make-variable-buffer-local 'comment-column)

(make-variable-buffer-local 'mode-comment-fun)

(defvar mode-indent-line nil
  "A function called to indent a specified line in the current buffer.")
(make-variable-buffer-local 'mode-indent-line)

(defvar mode-forward-exp nil
  "Function like `lisp-forward-sexp'.")
(make-variable-buffer-local 'mode-forward-exp)

(defvar mode-backward-exp nil
  "Function like `lisp-backward-sexp'.")
(make-variable-buffer-local 'mode-backward-exp)


;; Major mode handling

(defun get-mode (name)
  "Scan the alist `mode-alist' for a mode whose regexp matches NAME,
returning the initialisation function of that mode (a symbol) or nil."
  (let*
      ((list mode-alist)
       (elt nil))
    (while (setq elt (car list))
      (when (regexp-match (car elt) name t)
	(return (cdr elt)))
      (setq list (cdr list)))))

(defun init-mode (buf &optional name)
  "Initialise an edit-mode for buffer BUF, either calls the function named
in the buffer-local variable `major-mode' or finds a mode in `mode-alist'
using one of the following to match against:
  1. NAME
  2. The word specified on the first line of the buffer surrounded by
     `-*-...-*-' (ie, -*-texinfo-*-)
  3. The value of the variable `mode-name'
  4. The name of the file being edited in the buffer"
  (with-buffer (unless buf (current-buffer))
    (unless major-mode
      (setq name (or name
		     (regexp-expand-line "-\\*- *([^ ]+) *-\\*-" "\\1"
					 (buffer-start))
		     mode-name
		     (buffer-file-name buf)))
      (setq major-mode (get-mode name)))
    (when (functionp major-mode)
      (funcall major-mode name))))

(defun kill-mode (&optional buf)
  "Destroy the mode being used to edit buffer BUF with."
  (unless buf
    (setq buf (current-buffer)))
  (with-buffer buf
    (when major-mode-kill
      (funcall major-mode-kill buf))))


;; Minor-mode handling

(defvar minor-mode-list ()
  "List of all minor-modes enabled in this buffer.")
(make-variable-buffer-local 'minor-mode-list)

(defvar minor-mode-keymap nil
  "Buffer-local keymap to be used by minor-modes. This is only created
the first time a minor mode calls `add-minor-mode' in the buffer.")
(make-variable-buffer-local 'minor-mode-keymap)

(defun add-minor-mode (mode name &optional no-keymap)
  "For use by minor-modes. MODE is the mode's function symbol. This sets up the
current buffer. All minor-modes should call this before doing anything drastic.
NAME is the string to be displayed in the status-line to show that the mode
is enabled."
  (when (memq mode minor-mode-list)
    (error "Minor mode already installed" mode))
  (setq minor-mode-list (cons mode minor-mode-list)
	minor-mode-names (cons name minor-mode-names))
  (unless (or minor-mode-keymap no-keymap)
    (setq minor-mode-keymap (make-keylist)
	  keymap-path (cons 'minor-mode-keymap keymap-path)))
  t)

(defun remove-minor-mode (mode name)
  "For use by minor-modes. MODE is the mode's function symbol. Removes MODE
from the current buffer."
  (setq minor-mode-list (delq mode minor-mode-list)
	minor-mode-names (delete name minor-mode-names))
  t)

(defun minor-mode-installed (mode)
  "Returns t if MODE is installed in the current buffer."
  (memq mode minor-mode-list))


;; Comment handling

(defun insert-comment ()
  "Insert comment delimeters on the current line, place the cursor where the
comment should be written. This may or not be defined by each major mode."
  (interactive)
  (if (and (boundp 'mode-comment-fun) mode-comment-fun)
      (funcall mode-comment-fun)
    (error "No defined method for inserting comments in this buffer")))

(defun find-comment-pos ()
  (let
      ((pos (glyph-to-char-pos (pos (1- comment-column) nil))))
    (goto-line-end)
    (if (>= (line-end) pos)
	(insert "\t")
      (indent-to (1- comment-column)))))


;; Indentation

(defun indent-area (start end)
  "Use the `mode-indent-line' function to indent each line between START and
END."
  (interactive "-m\nM")
  (setq start (line-start start)
	end (line-start end))
  (unless mode-indent-line
    (error "No method for indenting lines in this buffer"))
  (while (< start end)
    (funcall mode-indent-line start)
    (next-line 1 start)))

(defvar newline-and-indent ()
  "(newline-and-indent)
Insert a newline then either call this buffer's `mode-indent-line' function
or insert a tab."
  (interactive)
  (if (null mode-indent-line)
      (insert "\n\t")
    (insert "\n")
    (funcall mode-indent-line)))

(defun indent-line ()
  "Indent the current line."
  (interactive)
  (if mode-indent-line
      (let
	  ((pos (funcall mode-indent-line)))
	(when (and (posp pos) (< (char-to-glyph-pos (cursor-pos)) pos))
	  (goto-glyph pos))
	(when (> (glyph-to-char-pos pos) (line-end))
	  (goto-line-end)))
    (error "No method for indentation in this buffer.")))


;; Expressions

(defun forward-exp (&optional number)
  "Move forward NUMBER expressions."
  (interactive "p")
  (goto-char (funcall (or mode-forward-exp 'forward-word) number)))

(defun backward-exp (&optional number)
  "Move backwards NUMBER expressions."
  (interactive "p")
  (goto-char (funcall (or mode-backward-exp 'backward-word) number)))

(defun kill-exp (&optional number)
  "Kill the next NUMBER expressions."
  (interactive "p")
  (kill-area (cursor-pos) (funcall (or mode-forward-exp 'forward-word)
				   number)))

(defun backward-kill-exp (&optional number)
  "Kills from the start of this NUMBER'th previous expression to the cursor."
  (interactive "p")
  (kill-area (funcall (or mode-backward-exp 'backward-word) number)
	     (cursor-pos)))

(defun transpose-exps (count)
  "Move the expression before the cursor COUNT expressions forwards."
  (interactive "p")
  (transpose-items (or mode-forward-exp 'forward-word)
		   (or mode-backward-exp 'backward-word)
		   count))
