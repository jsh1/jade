;;;; isearch.jl -- Emacs style incremental search
;;;  Copyright (C) 1994 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'isearch)

;;; Bugs:
;;; Doesn't un-wrap itself when backtracking

(defvar case-fold-search t
  "Buffer-local variable, when non-nil case of characters is ignored when
searching.")
(make-variable-buffer-local 'case-fold-search)

(defvar isearch-last-match nil
  "The last regexp successfully (i.e. entered with RET) found by isearch.")

;; Special vars
(defvar isearch-trace nil)
(defvar isearch-forwards nil)
(defvar isearch-failing nil)
(defvar isearch-wrapped nil)
(defvar isearch-initial-pos nil)
(defvar isearch-re-error nil)


;;; Wrappers for some regexp functions to trap errors

(defun isearch-find-next-regexp (pos)
  (setq isearch-re-error nil)
  (condition-case nil
      (re-search-forward (car (car isearch-trace)) pos nil case-fold-search)
    (regexp-error
      (setq isearch-re-error t)
      'regexp-error)))

(defun isearch-find-prev-regexp (pos)
  (setq isearch-re-error nil)
  (condition-case nil
      (re-search-backward (car (car isearch-trace)) pos nil case-fold-search)
    (regexp-error
      (setq isearch-re-error t)
      'regexp-error)))

(defun isearch-looking-at ()
  (setq isearch-re-error nil)
  (condition-case nil
      (looking-at (car (car isearch-trace)) nil nil case-fold-search)
    (regexp-error
      (setq isearch-re-error t)
      'regexp-error)))


;;; I-search stack handling. The variable `isearch-trace' holds a stack of
;;; all strings and match positions in the current search. It looks
;;; something like,
;;;   ((SEARCH-REGEXP [MATCH-POS]... )... )
;;; But although the current string will be on the stack, the current match
;;; position won't -- the cursor position is used to show this.

;; Pops the top match and moves the cursor to it.
(defun isearch-pop-match ()
  (let
      ((item (car isearch-trace)))
    (if (cdr item)
	(progn
	  (goto (nth 1 item))
	  (rplaca isearch-trace (cons (car item) (nthcdr 2 item))))
      (goto isearch-initial-pos)))
  (setq isearch-failing nil))

;; Pops the top string, then moves to the top match of the next string.
(defun isearch-pop-string ()
  (when (cdr isearch-trace)
    (setq isearch-trace (cdr isearch-trace))
    (isearch-pop-match)))

;; Pushes a match at POS onto the top string
(defun isearch-push-match (pos)
  (let
      ((item (car isearch-trace)))
    (rplaca isearch-trace (cons (car item) (cons pos (cdr item))))))

;; Pushes the current position, pushes the STRING onto the top of the
;; stack, then searches for it
(defun isearch-push-string (string)
  (isearch-push-match (cursor-pos))
  (setq isearch-trace (cons (cons string nil) isearch-trace))
  (unless (isearch-looking-at)
    (let
	((next (if isearch-forwards
		   (isearch-find-next-regexp (forward-char))
		 (isearch-find-prev-regexp (forward-char -1)))))
      (cond
	((posp next)
	  (goto next)
	  (setq isearch-failing nil))
	((null next)
	  (setq isearch-failing t)
	  (beep))))))


;; Keymap

(defvar isearch-keymap
  (bind-keys (make-sparse-keymap)
    "Ctrl-s"	'isearch-next-forward
    "Ctrl-r"	'isearch-next-backward
    "Ctrl-g"	'isearch-cancel
    "Ctrl-q"	'(progn (next-keymap-path nil) (isearch-title))
    "Ctrl-w"	'isearch-yank-word
    "Ctrl-y"	'isearch-yank-line
    "RET"	'isearch-accept
    "ESC"	'isearch-accept
    "BS"	'isearch-rubout))


;; Display the status message, has to be done after each command
(defun isearch-title ()
  (let
      ((msg (concat (if isearch-failing "failing ")
		    (if isearch-wrapped "wrapped ")
		    "I-search: "
		    (car (car isearch-trace))
		    (if isearch-re-error "    *Invalid/incomplete regexp*"))))
    (aset msg 0 (char-upcase (aref msg 0)))
    (message msg)))

;; Accept our current position
(defun isearch-accept ()
  (interactive)
  (set-auto-mark isearch-initial-pos)
  (setq isearch-last-match (car (car isearch-trace)))
  (throw 'isearch (cursor-pos)))

;; Cancel the search or if search is failing backtrack to the last match
(defun isearch-cancel ()
  (interactive)
  (if (not (isearch-looking-at))
      (progn
	(while (and (not (isearch-looking-at)) (cdr isearch-trace))
	  (isearch-rubout))
	(isearch-title))
    (goto isearch-initial-pos)
    (throw 'isearch nil)))

;; Copy the rest of the current word to the search string
(defun isearch-yank-word ()
  (interactive)
  (when (isearch-looking-at)
    (isearch-push-string (concat (car (car isearch-trace))
				 (copy-area (match-end)
					    (forward-word 1 (match-end))))))
  (isearch-title))

;; Copy the rest of the line to the search string
(defun isearch-yank-line ()
  (interactive)
  (when (isearch-looking-at)
    (isearch-push-string (concat (car (car isearch-trace))
				 (copy-area (match-end) (end-of-line)))))
  (isearch-title))

;; Backup one match/string
(defun isearch-rubout ()
  (interactive)
  (cond
    ((cdr (car isearch-trace))
      (isearch-pop-match))
    ((cdr isearch-trace)
      (isearch-pop-string))
    (t
      (error "Beginning of I-search")))
  (isearch-title))

;; Add the typed character to the search string
(defun isearch-unbound-key-fun ()
  (let
      ((str (current-event-string)))
    (if (/= (length str) 1)
        (isearch-accept)
      (isearch-push-string (concat (car (car isearch-trace)) str))))
  (isearch-title))

(defun isearch (isearch-forwards)
  (let
      ((isearch-trace (cons (cons "" nil) nil))
       (isearch-initial-pos (cursor-pos))
       (isearch-buffer (current-buffer))
       isearch-failing
       isearch-wrapped
       isearch-re-error
       (esc-means-meta nil)		; want to bind to ESC
       (old-kp keymap-path))
    (setq keymap-path '(isearch-keymap))
    (add-hook 'unbound-key-hook 'isearch-unbound-key-fun)
    (unwind-protect
	(catch 'isearch
	  (isearch-title)
	  (recursive-edit))
      (with-buffer isearch-buffer
	(remove-hook 'unbound-key-hook 'isearch-unbound-key-fun)
	(setq keymap-path old-kp)))))

;;;###autoload
(defun isearch-forward ()
  "Enters the incremental search mode. You are then able to type letters to add
to the regexp being searched for. Special commands are,\n
  `Ctrl-s'     Search forwards for another occurrence
  `Ctrl-r'     Search backwards 
  `Ctrl-g'     If the search is failing, backtrack to the first non-failing
               match, else, cancel the search leaving the cursor at its
               original position.
  `Ctrl-w'     Copy the rest of the current word into the search string
  `Ctrl-y'     Copy the rest of the line to the search string
  `RET'        Exit isearch leaving the cursor at its current position
  `BS'         Retrace your movements one step"
  (interactive)
  (isearch t))

;;;###autoload
(defun isearch-backward ()
  "Similar to `isearch-forward' except the first searching is done in the other
direction."
  (interactive)
  (isearch nil))

(defun isearch-next-forward ()
  (interactive)
  (if (and (equal (car (car isearch-trace)) "") isearch-last-match)
      (progn
	(isearch-push-string isearch-last-match)
	(isearch-title))
    (let
	((next (if (and isearch-failing isearch-forwards)
	           (progn
		     (setq isearch-wrapped t)
		     (start-of-buffer))
		 (forward-char))))
      (setq next (isearch-find-next-regexp next))
      (cond
        ((posp next)
	  (isearch-push-match (cursor-pos))
	  (setq isearch-failing nil)
	  (goto next))
	((null next)
	  (setq isearch-failing t)
	  (beep)))
      (setq isearch-forwards t)
      (isearch-title))))

(defun isearch-next-backward ()
  (interactive)
  (if (and (equal (car (car isearch-trace)) "") isearch-last-match)
      (progn
	(isearch-push-string isearch-last-match)
	(isearch-title))
    (let
	((next (if (and isearch-failing (not isearch-forwards))
		   (progn
		     (setq isearch-wrapped t)
		     (end-of-buffer))
		 (forward-char -1))))
      (setq next (isearch-find-prev-regexp next))
      (cond
       ((posp next)
	(isearch-push-match (cursor-pos))
	(setq isearch-failing nil)
	(goto next))
       ((null next)
	(setq isearch-failing t)
	(beep)))
      (setq isearch-forwards nil)
      (isearch-title))))
