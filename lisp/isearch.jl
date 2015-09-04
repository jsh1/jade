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

(require 'prompt)
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

;; History of the current search
(defvar isearch-trace nil)
(make-variable-buffer-local 'isearch-trace)

;; Non-nil when searching forwards
(defvar isearch-forwards nil)
(make-variable-buffer-local 'isearch-forwards)

;; Non-nil when search is failing
(defvar isearch-failing nil)
(make-variable-buffer-local 'isearch-failing)

;; Non-nil when search has passed the end of the buffer
(defvar isearch-wrapped nil)
(make-variable-buffer-local 'isearch-wrapped)

;; The original position
(defvar isearch-initial-pos nil)
(make-variable-buffer-local 'isearch-initial-pos)

;; Non-nil when the regexp is malformed
(defvar isearch-re-error nil)
(make-variable-buffer-local 'isearch-re-error)

;; The view in which we're searching
(defvar isearch-view nil)
(make-variable-buffer-local 'isearch-view)

;; The old buffer that was displayed in the minibuffer
(defvar isearch-original-buffer nil)
(make-variable-buffer-local 'isearch-original-buffer)

;; The extent used to highlight the current selection
(defvar isearch-extent nil)
(make-variable-buffer-local 'isearch-extent)

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


;; Entry points

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


;;; Wrappers for some regexp functions to trap errors

(defun isearch-find-next-regexp (p)
  (setq isearch-re-error nil)
  (condition-case nil
      (re-search-forward (car (car isearch-trace)) p
			 (current-buffer isearch-view)
			 case-fold-search)
    (regexp-error
      (setq isearch-re-error t)
      'regexp-error)))

(defun isearch-find-prev-regexp (p)
  (setq isearch-re-error nil)
  (condition-case nil
      (re-search-backward (car (car isearch-trace)) p
			  (current-buffer isearch-view)
			  case-fold-search)
    (regexp-error
      (setq isearch-re-error t)
      'regexp-error)))

(defun isearch-looking-at ()
  (setq isearch-re-error nil)
  (condition-case nil
      (looking-at (car (car isearch-trace))
		  (with-view isearch-view (cursor-pos))
		  (current-buffer isearch-view) case-fold-search)
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
	  (isearch-goto (nth 1 item))
	  (rplaca isearch-trace (cons (car item) (nthcdr 2 item))))
      (isearch-goto isearch-initial-pos)))
  (setq isearch-failing nil))

;; Pops the top string, then moves to the top match of the next string.
(defun isearch-pop-string ()
  (when (cdr isearch-trace)
    (setq isearch-trace (cdr isearch-trace))
    (isearch-pop-match)))

;; Pushes a match at POS onto the top string
(defun isearch-push-match (p)
  (let
      ((item (car isearch-trace)))
    (rplaca isearch-trace (cons (car item) (cons p (cdr item))))))

;; Pushes the current position, pushes the STRING onto the top of the
;; stack, then searches for it
(defun isearch-push-string (string)
  (isearch-push-match (with-view isearch-view (cursor-pos)))
  (setq isearch-trace (cons (cons string nil) isearch-trace))
  (if (isearch-looking-at)
      (isearch-goto (with-view isearch-view (cursor-pos)))
    (let
	((next (if isearch-forwards
		   (isearch-find-next-regexp (with-view isearch-view
					       (forward-char)))
		 (isearch-find-prev-regexp (with-view isearch-view
					     (forward-char -1))))))
      (cond
	((posp next)
	 (isearch-goto next)
	 (setq isearch-failing nil))
	((null? next)
	 (setq isearch-failing t)
	 (beep))))))

;; Goto the current isearch position
(defun isearch-goto (p)
  (with-view isearch-view
    (goto p))
  (isearch-looking-at)
  (when (match-start)
    (let
	((extent isearch-extent))
      (with-view isearch-view
	(goto (match-start))
	(if extent
	    (move-extent extent (match-start) (match-end))
	  (setq extent (make-extent (match-start) (match-end)
				    (list 'face highlight-face)))))
      (setq isearch-extent extent))))


;; Misc functions

;; Display the status message, has to be done after each command
(defun isearch-title ()
  (clear-buffer)
  (insert (concat (if isearch-failing "failing ")
		  (if isearch-wrapped "wrapped ")
		  "I-search: "
		  (car (car isearch-trace))))
  (let
      ((p (cursor-pos)))
    (when isearch-re-error
      (insert "    *Invalid/incomplete regexp*"))
    (goto (start-of-buffer))
    (set-char (char-upcase (get-char)))
    (goto p)))

;; Accept our current position
(defun isearch-accept ()
  (interactive)
  (setq isearch-last-match (car (car isearch-trace)))
  (let
      ((p isearch-initial-pos))
    (with-view isearch-view
      (set-auto-mark p)))
  (isearch-quit t))

;; Cancel the search or if search is failing backtrack to the last match
(defun isearch-cancel ()
  (interactive)
  (if (not (isearch-looking-at))
      (progn
	(while (and (not (isearch-looking-at)) (cdr isearch-trace))
	  (isearch-rubout))
	(isearch-title))
    (isearch-goto isearch-initial-pos)
    (isearch-quit nil)))

;; Copy the rest of the current word to the search string
(defun isearch-yank-word ()
  (interactive)
  (when (isearch-looking-at)
    (isearch-push-string
     (concat (car (car isearch-trace))
	     (with-view isearch-view
	       (quote-regexp (copy-area (match-end)
					(forward-word 1 (match-end))))))))
  (isearch-title))

;; Copy the rest of the line to the search string
(defun isearch-yank-line ()
  (interactive)
  (when (isearch-looking-at)
    (isearch-push-string
     (concat (car (car isearch-trace))
	     (with-view isearch-view
	       (quote-regexp (copy-area (match-end) (end-of-line)))))))
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

(defun isearch (forwards)
  (let
      ((prompt-buffer (make-buffer "*isearch*"))
       (old-buffer (current-buffer))
       ;; FIXME
       (esc-means-meta nil))
    (with-buffer prompt-buffer
      (setq isearch-trace (cons (cons "" nil) nil)
	    isearch-initial-pos (with-buffer old-buffer (cursor-pos))
	    isearch-failing nil
	    isearch-wrapped nil
	    isearch-re-error nil
	    isearch-forwards forwards
	    isearch-view (current-view)
	    isearch-original-buffer (current-buffer (minibuffer-view)))
      (setq local-keymap 'isearch-keymap)
      (make-local-variable 'unbound-key-hook)
      (add-hook 'unbound-key-hook isearch-unbound-key-fun))
    (set-current-view (minibuffer-view))
    (goto-buffer prompt-buffer)
    (isearch-title)))

(defun isearch-quit (status)
  (unless status
    (let
	((old isearch-initial-pos))
      (with-view isearch-view
	(goto old))))
  (let
      ((old-view isearch-view)
       (old-buffer isearch-original-buffer))
    (when isearch-extent
      (delete-extent isearch-extent))
    (kill-all-local-variables)
    (kill-current-buffer)
    (set-current-buffer old-buffer)
    (set-current-view old-view)))
  
(defun isearch-next-forward ()
  (interactive)
  (if (and (equal? (car (car isearch-trace)) "") isearch-last-match)
      (progn
	(isearch-push-string isearch-last-match)
	(isearch-title))
    (let
	((next (if (and isearch-failing isearch-forwards)
	           (progn
		     (setq isearch-wrapped t)
		     (with-view isearch-view
		       (start-of-buffer)))
		 (with-view isearch-view
		   (forward-char)))))
      (setq next (isearch-find-next-regexp next))
      (cond
        ((posp next)
	  (isearch-push-match (with-view isearch-view (cursor-pos)))
	  (setq isearch-failing nil)
	  (isearch-goto next))
	((null? next)
	  (setq isearch-failing t)
	  (beep)))
      (setq isearch-forwards t)
      (isearch-title))))

(defun isearch-next-backward ()
  (interactive)
  (if (and (equal? (car (car isearch-trace)) "") isearch-last-match)
      (progn
	(isearch-push-string isearch-last-match)
	(isearch-title))
    (let
	((next (if (and isearch-failing (not isearch-forwards))
		   (progn
		     (setq isearch-wrapped t)
		     (with-view isearch-view
		       (end-of-buffer)))
		 (with-view isearch-view
		   (forward-char -1)))))
      (setq next (isearch-find-prev-regexp next))
      (cond
       ((posp next)
	(isearch-push-match (with-view isearch-view (cursor-pos)))
	(setq isearch-failing nil)
	(isearch-goto next))
       ((null? next)
	(setq isearch-failing t)
	(beep)))
      (setq isearch-forwards nil)
      (isearch-title))))
