;;;; replace.jl -- Commands for replacing text
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

(provide 'replace)

;; From isearch.jl
(defvar case-fold-search t)
(make-variable-buffer-local 'case-fold-search)

(defvar replace-preserve-case t
  "When non-nil, replace-last-match will attempt to preserve the original case
of replaced strings.")



;;;###autoload
(defun replace-last-match (template)
  "Replace the whole of the most recently matched regular expression with
the result of `(expand-last-match TEMPLATE)'. Returns the position of the
character following the insertion."
  (let
      ((old (copy-area (match-start) (match-end)))
       (new (expand-last-match template)))
    (when replace-preserve-case
      (cond ((string-upper-case? old)
	     (setq new (string-upcase new)))
	    ((string-capitalized? old)
	     (setq new (capitalize-string new)))))
    (delete-area (match-start) (match-end))
    (insert new (match-start))))

;;;###autoload
(defun replace-string (old new p)
  "Replace the string OLD beginning at position POS, by the string NEW.
Returns the position of the character following the insertion."
  (when (looking-at (quote-regexp old) p)
    (delete-area (match-start) (match-end))
    (insert new (match-start))))

;;;###autoload
(defun replace-all (from template)
  "Replace all occurrences of the regexp FROM with the expansion from TEMPLATE
for that particular occurrence (see the `expand-last-match' function for
details of what can be in TEMPLATE). Moves the cursor to the end of the
last change."
  (interactive "sReplace regexp:\nsReplace regexp `%s' with:")
  (goto (start-of-buffer))
  (while (re-search-forward from nil nil case-fold-search)
    (goto (replace-last-match template))))


;;; Query replace

(defvar query-replace-keymap
  (bind-keys (make-sparse-keymap)
    "SPC" 'query-replace-replace
    "y" 'query-replace-replace
    "BS" 'query-replace-skip
    "n" 'query-replace-skip
    "," 'query-replace-replace-and-wait
    "RET" 'query-replace-exit
    "ESC" 'query-replace-exit
    "q" 'query-replace-exit
    "Ctrl-g" 'query-replace-exit
    "." 'query-replace-once-only
    "!" 'query-replace-rest
    "^" 'query-replace-backtrack
    "Ctrl-r" 'query-replace-edit
    "Ctrl-w" '(progn (query-replace-delete) (query-replace-edit))
    "?" 'query-replace-help
    "Ctrl-h" 'query-replace-help))

;; Special vars
(defvar query-replace-from nil)
(defvar query-replace-to nil)
(defvar query-replace-title nil)
(defvar query-replace-trace nil)
(defvar query-replace-old-kp nil)
(defvar query-replace-alive nil)

(defun query-replace-replace ()
  (interactive)
  (when (looking-at query-replace-from nil nil case-fold-search)
    (goto (replace-last-match query-replace-to)))
  (throw 'query-replace))

(defun query-replace-skip ()
  (interactive)
  (when (looking-at query-replace-from nil nil case-fold-search)
    (goto (match-end)))
  (throw 'query-replace))

(defun query-replace-replace-and-wait ()
  (interactive)
  (when (looking-at query-replace-from nil nil case-fold-search)
    (goto (replace-last-match query-replace-to)))
  (message query-replace-title))

(defun query-replace-exit ()
  (interactive)
  (setq query-replace-alive nil)
  (throw 'query-replace))

(defun query-replace-once-only ()
  (interactive)
  (when (looking-at query-replace-from nil nil case-fold-search)
    (goto (replace-last-match query-replace-to)))
  (query-replace-exit))

(defun query-replace-rest ()
  (interactive)
  (and (looking-at query-replace-from nil nil case-fold-search)
       (goto (replace-last-match query-replace-to)))
  (while (re-search-forward query-replace-from nil nil case-fold-search)
    (goto (replace-last-match query-replace-to)))
  (setq query-replace-alive 'rest)
  (throw 'query-replace))

(defun query-replace-edit ()
  (interactive)
  (remove-hook 'unbound-key-hook query-replace-unbound-key-fun)
  (let
      ((buf (current-buffer))
       (esc-means-meta t)
       (overriding-local-keymap nil)
       (global-keymap query-replace-old-kp))
    (unwind-protect
	(recursive-edit)
      (with-buffer buf
	(add-hook 'unbound-key-hook query-replace-unbound-key-fun))))
  (throw 'query-replace))

(defun query-replace-delete ()
  (interactive)
  (when (looking-at query-replace-from nil nil case-fold-search)
    (delete-area (match-start) (match-end))))

(defun query-replace-backtrack ()
  (interactive)
  (if (cdr query-replace-trace)
      (progn
	(setq query-replace-trace (cdr query-replace-trace))
	(goto (car query-replace-trace)))
    (beep))
  (message query-replace-title))
    
(defun query-replace-unbound-key-fun ()
  (beep)
  (message query-replace-title))

;;;###autoload
(defun query-replace (query-replace-from query-replace-to)
  "Command to interactively replace all occurrences of the regexp
QUERY-REPLACE-FROM with the expansion of the template QUERY-RFEPLACE-TO.
If FROM or TO are not given they are prompted for.
  As each occurrence is found the editor pauses, waiting for the user to
type one of the following special commands,\n
  `SPC', `y'         replace this occurrence and find the next
  `BS', `n'          ignore this occurrence and search for the next
  `,'                replace this match and wait for another command
  `RET', `ESC', `q'  exit the query-replace
  `.'                replace this occurrence then exit the query-replace
  `!'                replace all matches from here to the end of the buffer
  `^'                return to the previous match
  `Ctrl-r'           enter a recursive edit (`Ctrl-Meta-c' to exit)
  `Ctrl-w'           delete the match, then enter a recursive edit
  `Ctrl-h'           show some help text"
  (interactive "sQuery replace regexp: \nsQuery replace regexp `%s' with: ")
  (let
      ((query-replace-trace nil)
       (query-replace-alive t)
       (query-replace-title (concat "Query replacing " query-replace-from
				    " with " query-replace-to ": "))
       (query-replace-old-kp global-keymap)
       (global-keymap nil)
       (buf (current-buffer))
       (overriding-local-keymap 'query-replace-keymap)
       (esc-means-meta nil)		; want to bind to ESC
       match)
    (add-hook 'unbound-key-hook query-replace-unbound-key-fun)
    (unwind-protect
	(while (and (eq? query-replace-alive t)
		    (setq match (re-search-forward
				 query-replace-from nil nil case-fold-search)))
	  (goto match)
	  (setq query-replace-trace (cons match query-replace-trace))
	  (catch 'query-replace
	    (message query-replace-title)
	    (recursive-edit)))
      (with-buffer buf
	(remove-hook 'unbound-key-hook query-replace-unbound-key-fun)))
    (message "Done.")
    ;; Returns `rest' if the `!' command was given, t if the cycle ended due
    ;; to lack of matches, nil otherwise
    query-replace-alive))
