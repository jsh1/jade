;;;; dict.jl -- Interface to the dict(1) dictionary client
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

;; Commentary:
;;
;; This uses the dict(1) dictionary client by Rik Faith. It can be
;; downloaded via the DICT development groups pages:
;;
;;	http://www.dict.org/links.html
;;
;; I've tested this code with version 1.4.9.
;;
;; The interface is currently exceptionally basic; it could use some
;; more features (asynchronous execution, commands to move through the
;; output, etc..)


;; Configuration

(defvar dict-program "dict"
  "Name of program used for dictionary lookups.")

(defvar dict-strategy 'exact
  "Strategy used for dictionary lookups.")

(defvar dict-strategy-list '(exact prefix substring re regexp soundex lev)
  "List of all dictionary lookup strategies.")

(defvar dict-options nil
  "When non-nil, a string defining extra options passed to the dict program.")


;; Code

;;;###autoload
(defun dict-lookup (word &optional strategy)
  "Look up WORD in the dictionary, displaying output in the `*shell-output*'
buffer. If STRATEGY is non-nil, it should be a symbol whose name defines the
lookup strategy used, otherwise the `dict-strategy' variable specifies the
strategy."
  (interactive
   (let
       ((arg current-prefix-arg))
     (list (prompt-for-string "Word to lookup:" (symbol-at-point))
	   (when arg
	     (intern (prompt-from-list (mapcar 'symbol-name dict-strategy-list)
				       "Lookup strategy:"))))))
  (or word (error "Null word to dictionary lookup"))
  ;; Maybe this should be run asynchronously? It could take a while..
  (message "Querying the DICT server..." t)
  (and (shell-command (concat dict-program " -s "
			      (symbol-name (or strategy dict-strategy))
			      (or dict-options "") " '" word "'"))
       (with-buffer (get-buffer "*shell-output*")
	 (dict-mark-xrefs))))

(defvar dict-xref-map (bind-keys (make-sparse-keymap)
			"Button2-Click1" 'goto-mouse
			"Button2-Move" 'goto-mouse
			"Button2-Off" 'dict-follow-xref))

(defun dict-mark-xrefs ()
  (let
      ((point (start-of-buffer)))
    (while (re-search-forward "{([a-zA-Z-]+)}" point)
      (setq point (match-end))
      (make-extent (match-start 1) (match-end 1)
		   (list 'face underline-face
			 'mouse-face active-face
			 'mouse-keymap dict-xref-map)))))

(defun dict-follow-xref ()
  (interactive)
  (when (looking-at "{([a-zA-Z-]+)}" (char-search-backward ?\{))
    (dict-lookup (expand-last-match "\\1"))))
