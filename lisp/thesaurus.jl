;;;; thesaurus.jl -- Interface to the Perl based thesaurus
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
;; This implements the main two functions from the GNU Emacs interface
;; to the th program by Darryl Okahata (darrylo@sr.hp.com). Unfortunately
;; I don't know where this can be obtained

(defvar thesaurus-program "th"
  "Name of program used for thesaurus lookups.")

;;;###autoload
(defun thesaurus-lookup-word (word #!optional exact)
  "Look up WORD in the thesaurus, displaying output in the `*shell-output*'
buffer. If EXACT is non-nil, only the entry containing WORD is displayed,
otherwise all entries beginning with WORD are shown."
  (interactive
   (let
       ((arg current-prefix-arg))
     (list (prompt-for-string "Word to lookup:" (symbol-at-point)) arg)))
  (or word (error "Null word to thesaurus lookup"))
  (shell-command (concat thesaurus-program (if exact " -W " " -V ") word)))

;;;###autoload
(defun thesaurus-show-words (word)
  "List all words in the thesaurus matching WORD. Output is left in the
`*shell-output*' buffer."
  (interactive (list (prompt-for-string "Word to lookup:" (symbol-at-point))))
  (or word (error "Null word to thesaurus lookup"))
  (shell-command (concat thesaurus-program " -w " word)))
