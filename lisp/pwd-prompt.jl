;;;; pwd-prompt.jl -- Prompt for a confidential answer (i.e. a password)
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

(require 'prompt)
(provide 'pwd-prompt)

(defvar pw-glyph-table (make-glyph-table)
  "Glyph table in which every single character is obscured.")

(defvar pw-printed-char ?*
  "The character used in the pw-glyph-table.")

;; Initialise the glyph table
(let
    ((string (make-string 1 pw-printed-char))
     (c 0))
  (while (< c 256)
    (set-glyph pw-glyph-table c string)
    (setq c (1+ c))))

;;;###autoload
(defun pwd-prompt (prompt)
  "Prompt for a confidential string, with PROMPT as the title string. The
contents of the prompt will be masked out whilst being entered."
  (let*
      ((buffer (get-prompt-buffer))
       (orig (buffer-glyph-table buffer)))
    (set-buffer-glyph-table buffer pw-glyph-table)
    ;; This relies upon the assumption that the next caller of
    ;; get-prompt-buffer will be the prompt function itself
    (return-prompt-buffer buffer)
    (prog1
	(prompt prompt)
      (set-buffer-glyph-table buffer orig))))
