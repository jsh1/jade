;;;; latin-1.jl -- Make the default glyph-table show Latin 1 chars
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

(provide 'latin-1)

(defvar use-latin-1 nil
  "Non-nil when the Latin-1 character set is being used.")

;;;###autoload
(defun latin-1-mode ()
  "Toggles whether or not the characters with numeric values from 160 to 256
are displayed as octal escape sequences or in the Latin-1 character set."
  (interactive)
  (let
      ((i 160))
    (setq use-latin-1 (not use-latin-1))
    (while (< i 256)
      (set-glyph (default-glyph-table) i (if use-latin-1
					     (make-string 1 i)
					   (format nil "\\%o" i)))
      (setq i (1+ i))))
  use-latin-1)
