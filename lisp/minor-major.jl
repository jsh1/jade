;;;; minor-major.jl -- Major modes by extent
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

(defvar minor-major-face (or (boundp 'minor-major-face)
			     (let
				 ((face (make-face "minor-major")))
			       (set-face-attribute face 'foreground "darkblue")
			       face))
  "Face used in areas of the buffer with a minor-major mode.")

;;;###autoload
(defun minor-major-mode (mode start end)
  "Set the area of the current buffer between START and END (the current block
when called interactively) to have the major-mode MODE.

Returns the extent containing the region. This extent always has its
`rear-sticky' property set, but not its `front-sticky' property."
  (interactive "-aMajor mode:\nm\nM")
  (let
      ((extent (make-extent start end)))
    (extent-put extent 'rear-sticky t)
    (extent-put extent 'catch-variables t)
    (extent-put extent 'minor-major mode)
    (extent-put extent 'face minor-major-face)
    (save-excursion
     (goto start)
     (save-restriction
       (restrict-buffer start end)
       (setq major-mode-kill nil)
       (funcall mode)
       extent))))

(defun delete-minor-major-mode (position)
  "Remove the innermost minor-major mode containing POSITION (the cursor
position when called interactively)."
  (interactive "d")
  (let
      ((extent (get-extent position)))
    (while (and extent (not (extent-get extent 'minor-major)))
      (setq extent (extent-parent extent)))
    (if extent
	(delete-extent extent)
      (error "No minor-major mode here: %s" position))))
