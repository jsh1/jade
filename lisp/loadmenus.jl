;;;; loadmenus.jl -- Set up standard menu definitions (Amiga only)
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

(if (not (amiga-p))
    (error "Menus only work on an Amiga")
  (set-menu
   '("Project"
     ("Open file... (C-x C-f)" find-file)
     ("Open window (C-x 2)" open-window)
     ("Insert file... (C-x i)" insert-file)
     ()
     ("Save file (C-x C-s)" save-file)
     ("Save file as... (C-x C-w)" save-file-as)
     ("Save some... (C-x s)" save-some-buffers)
     ()
     ("Clear buffer" (when (check-changes) (clear-buffer)))
     ("Kill buffer... (C-x k)" kill-buffer)
     ("Switch buffer... (C-x b)" switch-to-buffer)
     ("Toggle read-only (C-x q)" (set-buffer-read-only nil (not (buffer-read-only-p))))
     ("Toggle modified (M-~)" (set-buffer-modified nil nil))
     ("Close server file (Ctrl-x #)" server-close-file)
     ()
     ("Close window (C-x 0)" close-window)
     ("Close other windows (C-x 1)" close-other-windows)
     ()
     ("Quit (C-x C-c)" save-and-quit))
   '("Edit"
     ("Mark block (C-SPC)" block-toggle)
     ("Mark rectangles (C-M)" toggle-rect-blocks)
     ("Mark whole buffer (C-x h)" mark-whole-buffer)
     ()
     ("Kill block (C-w)" kill-block)
     ("Copy block (M-w)" copy-block-as-kill)
     ("Delete block (C-z)" delete-block)
     ("Upcase block (C-x C-u)" upcase-area)
     ("Downcase block (C-x C-l)" downcase-area)
     ()
     ("Kill word (M-k)" kill-word)
     ("Upcase word (M-u)" upcase-word)
     ("Downcase word (M-l)" downcase-word)
     ("Capitalise word (M-c)" capitalize-word)
     ()
     ("Yank (C-y)" yank)
     ("Rectangular yank (C-Y)" rectangular-yank)
     ("Insert block (C-i)" insert-block)
     ("Kill line (C-k)" kill-line)
     ()
     ("Undo (C-x u)" undo))
   '("Find"
     ("I-search... (C-s)" isearch-forward)
     ("I-search backwards... (C-r)" isearch-backward)
     ()
     ("Global replace..." replace-all)
     ("Query replace... (M-%)" query-replace)
     ()
     ("Line number... (C-j)" goto-line)
     ("Matching bracket (M-n)" (progn (set-auto-mark) (goto-matching-bracket)))
     ("Next window (C-x o)" (set-current-window (next-window) t)))
   '("Marks"
     ("Set auto-mark (C-#)" set-auto-mark)
     ("Goto auto-mark (C-x C-x)" swap-cursor-and-auto-mark)
     ()
     ("Set #1 (S-F1)" (set-mark mark-1 (cursor-pos) (current-buffer)))
     ("Set #2 (S-F2)" (set-mark mark-2 (cursor-pos) (current-buffer)))
     ("Set #3 (S-F3)" (set-mark mark-3 (cursor-pos) (current-buffer)))
     ()
     ("Goto #1 (F1)" (goto-mark mark-1))
     ("Goto #2 (F2)" (goto-mark mark-2))
     ("Goto #3 (F3)" (goto-mark mark-3)))
   '("Help"
     ("Describe key... (C-h k)" describe-key)
     ("Describe key bindings (C-h b)" describe-keymap)
     ("Describe editing mode (C-h m)" describe-mode)
     ()
     ("Describe variable... (C-h v)" describe-variable)
     ("Describe function... (C-h f)" describe-function))))
