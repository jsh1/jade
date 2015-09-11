;;;; page-summary.jl -- Summarise buffer by its page headers
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

(defvar page-summary-keymap (bind-keys (make-sparse-keymap)
			      "TAB" 'page-summary-next
			      "M-TAB" 'page-summary-previous
			      "RET" 'page-summary-select))

(defvar page-summary-file nil)
(make-variable-buffer-local 'page-summary-file)

;;;###autoload
(defun page-summary (buffer)
  "Display a summary of the `pages' in the current buffer. For each page, the
first group of non-empty lines is displayed."
  (interactive (list (if current-prefix-arg
			 (prompt-for-buffer "Summarise buffer:" t)
		       (current-buffer))))
  (let
      ((output (open-buffer (concat "*summary-of-"
				    (buffer-name buffer) #\*)))
       (point (start-of-buffer buffer t)))
    (with-buffer buffer
      (save-restriction
	(unrestrict-buffer)
	(clear-buffer output)
	(while (< point (end-of-buffer))
	  (unless (equal? point (start-of-buffer))
	    (setq point (start-of-line (forward-line 1 point))))
	  (let
	      ((start point))
	    (while (not (empty-line-p point))
	      (setq point (forward-line 1 point)))
	    (setq point (forward-line 1 point))
	    (format output "\nLine %d:\n" (pos-line start))
	    (write output (copy-area start point)))
	  (setq point (forward-page 1 point)))))
    (with-view (other-view)
      (goto-buffer output)
      (setq page-summary-file (buffer-file-name buffer))
      (setq local-keymap 'page-summary-keymap)
      (setq major-mode 'page-summary-mode)
      (setq mode-name "Page-Summary")
      (goto (start-of-buffer)))))

(defun page-summary-mode ()
  "Page summary mode:\n
Major mode used to summarise the pages in a buffer. Local commands are:\n
\\{page-summary-keymap}")

(defun page-summary-next (count #!optional point)
  (interactive "@p")
  (unless point
    (setq point (cursor-pos)))
  (while (and (> count 0) point)
    (setq point (re-search-forward "^Line [0-9]+:$" (forward-line 1 point)))
    (setq count (1- count)))
  (while (and (< count 0) point)
    (setq point (re-search-backward "^Line [0-9]+:$" (backward-line 1 point)))
    (setq count (1+ count)))
  point)

(defun page-summary-previous (count #!optional point)
  (interactive "@p")
  (page-summary-next (- (or count 1)) point))

(defun page-summary-select ()
  (interactive)
  (when (looking-at "^Line ([0-9]+):$" (start-of-line))
    (let
	((point (pos 0 (string->number (expand-last-match "\\1"))))
	 (file page-summary-file))
      (with-view (other-view)
	(find-file file)
	(goto point)))))
