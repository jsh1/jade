;;;; add-log.jl -- Making ChangeLog files
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

(defvar change-log-file "ChangeLog"
  "File name of change logs")

;;;###autoload
(defun add-change-log-entry (&optional log-file)
  (interactive "FLog file:")
  (setq log-file (expand-file-name (unless log-file "")))
  (when (or (file-directory-p log-file) (equal log-file ""))
    (setq log-file (file-name-concat log-file change-log-file)))
  (let
      ((log-buffer (open-file log-file)))
    (when log-buffer
      (goto-buffer log-buffer)
      (goto (start-of-buffer))
      (unless (log-in-same-day-p (copy-area (start-of-buffer)
					    (end-of-line (start-of-buffer))))
	(insert (concat (current-time-string) "  "
			(user-full-name) "  <" user-mail-address ">\n\n")))
      (goto (pos 0 1))
      (insert "\n\t* \n")
      (goto (end-of-line (pos 0 2)))
      (unless major-mode
	(indented-text-mode)))))

(defun log-in-same-day-p (old-header)
  (string-match (concat (quote-regexp (substring (current-time-string) 0 11))
			".*  "
			(quote-regexp (user-full-name))
			"  <"
			(quote-regexp user-mail-address)
			">")
		old-header))

;;;###autoload
(defun changelog-mode ()
  "Major mode for editing ChangeLogs."
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill (current-buffer)))
  (setq mode-name "ChangeLog"
	major-mode 'changelog-mode
	major-mode-kill 'changelog-mode-kill
	keymap-path (cons 'text-mode-indent-keymap
			  (cons 'text-mode-keymap keymap-path))
	fill-prefix "\t"
	fill-prefix-width tab-size)
  (call-hook 'text-mode-hook)
  (call-hook 'changelog-mode-hook))

(defun changelog-mode-kill ()
  (setq mode-name nil
	keymap-path (delq 'text-mode-keymap 
			  (delq 'text-mode-indent-keymap keymap-path))
	major-mode nil
	major-mode-kill nil))
