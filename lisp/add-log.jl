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
      (goto-buffer-start)
      (unless (log-in-same-day-p (copy-area (buffer-start)
					    (line-end (buffer-start))))
	(insert (concat (current-time-string) "  "
			(user-full-name) "  <" user-mail-address ">\n\n")))
      (goto-char (pos 0 1))
      (insert "\n\t* \n")
      (goto-char (line-end (pos 0 2)))
      (unless major-mode
	(indented-text-mode)))))

(defun log-in-same-day-p (old-header)
  (regexp-match (concat (regexp-quote (substring (current-time-string) 0 11))
			".*  "
			(regexp-quote (user-full-name))
			"  <"
			(regexp-quote user-mail-address)
			">")
		old-header))
