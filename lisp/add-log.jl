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

(require 'fill)
(require 'maildefs)			;for user-mail-address
(provide 'add-log)

(defvar change-log-file "ChangeLog"
  "File name of change logs")

(defvar change-log-date-format "%Y-%m-%d"
  "Format of dates in ChangeLog entry separators. See `current-time-string'")

(defvar change-log-date-match-format change-log-date-format
  "Format of ChangeLog date string that must match for two entries to be
considered as referring to the same day.")

(defun change-log-shorten-file (file log-file)
  (let ((log-dir (file-name-directory log-file)))
    (if (string-looking-at (quote-regexp log-dir) file)
	(substring file (match-end))
      file)))

;;;###autoload
(defun find-change-log-file (directory)
  (when (and directory (file-directory-p directory))
    (if (file-exists-p (expand-file-name change-log-file directory))
	;; expand twice to make absolute
	(expand-file-name (expand-file-name change-log-file directory))
      (find-change-log-file
       (expand-file-name ".." directory)))))

;;;###autoload
(defun add-change-log-entry (#!optional log-file file-list function-list)
  (interactive
   (let
       ((arg current-prefix-arg))
     (list (prompt-for-file "Log file:" nil
			    (find-change-log-file default-directory))
	   (list (or (buffer-file-name) default-directory))
	   (when (not arg)
	     (let
		 ((defun (defun-at-point)))
	       (and defun (list defun)))))))
  (setq log-file (expand-file-name (or log-file "")))
  (cond
   ((file-directory-p log-file)
    (setq log-file (concat log-file change-log-file)))
   ((equal log-file "")
    (setq log-file change-log-file)))
  (when (find-file log-file)
    (goto (start-of-buffer))
    (unless (log-in-same-day-p (copy-area (start-of-buffer)
					  (end-of-line (start-of-buffer))))
      (insert (concat (current-time-string nil change-log-date-format)
		      "  " (user-full-name) "  <" user-mail-address ">\n\n")))
    (goto (pos 0 1))
    (insert "\n\t* \n")
    (goto (end-of-line (pos 0 2)))
    (unless major-mode
      (indented-text-mode))
    (when (or file-list function-list)
      (when file-list
	(mapc (lambda (f)
		(insert (change-log-shorten-file f log-file))
		(insert ", ")) file-list)
	(backspace-char 2)
	(insert " "))
      (when function-list
	(insert "\(")
	(mapc (lambda (f)
		(insert f)
		(insert " ")) function-list)
	(backspace-char 1)
	(insert "\) "))
      (backspace-char 1)
      (insert ": ")
      (fill-paragraph))))

(defun log-in-same-day-p (old-header)
  (string-match (concat "^.*" (quote-regexp
				(current-time-string
				 nil change-log-date-match-format))
			".*  " (quote-regexp (user-full-name))
			"  <" (quote-regexp user-mail-address) ">")
		old-header))

;;;###autoload
(defun changelog-mode ()
  "Major mode for editing ChangeLogs."
  (interactive)
  (when major-mode-kill
    (major-mode-kill (current-buffer)))
  (require 'text-mode)
  (setq mode-name "ChangeLog"
	major-mode 'changelog-mode
	major-mode-kill changelog-mode-kill
	local-keymap 'text-mode-indent-keymap)
  ;; In case fill.jl hasn't been loaded yet.
  (make-local-variable 'fill-prefix)
  (make-local-variable 'fill-prefix-width)
  (setq fill-prefix "\t"
	fill-prefix-width (tab-size))
  (call-hook 'text-mode-hook)
  (call-hook 'changelog-mode-hook))

(defun changelog-mode-kill ()
  (setq mode-name nil
	local-keymap nil
	major-mode nil
	major-mode-kill nil))
