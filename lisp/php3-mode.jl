;;;; php3-mode.jl -- Major mode for editing PHP3 web pages
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

(require 'c-mode)
(require 'text-mode)
(provide 'php3-mode)

(defvar php3-mode-minor 'c-mode
  "The major mode to install in PHP3 sections; nil to disable.")

(defvar php3-mode-major 'fundamental-mode
  "The major mode to install in non-PHP3 sections.")

(defvar php3-mode-scan-when-idle t
  "When non-nil each time the editor becomes idle, and the buffer has been
modified, rescan for C regions. This variable is only consulted when the
mode is first initialised.")

(defvar php3-mode-last-scan nil)
(make-variable-buffer-local 'php3-mode-last-scan)
(defvar php3-mode-major-kill nil)
(make-variable-buffer-local 'php3-mode-major-kill)

(defvar php3-mode-ctrl-c-map (bind-keys (make-sparse-keymap)
			       "C-l" 'php3-mode-make-minor))

;;;###autoload
(defun php3-mode ()
  "PHP3 Mode:\n
Major mode for editing PHP3 web pages. Mode-local commands are:\n
\\{php3-mode-ctrl-c-map,C-c}

The variable `php3-mode-minor' controls whether or not the parts of the
web page that are written in PHP3 have the value of `php3-mode-minor'
installed in them. All other parts of the buffer use the value of
`php3-mode-major' as their major-mode.

The \\[php3-mode-make-minor] command reparses the buffer for PHP3 sections.

If the `php3-mode-scan-when-idle' variable is non-nil (the default), the
buffer will automatically be rescanned each time the editor is idle for
a second or more, providing the buffer has been modified since the previous
scan."
  (interactive)
  (when major-mode-kill
    (funcall major-mode-kill))
  (when php3-mode-major
    (funcall php3-mode-major))
  (setq php3-mode-major-kill major-mode-kill)
  (setq mode-name "PHP3")
  (setq major-mode 'php3-mode)
  (setq major-mode-kill 'php3-mode-kill)
  (setq local-ctrl-c-keymap php3-mode-ctrl-c-map)
  (call-hook 'php3-mode-hook)
  (when php3-mode-scan-when-idle
    (make-local-variable 'idle-hook)
    (add-hook 'idle-hook 'php3-mode-idle-function))
  (php3-mode-make-minor))

(defun php3-mode-kill ()
  (php3-mode-delete-minors)
  (when php3-mode-major-kill
    (funcall php3-mode-major-kill))
  (kill-all-local-variables))

(defun php3-mode-make-minor ()
  "Rescan the current buffer for PHP3 regions embedded in the web page.
Give any such regions minor-major modes."
  (interactive)
  (when php3-mode-minor
    (let
	((point (end-of-buffer))
	 start end)
      (while (setq point (re-search-backward
			  "\\s\\?>|\\s%>|</script>" point nil t))
	(setq end point)
	(setq start nil)
	(cond ((looking-at "\\s\\?>" end)
	       (and (re-search-backward "<\\?\\s" end)
		    (setq start (match-end))))
	      ((looking-at "\\s%>" end)
	       (and (re-search-backward "<%\\s" end)
		    (setq start (match-end))))
	      (t
	       (and (re-search-backward "<script" end nil t)
		    (looking-at "\\s+language=\"?php\"?>" (match-end) nil t)
		    (setq start (match-end)))))
	(when (and start
		   (catch 'foo
		     (map-extents #'(lambda (e)
				      (when (extent-get e 'minor-major)
					(throw 'foo nil))) start end)
		     t))
	  (extent-put (minor-major-mode php3-mode-minor start end)
		      'rear-sticky nil))
	(setq point (or start (forward-char -1 end)))))))

(defun php3-mode-idle-function ()
  (when (or (not php3-mode-last-scan) (> (buffer-changes) php3-mode-last-scan))
    (php3-mode-make-minor)
    ;; Ensure that only one copy of php3-mode-last-scan per buffer
    ;; is maintained
    (extent-set (extent-root) 'php3-mode-last-scan (buffer-changes))))

(defun php3-mode-delete-minors ()
  "Delete all extents in the current buffer that have PHP3 minor-major modes."
  (interactive)
  (let
      (extents)
    (map-extents #'(lambda (e)
		     (when (eq (extent-get e 'minor-major) php3-mode-minor)
		       (setq extents (cons e extents))))
		 (start-of-buffer) (end-of-buffer))
    (mapc 'delete-extent extents)))
