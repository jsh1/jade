;;;; popup-menus.jl -- Portable definitions for popup-menus
;;;  Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
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

(require 'ring)
(provide 'popup-menus)

(defvar popup-menus-show-shortcuts nil
  "When non-nil popup menu labels will include the first keybinding of the
command. This can be quite slow.")

(defvar popup-menus-describe-location t
  "When non-nil, any commands executed through the menu will print their
keybindings if there are any.")

(defvar popup-local-menus nil
  "When non-nil, this variable should contain popup menu specifiers for the
local mode.")
(make-variable-buffer-local 'popup-local-menus)

;; Temporarily set to (VIEW . POS) defining where the mouse was when
;; the menu was popped.
(defvar popup-menus-pos nil)

(setq-default popup-menus
  '(("Files"
     ("Open file..." find-file)
     ("Open directory..." dired)
     ("Save buffer" save-file)
     ("Save buffer as..." save-file-as)
     ("Revert buffer" revert-buffer)
     ("Recover file" recover-file)
     ("Insert file..." insert-file)
     ("Select buffer" . popup-menu-buffers-spec)
     ("Kill current buffer" kill-current-buffer)
     ()
     ("Exit Jade" save-and-quit))
    ("Windows"
     ("Make new window" make-window)
     ("Delete window" delete-window)
     ("Split view" split-view)
     ("Delete view" delete-view)
     ("One view" delete-other-views))
    ("Tools"
     ("Print"
      ("Print buffer" print-buffer)
      ("Print buffer to file..." print-buffer-to-file)
      ("Print buffer to printer..." print-buffer-to-printer)
      ()
      ("Print block" print-area)
      ("Print block to file..." print-area-to-file)
      ("Print block to printer..." print-area-to-printer))
     ("Compare"
      ("Two buffers" diff-buffers)
      ("Two files" diff)
      ("With backup file" diff-backup)
      ("With autosave file" diff-auto-save))    
     ("Version control"
      ("RCS"
       ("Register buffer" rcs-register-buffer)
       ("Check in/out" rcs-toggle-read-only)
       ("Revert to last version" rcs-revert-buffer)
       ("Set default branch" rcs-set-default-branch)
       ()
       ("Show history" rcs-display-log)
       ("Compare with last version" rcs-compare-revisions)
       ("Show other version" rcs-view-revision))
      ("CVS"
       ("Add file" cvs-add)
       ("Ignore file" cvs-ignore)
       ("Commit file" cvs-commit)
       ("Commit directory..." cvs-commit-directory)
       ("Revert to last version" cvs-undo-modification)
       ("Show history" cvs-log)
       ("Show differences" cvs-diff-cvs)
       ()
       ("Summarise directory..." cvs-update)
       ("Summarise current directory" cvs-update-pwd)))
     ("Spell"
      ("Check buffer" ispell-buffer)
      ("Check block" ispell-region)
      ("Highlight misspellings" ispell-highlight-misspellings)
      ("Toggle asynchronous checking" ispell-minor-mode)
      ()
      ("Set dictionary..." ispell-set-dictionary)
      ("Add to personal dictionary..." ispell-add-word-to-dictionary)
      ("Add word for session..." ispell-add-word-for-session)
      ("Save dictionary now" ispell-save-dictionary)
      ()
      ("Kill spell subprocesses" ispell-kill-process))
     ("Read mail" read-mail)
     ("Send mail" mail-setup)
     ("Telnet..." telnet)
     ("Rlogin..." rlogin)
     ("Display URL..." find-url)
     ()
     ("Compile" compile)
     ("Next compilation error" next-error)
     ("Run debugger"
      ("GDB" gdb)
      ("Perldb" perldb)))
    ("Edit"
     ("Undo" undo)
     ("Cut" kill-block)
     ("Copy" copy-block-as-kill)
     ("Paste" yank)
     ("Select and paste" . popup-menu-yank-spec)
     ()
     ("Set mark" set-auto-mark)
     ("Swap point and mark" swap-cursor-and-auto-mark)
     ("Goto line" goto-line)
     ()
     ("Toggle overwrite mode" overwrite-mode)
     ("Toggle auto-fill mode" fill-mode))
    ("Search"
     ("Search..." isearch-forward)
     ("Search backwards..." isearch-backwards)
     ("Query replace..." query-replace)
     ()
     ("Find tag..." find-tag)
     ("Tags search..." tags-search)
     ("Tags query-replace..." tags-query-replace)
     ("Visit tags table..." visit-tags-table))
    (popup-local-menus)
    ("Help"
     ("Info browser" info)
     ("Describe mode" describe-mode)
     ("Apropos commands..." apropos-function)
     ("Apropos variables..." apropos-variable)
     ("Describe command..." describe-function)
     ("Describe variable..." describe-variable)
     ()
     ("List keybindings" describe-keymap)
     ("Describe key..." describe-key)
     ("Locate command..." where-is))))
(make-variable-buffer-local 'popup-menus)

(defun popup-menu ()
  (interactive)
  (or (fboundp 'popup-menu-from-spec)
      (error "Popup menus not supported in this window system"))
  (setq popup-menus-pos (mouse-view-pos))
  (let
      ((spec (or (and popup-menus-pos
		      (posp (cdr popup-menus-pos))
		      (extent-get
		       (get-extent (cdr popup-menus-pos)
				   (current-buffer (car popup-menus-pos)))
		       'popup-menus))
		 (progn
		   ;; when using the usual menus, don't warp the cursor
		   ;; to the mouse position when dispatching a command
		   (setq popup-menus-pos nil)
		   popup-menus))))
    (when spec
      ;; This function should be defined by the window system
      (popup-menu-from-spec spec))))

(defun popup-menu-buffers-spec ()
  (nconc (mapcar #'(lambda (b)
		     (list (buffer-name b) t `(lambda () (goto-buffer ,b))))
		 buffer-list)
	 (list nil '("List all buffers" buffer-summary))))

(defun popup-menu-yank-spec ()
  (let
      ((i 1)
       (print-escape 't)
       out)
    (while (<= i (ring-size kill-ring))
      (let
	  ((string (get-from-ring kill-ring i)))
	(when (> (length string) 32)
	  (setq string (concat (substring string 0 32) "...")))
	(setq out (cons (list (format nil "%S" string)
			      t `(lambda ()
				   (insert ,(get-from-ring kill-ring i))))
			out))
	(setq i (1+ i))))
    (nreverse out)))

;; This function should be invoked when COMMAND is selected from a menu
(defun popup-menu-dispatch-command (command)
  (let
      ((current-command-from-mouse t)
       (location (and popup-menus-describe-location
		      (commandp command)
		      (where-is command))))
    (when location
      (message (format nil "You can run the command `%s' with `%s'"
		       command (car location))))
    (when popup-menus-pos
      (when (viewp (car popup-menus-pos))
	(set-current-view (car popup-menus-pos)))
      (when (posp (cdr popup-menus-pos))
	(goto (cdr popup-menus-pos))))
    (setq popup-menus-pos nil)
    (if (commandp command)
	(call-command command)
      (funcall command))))
