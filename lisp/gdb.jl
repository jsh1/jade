;;;; gdb.jl -- run gdb in a buffer
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

(require 'shell)
(provide 'gdb)

(defvar gdb-file-name "gdb"
  "The file name of the gdb program to run.")

(defvar gdb-auto-centre nil
  "When non-nil the current line in the source view is automatically
centred each time it changes.")

(defvar gdb-last-frame nil
  "(FILE . LINE-NUMBER) representing the last frame we displayed.")
(make-variable-buffer-local 'gdb-last-frame)

(defvar gdb-last-buffer nil
  "This is the last *gdb* buffer which was used.")

(defvar gdb-buffer-p nil
  "Flag that says this buffer is for running a gdb process.")
(make-variable-buffer-local 'gdb-buffer-p)

(defvar gdb-spare-output nil)
(make-variable-buffer-local 'gdb-spare-output)

(defvar gdb-delete-prompt nil)
(make-variable-buffer-local 'gdb-delete-prompt)

(defvar gdb-ctrl-c-keymap (copy-sequence shell-ctrl-c-keymap))
(bind-keys gdb-ctrl-c-keymap
  "Ctrl-n" '(gdb-command "next %d\n" (prefix-numeric-argument current-prefix-arg))
  "Ctrl-s" '(gdb-command "step %d\n" (prefix-numeric-argument current-prefix-arg))
  "Ctrl-i" '(gdb-command "stepi %d\n" (prefix-numeric-argument current-prefix-arg))
  "Ctrl-I" '(gdb-command "nexti %d\n" (prefix-numeric-argument current-prefix-arg))
  "Ctrl-f" '(gdb-command "finish\n")
  "Ctrl-r" '(gdb-command "cont\n")
  "Ctrl-<" '(gdb-command "up %d\n" (prefix-numeric-argument current-prefix-arg))
  "Ctrl->" '(gdb-command "down %d\n" (prefix-numeric-argument current-prefix-arg))
  "Ctrl-b" '(gdb-command "break %s:%d\n" (gdb-current-file) (gdb-current-line))
  "Ctrl-t" '(gdb-command "tbreak %s:%d\n" (gdb-current-file) (gdb-current-line))
  "Ctrl-d" '(gdb-command "clear %d\n" (gdb-current-line))
  "Ctrl-l" 'gdb-redisplay-frame)

(bind-keys ctrl-x-keymap
  "Ctrl-a" '(setq next-keymap-path '(gdb-ctrl-c-keymap)))

;;;###autoload
(defun gdb (prog)
  "Run the gdb debugger in an editor buffer (called `*gdb*'). PROG is the
program which is to be debugged.
See the `gdb-mode' documentation for details of the available commands.
There is no limit to the number of gdb processes you may run at once."
  (interactive "fProgram to debug:")
  (let*
      ((buffer (get-buffer "*gdb*")))
    (unless (setq prog (local-file-name prog))
      (error "Can only debug local programs"))
    (if (or (not buffer) (with-buffer buffer shell-process))
	(setq buffer (open-buffer "*gdb*" t))
      (clear-buffer buffer))
    (goto-buffer buffer)
    (kill-all-local-variables)
    (setq default-directory (file-name-directory prog)
	  shell-program gdb-file-name
	  shell-program-args (list "-fullname" (file-name-nondirectory prog))
	  shell-prompt-regexp "^(\\(gdb\\) *|.*\\(.+\\) *|.+---)"
	  shell-output-stream (list 'lambda '(x)
				    (list 'gdb-output-filter
					  (current-buffer)
					  'x)))
    (shell-mode)
    (setq mode-name (concat "GDB:" (file-name-nondirectory prog))
	  major-mode 'gdb-mode
	  ctrl-c-keymap gdb-ctrl-c-keymap
	  gdb-last-buffer buffer
	  gdb-buffer-p t)
    (call-hook 'gdb-hook)))

(defun gdb-mode ()
  "Gdb Mode:\n
This major-mode is used to run the GDB debugger in an editor buffer. To
start a gdb subprocess use the `Meta-x gdb' command.\n
Each time the target process stops executing the source line of the
current frame is highlighted in a separate view.\n
The following commands are available in the `*gdb*' buffer,\n
  `Ctrl-c Ctrl-n'	Next line
  `Ctrl-c Ctrl-s'	Step 
  `Ctrl-c Ctrl-i'	Step the next instruction
  `Ctrl-c Ctrl-I'	Next instruction
  `Ctrl-c Ctrl-f'	Finish this frame
  `Ctrl-c Ctrl-r'	Continue execution
  `Ctrl-c Ctrl-<'	Move up one frame
  `Ctrl-c Ctrl->'	Move down one frame
  `Ctrl-c Ctrl-b'	Set a breakpoint at the current line
  `Ctrl-c Ctrl-t'	Set a temporary breakpoint
  `Ctrl-c Ctrl-d'	Delete the breakpoint at the current line
  `Ctrl-c Ctrl-l'	Redisplay the current frame\n
They are also accessible in any buffer by replacing the `Ctrl-c' prefix
with `Ctrl-x Ctrl-a'.\n
Since this command is built on top of the shell-mode all commands from
that mode are available as well.")


;; Digs the variable VAR out of the gdb-last-buffer
(defmacro gdb-get-buffer-var (var)
  (list 'with-buffer 'gdb-last-buffer var))

;; Gets the name of the current file
(defun gdb-current-file ()
  (file-name-nondirectory (if gdb-buffer-p
			      (car gdb-last-frame)
			    (buffer-file-name))))

;; Gets the current line number, it increments it as well
(defun gdb-current-line ()
  (1+ (if gdb-buffer-p
	  (cdr gdb-last-frame)
	(pos-line (cursor-pos)))))

;; Send a command to the correct gdb process
(defun gdb-command (format-spec &rest format-args)
  (when gdb-buffer-p
    (setq gdb-last-buffer (current-buffer)))
  (apply 'format
	 (gdb-get-buffer-var shell-process)
	 format-spec format-args)
  (with-buffer gdb-last-buffer
    (when (looking-at "^\\(.*gdb.*\\) *" (start-of-line (end-of-buffer)))
      (setq gdb-delete-prompt t))))

;; Receives all output from the gdb subprocess, it acts upon and removes
;; any frame markers
(defun gdb-output-filter (buffer data)
  (let
      (new-frame)
    (with-buffer buffer
      (when gdb-spare-output
	(setq data (concat gdb-spare-output data)))
      (goto (end-of-buffer))
      (when (and gdb-delete-prompt
		 (looking-at shell-prompt-regexp (start-of-line)))
	(delete-area (match-start) (match-end))
	(setq gdb-delete-prompt nil))
      (while (string-match "\032\032([^:\n]+):([0-9]+):([0-9]+):.*\n" data)
	;; A whole marker to process
	(insert (substring data 0 (match-start 0)))
	(setq
	 gdb-last-frame (cons (substring data (match-start 1) (match-end 1))
			      (1- (read (cons 0 (substring data
							   (match-start 2)
							   (match-end 2))))))
	 new-frame t
	 data (substring data (match-end 0))))
      (if (string-match "\032" data)
	  ;; Start of an incomplete marker; save it for later
	  (progn
	    (insert (substring data 0 (match-start 0)))
	    (setq gdb-spare-output (substring data (match-start 0))))
	(insert data)
	(setq gdb-spare-output nil)))
    (when new-frame
      (setq gdb-last-buffer buffer)
      ;; Now redisplay the frame and its highlight
      (let*
	  ((frame (with-buffer buffer gdb-last-frame))
	   (view (if gdb-buffer-p (other-view) (current-view)))
	   (line-pos (pos 0 (cdr frame)))
	   old-buf)
	(with-view view
	  (setq old-buf (current-buffer))
	  (find-file (car frame))
	  (mark-block line-pos (end-of-line line-pos))
	  (goto (glyph-to-char-pos (indent-pos line-pos)))
	  (when (or gdb-auto-centre (not (eq old-buf (current-buffer))))
	    (center-display)))))))

(defun gdb-redisplay-frame ()
  (interactive)
  (with-view (if gdb-buffer-p (other-view) (current-view))
    (when gdb-buffer-p
      (setq gdb-last-buffer (current-buffer)))
    (let*
	((frame (gdb-get-buffer-var gdb-last-frame))
	 (line-pos (pos 0 (cdr frame))))
      (find-file (car frame))
      (mark-block line-pos (end-of-line line-pos))
      (goto (glyph-to-char-pos (indent-pos line-pos)))
      (center-display))))
