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

(defvar gdb-program "gdb"
  "The file name of the gdb program to run.")

(defvar perl-program "perl")

(defvar gdb-auto-centre nil
  "When non-nil the current line in the source view is automatically
centred each time it changes.")

(defvar gdb-frame-face highlight-face
  "Face used to highlight the line in the current stack frame.")

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

(defvar gdb-frame-extent nil)
(make-variable-buffer-local 'gdb-frame-extent)

(defvar gdb-actions nil)
(make-variable-buffer-local 'gdb-actions)

(defvar gdb-input-filter nil)
(make-variable-buffer-local 'gdb-input-filter)

(defvar gdb-ctrl-c-keymap
  (bind-keys (make-sparse-keymap shell-ctrl-c-keymap)
    "Ctrl-n" 'gdb-next
    "Ctrl-s" 'gdb-step
    "Ctrl-i" 'gdb-stepi
    "Ctrl-I" 'gdb-nexti
    "Ctrl-f" 'gdb-finish-frame
    "Ctrl-r" 'gdb-continue
    "Ctrl-<" 'gdb-up-frame
    "Ctrl->" 'gdb-down-frame
    "Ctrl-b" 'gdb-set-breakpoint
    "Ctrl-t" 'gdb-set-temporary-breakpoint
    "Ctrl-d" 'gdb-delete-breakpoint
    "Ctrl-l" 'gdb-redisplay-frame))

(bind-keys ctrl-x-keymap
  "Ctrl-a" 'gdb-ctrl-c-keymap)


;; GDB support

(defvar gdb-gdb-actions
  '((next . (gdb-command "next %d\n"
			 (prefix-numeric-argument current-prefix-arg)))
    (step . (gdb-command "step %d\n"
			 (prefix-numeric-argument current-prefix-arg)))
    (stepi . (gdb-command "stepi %d\n"
			  (prefix-numeric-argument current-prefix-arg)))
    (nexti . (gdb-command "nexti %d\n"
			  (prefix-numeric-argument current-prefix-arg)))
    (finish-frame . (gdb-command "finish\n"))
    (continue . (gdb-command "cont\n"))
    (up-frame . (gdb-command "up %d\n"
			     (prefix-numeric-argument current-prefix-arg)))
    (down-frame . (gdb-command "down %d\n"
			       (prefix-numeric-argument current-prefix-arg)))
    (set-breakpoint . (gdb-command "break %s:%d\n"
				   (gdb-current-file) (gdb-current-line)))
    (set-temporary-breakpoint (gdb-command
			       "tbreak %s:%d\n"
			       (gdb-current-file) (gdb-current-line)))
    (delete-breakpoint (gdb-command "clear %d\n" (gdb-current-line)))))

(defun gdb-gdb-input-filter (data)
  (let
      ((new-frame nil))
    (while (string-match "\032\032([^:\n]+):([0-9]+):([0-9]+):.*\n" data)
      ;; A whole marker to process
      (insert (substring data 0 (match-start 0)))
      (setq gdb-last-frame (cons (substring data (match-start 1) (match-end 1))
				 (1- (read (cons 0 (substring
						    data
						    (match-start 2)
						    (match-end 2)))))))
      (setq new-frame t)
      (setq data (substring data (match-end 0))))
    (if (string-match "\032" data)
	;; Start of an incomplete marker; save it for later
	(progn
	  (insert (substring data 0 (match-start 0)))
	  (setq gdb-spare-output (substring data (match-start 0))))
      (insert data)
      (setq gdb-spare-output nil))
    new-frame))

;;;###autoload
(defun gdb (args)
  "Run the gdb debugger in an editor buffer (called `*gdb*'). ARGS is a string
giving all arguments to the gdb subprocess (including the program to debug).
See the `gdb-mode' documentation for details of the available commands.
There is no limit to the number of gdb processes you may run at once."
  (interactive "sArguments to gdb:")
  (let*
      ((buffer (get-buffer "*gdb*"))
       (directory default-directory))
    (if (or (not buffer) (with-buffer buffer shell-process))
	(setq buffer (open-buffer "*gdb*" t))
      (clear-buffer buffer))
    (goto-buffer buffer)
    (kill-all-local-variables)
    (setq default-directory directory
	  shell-program-args (list "-c" (concat gdb-program
						" -fullname " args))
	  shell-prompt-regexp "^(\\(gdb\\) *|.*\\(.+\\) *|.+---)"
	  shell-output-stream (make-closure
			       (list 'lambda '(x)
				     (list 'gdb-output-filter
					   (current-buffer)
					   'x)))
	  shell-callback-function (lambda () (gdb-callback)))
    (shell-mode)
    (buffer-status-id (concat "GDB: " args))
    (setq major-mode 'gdb-mode
	  mode-name "GDB"
	  local-ctrl-c-keymap gdb-ctrl-c-keymap
	  gdb-last-buffer buffer
	  gdb-buffer-p t
	  gdb-actions gdb-gdb-actions
	  gdb-input-filter gdb-gdb-input-filter)
    (call-hook 'gdb-hook)))

(defun gdb-mode ()
  "Gdb Mode:\n
This major-mode is used to run the GDB debugger in an editor buffer. To
start a gdb subprocess use the `Meta-x gdb' command.\n
Each time the target process stops executing the source line of the
current frame is highlighted in a separate view.\n
The following commands are available in the `*gdb*' buffer:\n
\\{gdb-ctrl-c-keymap,Ctrl-c}
They are also accessible in any buffer by replacing the `Ctrl-c' prefix
with `Ctrl-x Ctrl-a'.")


;; Perldb support

(defvar gdb-perldb-actions
  '((next . (gdb-command "n\n"))
    (step . (gdb-command "s\n"))
    (continue . (gdb-command "c\n"))
    ;; XXX this needs to use (gdb-current-file) as well
    (set-breakpoint . (gdb-command "b %d\n" (gdb-current-line)))
    (delete-breakpoint (gdb-command "d %d\n" (gdb-current-line)))))

(defun gdb-perldb-input-filter (data)
  (let
      ((new-frame nil))
    (while (string-match "\032\032([^:\n]+):([0-9]+):.*\n" data)
      ;; A whole marker to process
      (insert (substring data 0 (match-start 0)))
      (setq gdb-last-frame (cons (expand-file-name
				  (substring data (match-start 1)
					     (match-end 1))
				  default-directory)
				 (1- (read (cons 0 (substring
						    data
						    (match-start 2)
						    (match-end 2)))))))
      (setq new-frame t)
      (setq data (substring data (match-end 0))))
    (if (string-match "\032" data)
	;; Start of an incomplete marker; save it for later
	(progn
	  (insert (substring data 0 (match-start 0)))
	  (setq gdb-spare-output (substring data (match-start 0))))
      (insert data)
      (setq gdb-spare-output nil))
    new-frame))

;;;###autoload
(defun perldb (args)
  "Run the Perl debugger in an editor buffer (called `*perldb*'). ARGS is a
string giving all arguments to the perl subprocess (including the program
to debug). See the `perl-mode' documentation for details of the available
commands. There is no limit to the number of processes you may run at once."
  (interactive "sArguments to perl:")
  (let*
      ((buffer (get-buffer "*perldb*"))
       (directory default-directory))
    (if (or (not buffer) (with-buffer buffer shell-process))
	(setq buffer (open-buffer "*perldb*" t))
      (clear-buffer buffer))
    (goto-buffer buffer)
    (kill-all-local-variables)
    (setq default-directory directory
	  shell-program-args (list "-c" (concat perl-program " -d "
						args " -emacs"))
	  shell-prompt-regexp "^ *DB<+[0-9]+>+ *"
	  shell-output-stream (make-closure
			       (list 'lambda '(x)
				     (list 'gdb-output-filter
					   (current-buffer) 'x)))
	  shell-callback-function (lambda () (gdb-callback)))
    (shell-mode)
    (buffer-status-id (concat "PerlDB: " args))
    (setq major-mode 'perldb-mode
	  mode-name "PerlDB"
	  local-ctrl-c-keymap gdb-ctrl-c-keymap
	  gdb-last-buffer buffer
	  gdb-buffer-p t
	  gdb-actions gdb-perldb-actions
	  gdb-input-filter gdb-perldb-input-filter)
    (call-hook 'gdb-hook)))

(defun perldb-mode ()
  "PerlDB Mode:\n
This major-mode is used to run the Perl debugger in an editor buffer. To
start a PerlDB subprocess use the `Meta-x perldb' command.\n
Each time the target process stops executing the source line of the
current frame is highlighted in a separate view.\n
The following commands are available in the `*perldb*' buffer:\n
\\{gdb-ctrl-c-keymap,Ctrl-c}
They are also accessible in any buffer by replacing the `Ctrl-c' prefix
with `Ctrl-x Ctrl-a'.")


;; Digs the variable VAR out of the gdb-last-buffer
(defmacro gdb-get-buffer-var (var)
  (list 'with-buffer 'gdb-last-buffer var))

(defmacro gdb-set-buffer-var (var value)
  (list 'with-buffer 'gdb-last-buffer (list 'setq var value)))

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
  (apply format
	 (gdb-get-buffer-var shell-process)
	 format-spec format-args)
  (with-buffer gdb-last-buffer
    (when (looking-at shell-prompt-regexp (start-of-line (end-of-buffer)))
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
      (setq new-frame (funcall gdb-input-filter data)))
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
	  (gdb-highlight-line line-pos)
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
      (gdb-highlight-line line-pos)
      (goto (glyph-to-char-pos (indent-pos line-pos)))
      (center-display))))

(defun gdb-highlight-line (line)
  (when (gdb-get-buffer-var gdb-frame-extent)
    (delete-extent (gdb-get-buffer-var gdb-frame-extent)))
  (let
      ((extent (make-extent line (end-of-line line)
			    (list 'face gdb-frame-face))))
    (gdb-set-buffer-var gdb-frame-extent extent)))

;; Called when the current buffer's process changes state
(defun gdb-callback ()
  ;; Use the default callback function
  (shell-default-callback)
  ;; Then do our own cleanup at process termination
  (unless shell-process
    (when gdb-frame-extent
      (delete-extent gdb-frame-extent))
    (when (eq gdb-last-buffer (current-buffer))
      ;; Ensure the buffer can be gc'd in the future
      (setq gdb-last-buffer nil))))


;; User commands

(defun gdb-next ()
  (interactive)
  (gdb-do-command 'next))

(defun gdb-step ()
  (interactive)
  (gdb-do-command 'step))

(defun gdb-stepi ()
  (interactive)
  (gdb-do-command 'stepi))

(defun gdb-nexti ()
  (interactive)
  (gdb-do-command 'nexti))

(defun gdb-finish-frame ()
  (interactive)
  (gdb-do-command 'finish-frame))

(defun gdb-continue ()
  (interactive)
  (gdb-do-command 'continue))

(defun gdb-up-frame ()
  (interactive)
  (gdb-do-command 'up-frame))

(defun gdb-down-frame ()
  (interactive)
  (gdb-do-command 'down-frame))

(defun gdb-set-breakpoint ()
  (interactive)
  (gdb-do-command 'set-breakpoint))

(defun gdb-set-temporary-breakpoint ()
  (interactive)
  (gdb-do-command 'set-temporary-breakpoint))

(defun gdb-delete-breakpoint ()
  (interactive)
  (gdb-do-command 'delete-breakpoint))

(defun gdb-do-command (command)
  (when gdb-buffer-p
    (setq gdb-last-buffer (current-buffer)))
  (let
      ((action (cdr (assq command (gdb-get-buffer-var gdb-actions)))))
    (if action
	(eval action)
      (error "Command %s not supported by this debugger" command))))
