;;;; shell.jl -- a process-in-a-buffer
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

(provide 'shell)

;;; This is a *very* quick package for running a subprocess in a buffer
;;; No completion whatsoever, my plan is to get the shell to do that for
;;; me, though I'm not sure how :-(

;;; By default this sets itself up to run a shell, but it can be used
;;; to provide the base for most types of line-based interaction with
;;; a subprocess. The gdb package is a good example -- it sets up the
;;; buffer-local shell- variables, calls shell-mode to install the
;;; subprocess then redefines the name of the mode and the keymaps.
;;; Its ctrl-c-keymap is built from a copy of shell-ctrl-c-keymap.


;; User options

(defvar shell-file-name (or (getenv "SHELL") "/bin/sh")
  "The name of the shell program.")

(defvar shell-whole-line t
  "When non-nil the whole line (minus the prompt) is sent to the shell
process when `RET' is typed, even if the cursor is not at the end of the
line.")


;; Program options

(defvar shell-program shell-file-name
  "The program to run, by default the standard shell.")
(make-variable-buffer-local 'shell-program)

(defvar shell-program-args nil
  "The arguments to give to the program when it's started.")
(make-variable-buffer-local 'shell-program-args)

(defvar shell-prompt-regexp "^[^\]#$%>\)]*[\]#$%>\)] *"
  "A regexp matching the prompt of the shell.")
(make-variable-buffer-local 'shell-prompt-regexp)

(defvar shell-callback-function nil
  "Holds the function to call when the process changes state.")
(make-variable-buffer-local 'shell-callback-function)

(defvar shell-output-stream nil
  "Stream to output to from subprocess. If nil the process' buffer is
written to. This is only consulted when the process is started.")
(make-variable-buffer-local 'shell-output-stream)

(defvar shell-output-filter nil
  "Function called to filter process output strings before they're printed
to the buffer. Only used when shell-output-stream is nil.")
(make-variable-buffer-local 'shell-output-filter)

(defvar shell-output-limit 500
  "When non-nil, the maximum number of lines allowed in a buffer using
shell-mode. Only used when shell-output-stream is nil.")
(make-variable-buffer-local 'shell-output-limit)

(defvar shell-echos nil
  "When non-nil, all commands sent to an interactive shell will be deleted
from the buffer before being sent (since the shell will echo them anyway)")
(make-variable-buffer-local 'shell-echos)


(defvar shell-process nil
  "The process that the Shell mode created in the current buffer.")
(make-variable-buffer-local 'shell-process)

(defvar shell-last-output nil
  "The position in the buffer after the last character output by the process.")
(make-variable-buffer-local 'shell-last-output)

(defvar shell-keymap
  (bind-keys (make-sparse-keymap)
    "Ctrl-a" 'shell-bol
    "Ctrl-d" 'shell-del-or-eof
    "RET" 'shell-enter-line)
  "Keymap for shell-mode.")

(defvar shell-ctrl-c-keymap
  (bind-keys (make-sparse-keymap)
    "Ctrl-c" 'shell-send-intr
    "Ctrl-z" 'shell-send-susp
    "Ctrl-d" 'shell-send-eof
    "Ctrl-n" 'shell-next-prompt
    "Ctrl-p" 'shell-prev-prompt
    "Ctrl-\\" 'shell-send-quit)
  "Keymap for ctrl-c in shell-mode.")

;; Ensure that the termcap stuff is set up correctly
(setenv "TERM" "jade")
(setenv "TERMCAP" "jade:tc=unknown:")


;;;###autoload
(defun shell-mode ()
  "Shell Mode:\n
Major mode for running a subprocess in a buffer. Local bindings are:\n
\\{shell-keymap}\\{shell-ctrl-c-keymap,Ctrl-c}"
  (set! local-keymap 'shell-keymap)
  (set! local-ctrl-c-keymap shell-ctrl-c-keymap)
  (set! mode-name "Shell")
  (set! major-mode 'shell-mode)
  (set! major-mode-kill shell-mode-kill)
  (add-hook 'kill-buffer-hook shell-kill-buffer-hook)
  (shell-start-process)
  (call-hook 'shell-mode-hook))

(defun shell-mode-kill ()
  (shell-kill-buffer-hook)
  (kill-all-local-variables))

(defun shell-kill-buffer-hook ()
  (when (and shell-process (process-in-use? shell-process))
    (unless (yes-or-no-p "Shell subprocess running; kill it?")
      (error "Can't kill shell without killing its subprocess"))
    ;; don't want the callback function to run or to output
    (set-process-function! shell-process nil)
    (set-process-output-stream! shell-process nil)
    (set-process-error-stream! shell-process nil)
    (kill-process shell-process nil)))


;; If a shell subprocess isn't running create one
(defun shell-start-process ()
  (unless (and shell-process (process-in-use? shell-process))
    (let ((buffer (current-buffer)))
      (set! shell-process (make-process
			   (or shell-output-stream
			       (lambda (o)
				 (with-buffer buffer
				   (shell-filter o))))
			   ;; Create a function which switches to the
			   ;; process' buffer then calls the callback
			   ;; function (through its variable)
			   (lambda ()
			     (with-buffer buffer
			       (shell-callback-function)))
			   nil shell-program shell-program-args))
      (set-process-connection-type! shell-process 'pty)
      (start-process shell-process))))

;; The default value of shell-callback-function
(defun shell-default-callback ()
  (when shell-process
    (insert (cond
	     ((process-stopped? shell-process)
	      "\nProcess suspended...")
	     ((process-running? shell-process)
	      "restarted\n")
	     (t
	      (set! shell-process nil)
	      "\nProcess terminated\n")))))

(set-default 'shell-callback-function shell-default-callback)

;; Default output stream
(defun shell-filter (output)
  (goto (end-of-buffer))
  (unless (string? output)
    (set! output (make-string 1 output)))
  (when shell-output-filter
    (set! output (shell-output-filter output)))
  (when output
    (insert output)
    (when (and shell-output-limit (> (buffer-length) shell-output-limit))
      (delete-area (start-of-buffer) (forward-line (- (buffer-length)
						      shell-output-limit)
						   (start-of-buffer))))
    (set! shell-last-output (end-of-buffer))))


;; Commands

(defun shell-bol ()
  "Go to the beginning of this shell line (but after the prompt)."
  (interactive)
  (if (looking-at shell-prompt-regexp (start-of-line))
      (goto (match-end))
    (goto (start-of-line))))

(defun shell-del-or-eof (count)
  "When at the very end of the buffer send the subprocess the EOF character,
otherwise delete the first COUNT characters under the cursor."
  (interactive "p")
  (if (equal? (cursor-pos) (end-of-buffer))
      (shell-send-eof)
    (delete-char count)))

(defun shell-enter-line ()
  "Send the current line to the shell process. If the current line is not the
last in the buffer the current command is copied to the end of the buffer."
  (interactive)
  (if (null? shell-process)
      (insert "\n")
    (if (and shell-last-output (<= shell-last-output (cursor-pos)))
	;; Send everything after the last output text
	(progn
	  (insert "\n")
	  (write shell-process
		 ((if shell-echos cut-area copy-area)
		  shell-last-output (cursor-pos))))
      ;; Try to identify a command on the current line
      (let
	  ((start (if (looking-at shell-prompt-regexp (start-of-line))
		      (match-end)
		    (start-of-line)))
	   cmdstr)
	(if (= (pos-line start) (1- (buffer-length)))
	    ;; last line in buffer
	    (progn
	      (when shell-whole-line
		(goto (end-of-line)))
	      (insert "\n")
	      (set! cmdstr ((if shell-echos cut-area copy-area)
			    start (cursor-pos))))
	  ;; copy the command at this line to the end of the buffer
	  (set! cmdstr (copy-area start (forward-line 1 (start-of-line))))
	  (set-auto-mark)
	  (goto (end-of-buffer))
	  (unless shell-echos
	    (insert cmdstr)))
	(write shell-process cmdstr)))))

(defun shell-send-intr ()
  (interactive)
  (write shell-process #\ETX))		;^C

(defun shell-send-susp ()
  (interactive)
  (write shell-process #\SUB))		;^Z

(defun shell-send-eof ()
  (interactive)
  (write shell-process #\EOT))		;^D

(defun shell-send-quit ()
  (interactive)
  (write shell-process #\FS))		;^SPC

(defun shell-next-prompt ()
  (interactive)
  (when (re-search-forward shell-prompt-regexp (end-of-line))
    (goto (match-end))))

(defun shell-prev-prompt ()
  (interactive)
  (when (re-search-backward shell-prompt-regexp
			  (forward-char -1 (start-of-line)))
    (goto (match-end))))


;;;###autoload
(defun shell ()
  "Run a subshell in a buffer called `*shell*' using the major mode
`shell-mode'."
  (interactive)
  (let
      ((buffer (get-buffer "*shell*")))
    (if (or (not buffer) (with-buffer buffer shell-process))
	(progn
	  (goto-buffer (open-buffer "*shell*" t))
	  (shell-mode))
      (goto-buffer buffer)
      (shell-start-process))))


;; Running shell commands non-interactively

;;;###autoload
(defun shell-command (command #!optional insertp)
  "Run the shell command string COMMAND using the shell named by the
variable `shell-file-name'. Leave the output in the buffer *shell-output*
unless INSERTP is t in which case it's inserted in the current buffer, or
the output is a single line in which case it goes to the status area.

This simply calls shell-command-on-area with a nil input region, and with
the DELETEP parameter also nil."
  (interactive "sShell command:\nP")
  (shell-command-on-area command (end-of-buffer) (end-of-buffer) nil insertp))

;;;###autoload
(defun shell-command-on-buffer (command #!optional replacep)
  "Run the shell command string COMMAND using the shell named by the
variable `shell-file-name' with the contents of the current buffer as its
standard input. If REPLACEP is non-nil the output of the command replaces
the current contents of the buffer; otherwise output is sent to the
*shell-output* buffer. See `shell-command-on-area' for more details."
  (interactive "sShell command on buffer:\nP")
  (shell-command-on-area command
			 (start-of-buffer) (end-of-buffer)
			 replacep replacep))

;;;###autoload
(defun shell-command-on-area (command start end #!optional deletep insertp)
  "Run the shell command string COMMAND using the shell named by the variable
`shell-file-name', giving the area of the current buffer from START to END
as its standard input. If DELETEP is t the input area will be deleted.

What happens to the output depends on the INSERTP and DELETEP arguments:

  1. INSERTP nil. Output to the *shell-output* buffer or the status line.
  2. INSERTP t, DELETEP nil. Output goes to the current position of the
     current buffer
  3. Both t. Output replaces the input area START to END.

If standard output is directed to the *shell-output* buffer this buffer
will be displayed in the other view, unless it's a single line in which case
it's displayed in the status line.

If INSERTP is t only standard *output* is redirected, standard error is
sent to the buffer *shell-errors* which will be displayed in a view if
there is something to display; when INSERTP is nil standard error is sent
to the *shell-output* buffer.

When called interactively a non-nil prefix argument means both insert and
delete, i.e. replace the marked area with the output of the command."
  (interactive "-sShell command on block:\nm\nM\nP\nP")
  (let*
      ((output (if insertp
		   (if (and insertp deletep)
		       (cons (current-buffer) start)
		     (current-buffer))
		 (open-buffer "*shell-output*")))
       (proc (make-process output nil nil shell-file-name (list "-c" command)))
       error-output
       result
       used-message)
    (unless insertp
      (clear-buffer output))
    (when insertp
      (set! error-output (open-buffer "*shell-errors*"))
      (clear-buffer error-output)
      (set-process-error-stream! proc error-output))
    (set! result (if (equal? start end)
		     (call-process proc)
		   (call-process-area proc start end deletep)))
    (unless insertp
      (set-buffer-modified output nil)
      (if (= (buffer-length output) 2)
	  (progn
	    (message (copy-area (start-of-buffer output)
				(end-of-buffer output)
				output))
	    (set! used-message t))
	(unless (equal? (start-of-buffer output) (end-of-buffer output))
	  (if (eq? (current-buffer) output)
	      (progn
		(goto (start-of-buffer))
		(shrink-view-if-larger-than-buffer))
	    (with-view (other-view)
	      (goto-buffer output)
	      (goto (start-of-buffer))
	      (shrink-view-if-larger-than-buffer))))))
    (when (and error-output
	       (not (equal? (start-of-buffer error-output)
			   (end-of-buffer error-output))))
      (set-buffer-modified error-output nil)
      (if (and (= (buffer-length error-output) 2)
	       (not used-message))
	  (progn
	    (message (copy-area (start-of-buffer error-output)
				(end-of-buffer error-output)
				error-output))
	    (set! used-message t))
	(unless insertp
	  (goto-buffer output))
	(with-view (other-view)
	  (goto-buffer error-output)
	  (goto (start-of-buffer))
	  (shrink-view-if-larger-than-buffer))))
    (unless used-message
      (format t "Command returned %d" result))))
