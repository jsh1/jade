;;;; compile.jl -- Running compilation processes
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

(provide 'compile)


;; Configuration

(defvar compile-error-regexp "^(.*):([0-9]+):(.+)"
  "String used to match error messages in compilation output.")

(defvar compile-file-expand "\\1"
  "Expansion template for the name of the file in an error matched by
`compile-error-regexp'.")

(defvar compile-line-expand "\\2"
  "Expansion template for the line number of an error matched by
`compile-error-regexp'.")

(defvar compile-error-expand "\\3"
  "Expansion template for the error description in an error matched by
`compile-error-regexp'.")

(defvar compile-push-directory-regexp "^make: Entering directory `(.*)'"
  "Regexp matching output when a new directory is entered. First
subexpression should contain the name of the directory.")

(defvar compile-pop-directory-regexp "^make: Leaving directory "
  "Regexp matching output when leaving a directory.")

(defvar compile-command "make"
  "The command to run from `M-x compile'.")

(defvar compile-shell (unless (getenv "SHELL") "/bin/sh")
  "The filename of the shell to use to run the compilation in.")


;; Variables

(defvar compile-keymap 
  (bind-keys (make-sparse-keymap)
    "Ctrl-c" 'kill-compilation
    "Ctrl-z" 'stop-compilation
    "Ctrl-f" 'continue-compilation
    "r" 'restart-compile-command
    "Ctrl-r" 'compile))

(defvar compile-buffer nil)
(defvar compile-proc nil)
(defvar compile-errors nil "List of (ERROR-POS-MARK . ERROR-DESC-LINE)")
(defvar compile-error-pos nil)
(defvar compile-parsed-errors nil)
(defvar compile-errors-exist nil)

(defvar compile-last-command nil
  "The previous command run in the *compilation* buffer.")

(defvar compile-last-type nil
  "The type of the last command.")

(defvar compile-last-grep "grep -n "
  "The last string entered to the `M-x grep' command.")


;; Compilation

(defun compile-init ()
  (let
      ((original-dir default-directory))
    (if compile-buffer
	(clear-buffer compile-buffer)
      (setq compile-buffer (open-buffer "*compilation*"))
      (with-buffer compile-buffer
	(setq ctrl-c-keymap compile-keymap)))
    (with-buffer compile-buffer
      (setq default-directory original-dir)))
  (when compile-buffer
    (setq compile-errors nil
	  compile-parsed-errors nil
	  compile-errors-exist nil
	  compile-error-pos (start-of-buffer))
    t))

(defun compile-callback ()
  (when compile-proc
    (cond
     ((process-stopped-p compile-proc)
      (write (cons compile-buffer t) "Compilation suspended..."))
     ((process-running-p compile-proc)
      (write (cons compile-buffer t) "restarted\n"))
     (t
      (beep)
      (if (process-exit-value compile-proc)
	  (format (cons compile-buffer t) "\n%s%d\n"
		  "Compilation exited with value "
		  (process-exit-value compile-proc))
	(format (cons compile-buffer t) "\n%s%x\n"
		"Compilation exited abnormally: status 0x"
		(process-exit-status compile-proc)))
      (setq compile-proc nil)))))

;;;###autoload
(defun start-compile-command (command type-str)
  "Executes SHELL-COMMAND asynchronously in the directory containing the file
being edited in the current buffer. Output from the process is sent to the
`*compilation*' buffer. TYPE-STR is a string describing the type of messages
the command may output (i.e. `errors' for a compilation)."
  (if compile-proc
      (error "Compilation process already running")
    (save-some-buffers)
    (compile-init)
    (goto-buffer compile-buffer)
    (setq compile-proc (make-process (cons compile-buffer t)
				     'compile-callback))
    (let
	((shell-cmd (concat command ?\n)))
      (write compile-buffer shell-cmd)
      (when (start-process compile-proc compile-shell "-c" shell-cmd)
	(setq compile-last-type type-str
	      compile-last-command command)
	compile-proc))))

(defun restart-compile-command ()
  "Rerun the previous command started in the *compilation* buffer."
  (interactive)
  (start-compile-command compile-last-command compile-last-type))

(defun kill-compilation ()
  (interactive)
  (when compile-proc
    (interrupt-process compile-proc t)))

(defun stop-compilation ()
  (interactive)
  (when (process-running-p compile-proc)
    (stop-process compile-proc t)))

(defun continue-compilation ()
  (interactive)
  (when (process-stopped-p compile-proc)
    (continue-process compile-proc t)))

;;;###autoload
(defun compile (command)
  "Runs the COMMAND in the `*compilation*' buffer."
  (interactive (list (prompt-for-string "Compile command:" compile-command)))
  (setq compile-command command)
  (start-compile-command command "errors"))


;; Grepping

;;;###autoload
(defun grep (arg)
  "Runs the shell command ARG (or the result of a prompt) and sends its
output to the `*compilation*' buffer. The process may still be executing
when this function returns."
  (interactive (list (prompt-for-string "Run grep command:"
					compile-last-grep)))
  (when arg
    (setq compile-last-grep arg)
    ;; Concat "/dev/null /dev/null" To stop a null command (i.e. "grep")
    ;; hanging on standard input
    (start-compile-command (concat arg " /dev/null /dev/null") "grep-hits")))

(defvar grep-buffer-regexp nil
  "Regular-expression which `grep-buffer' scans for")

;;;###autoload
(defun grep-buffer (&optional regexp)
  "Scans the current buffer for all matches of REGEXP (or the contents of
variable `grep-buffer-regexp'). All hits are displayed in the `*compilation*'
buffer in a form that `goto-next-error' understands."
  (interactive "sRegular expression")
  (when regexp
    (setq grep-buffer-regexp regexp))
  (when (and grep-buffer-regexp (compile-init))
    (let*
	((scanpos (start-of-buffer))
	 (number 0)
	 (stream (cons compile-buffer t)))
      (write stream ?\n)
      (while (setq scanpos (re-search-forward grep-buffer-regexp scanpos))
	(format stream "%s:%d:%s\n" (buffer-file-name) (pos-line scanpos)
		(copy-area (start-of-line scanpos) (end-of-line scanpos)))
	(setq number (+ number 1)
	      scanpos (end-of-line scanpos)))
      (goto-buffer compile-buffer)
      (goto (start-of-buffer))
      number)))


;; Parsing compiler/grep output

;; This can be called while the process is still running, one problem though,
;; if the compiled file is modified and then a new error is found the line
;; numbers won't coincide like they normally would.
(defun compile-parse-errors ()
  (unless compile-parsed-errors
    (with-buffer compile-buffer
      (let
	  ((pos compile-error-pos)
	   (dir-stack nil)
	   (current-dir default-directory)
	   errors last-line last-file)
	(while (/= (pos-line pos) (buffer-length))
	  (cond
	   ((looking-at compile-error-regexp pos)
	    ;; Parse the error
	    (let
		((line (read-from-string
			(expand-last-match compile-line-expand)))
		 (file (expand-file-name
			(expand-last-match compile-file-expand)
			current-dir)))
	      (unless (and last-line (= line last-line)
			   (file-name= file last-file))
		(setq errors (cons (cons (make-mark (pos 0 (1- line)) file)
					 (pos-line pos))
				   errors)
		      last-line line
		      last-file file))))
	   ((looking-at compile-push-directory-regexp pos)
	    (setq dir-stack (cons current-dir dir-stack)
		  current-dir (expand-file-name (expand-last-match "\\1")
						current-dir)))
	   ((and (looking-at compile-pop-directory-regexp pos) dir-stack)
	    (setq current-dir (car dir-stack)
		  dir-stack (cdr dir-stack))))
	  (setq pos (forward-line 1 pos)))
	(when errors
	  (setq compile-errors (nconc (nreverse errors))
		compile-errors-exist t))
	(if compile-proc
	    (setq compile-error-pos pos)
	  (setq compile-parsed-errors t))))))

;;;###autoload
(defun next-error ()
  "Moves the cursor to the file and line of the next error displayed in the
`*compilation*' buffer."
  (interactive)
  (compile-parse-errors)
  (if compile-errors
      (let
	  ((error (car compile-errors)))
	(goto-mark (car error))
	(when (and (cdr error)
		   (looking-at compile-error-regexp (pos 0 (cdr error))
			       compile-buffer))
	  (message (expand-last-match compile-error-expand)))
	(setq compile-errors (cdr compile-errors)))
    (error "No %s%s%s"
	   (if compile-errors-exist "more " "")
	   (or compile-last-type "")
	   (if compile-proc " yet" ""))))
