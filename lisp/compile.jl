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


;; Variables

(setq gcc-error-regexp "^(.*):([0-9]+):(.+)"
      gcc-file-expand "\\1"
      gcc-line-expand "\\2"
      gcc-error-expand "\\3")

(defvar compile-error-regexp gcc-error-regexp
  "String used to match error messages in compilation output.")
(defvar compile-file-expand gcc-file-expand
  "Expansion template for the name of the file in an error matched by
`compile-error-regexp'.")
(defvar compile-line-expand gcc-line-expand
    "Expansion template for the line number of an error matched by
`compile-error-regexp'.")
(defvar compile-error-expand gcc-error-expand
    "Expansion template for the error description in an error matched by
`compile-error-regexp'.")

(defvar compile-keymap (make-keylist))

(unless (boundp 'compile-buffer)
  (bind-keys compile-keymap
    "Ctrl-c" 'kill-compilation
    "Ctrl-z" 'stop-compilation
    "Ctrl-f" 'continue-compilation
    "r" 'restart-compile-command
    "Ctrl-r" 'compile))

(defvar compile-buffer nil)
(defvar compile-proc nil)
(defvar compile-errors nil "List of (ERROR-POS-MARK . ERROR-DESC-LINE)")
(defvar compile-error-pos nil)
(defvar compile-error-parsed-errors-p nil)
(defvar compile-errors-exist-p nil)

(defvar compile-command "make"
  "The command to run from `M-x compile'.")

(defvar compile-last-command nil
  "The previous command run in the *compilation* buffer.")

(defvar compile-last-type nil
  "The type of the last command.")

(defvar compile-shell (unless (getenv "SHELL") "/bin/sh")
  "The filename of the shell to use to run the compilation in.")

(defvar compile-last-grep "grep -n "
  "The last string entered to the `M-x grep' command.")


;; Compilation

(defun compile-init ()
  (if compile-buffer
      (clear-buffer compile-buffer)
    (setq compile-buffer (make-buffer "*compilation*"))
    (with-buffer compile-buffer
      (setq ctrl-c-keymap compile-keymap))
    (set-buffer-special compile-buffer t))
  (when compile-buffer
    (add-buffer compile-buffer)
    (set-buffer-file-name compile-buffer
			  (file-name-directory (buffer-file-name)))
    (setq compile-errors nil
	  compile-parsed-errors-p nil
	  compile-errors-exists-p nil
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
				     'compile-callback
				     (file-name-directory (buffer-file-name))))
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
  (start-compile-command compile-command compile-last-type))

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
    ;; from hanging on standard input
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

(defun compile-parse-errors ()
  ;; This can be called while the process is still running, one problem though,
  ;; if the compiled file is modified and then a new error is found the line
  ;; numbers won't coincide like they normally would.
  (unless compile-parsed-errors-p
    (with-buffer compile-buffer
      (let*
	  (error-file
	   error-line
	   last-e-line
	   last-e-file
	   new-errors)
	(while (setq compile-error-pos (re-search-forward compile-error-regexp
							 compile-error-pos))
	  (setq error-line (1- (read-from-string
				(expand-last-match compile-line-expand))))
	  (when (or (not last-e-line) (/= error-line last-e-line))
	    (setq last-e-line error-line
		  error-file (file-name-concat
			      (buffer-file-name)
			      (expand-last-match compile-file-expand)))
	    (if (equal last-e-file error-file)
		(setq error-file last-e-file)
	      (setq last-e-file error-file))
	    (setq new-errors (cons (cons (make-mark (pos 0 error-line)
						    error-file)
					 (pos-line compile-error-pos))
				   new-errors)))
	  (setq compile-error-pos (match-end)))
	(when new-errors
	  (setq compile-errors (nconc compile-errors (nreverse new-errors))
		compile-errors-exist-p t))))
    (unless compile-proc
      (setq compile-parsed-errors-p t)))
  t)

;;;###autoload
(defun next-error ()
  "Moves the cursor to the file and line of the next error displayed in the
`*compilation*' buffer."
  (interactive)
  (compile-parse-errors)
  (let*
      ((err (car compile-errors)))
    (setq compile-errors (cdr compile-errors))
    (cond
     ((not err)
      (message (concat "No " (if compile-errors-exist-p "more ")
		       compile-last-type (if compile-proc " yet")))
      (beep)
      nil)
     (t
      (goto-mark (car err))
      (when (cdr err)
	(and (looking-at compile-error-regexp (pos 0 (cdr err)) compile-buffer)
	     (message (expand-last-match compile-error-expand))))
      t))))
