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

(defvar compile-push-directory-regexp
  "^make(\\[[0-9]+\\])?: Entering directory `(.*)'"
  "Regexp matching output when a new directory is entered. See also the
variable `compile-push-directory-expand'.")

(defvar compile-push-directory-expand "\\2"
  "Regexp expansion string for `compile-push-directory-regexp'.")

(defvar compile-pop-directory-regexp "^make(\\[[0-9]+\\])?: Leaving directory "
  "Regexp matching output when leaving a directory.")

(defvar compile-command "make -w"
  "The command to run from `M-x compile'.")

(defvar compile-shell (or (getenv "SHELL") "/bin/sh")
  "The filename of the shell to use to run the compilation in.")


;; Variables

(defvar compile-keymap 
  (bind-keys (make-sparse-keymap)
    "Ctrl-c" 'kill-compilation
    "Ctrl-z" 'stop-compilation
    "Ctrl-f" 'continue-compilation
    "r" 'restart-compile-command
    "Ctrl-r" 'compile))

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

(defun compile-setup-buffer (directory)
  (unless directory
    (set! directory *default-directory*))
  (let ((buffer (open-buffer "*compilation*")))
    (clear-buffer buffer)
    (with-buffer buffer
      (set! *default-directory* directory))
    (set! compile-errors nil)
    (set! compile-parsed-errors nil)
    (set! compile-errors-exist nil)
    (set! compile-error-pos (start-of-buffer))
    buffer))

(defun compile-last-directory ()
  (let ((buffer (get-buffer "*compilation*")))
    (and buffer (with-buffer buffer *default-directory*))))

(defun compile-callback ()
  (when compile-proc
    (let ((stream (process-output-stream compile-proc)))
      (cond ((process-stopped? compile-proc)
	     (write stream "Compilation suspended..."))
	    ((process-running? compile-proc)
	     (write stream "restarted\n"))
	    (t
	     (beep)
	     (if (process-exit-value compile-proc)
		 (format stream "\nCompilation exited with value %d\n"
			 (process-exit-value compile-proc))
	       (format stream "\nCompilation exited abnormally: status 0x%x\n"
		       (process-exit-status compile-proc)))
	     (set! compile-proc nil))))))

;;;###autoload
(defun start-compile-command (command directory type-str)
  "Executes SHELL-COMMAND asynchronously in DIRECTORY. Output from the
process is sent to the `*compilation*' buffer. TYPE-STR is a string
describing the type of messages the command may output (i.e. `errors'
for a compilation)."
  (if compile-proc
      (error "Compilation process already running")
    (save-some-buffers)
    (goto-buffer (compile-setup-buffer directory))
    (set! compile-proc (make-process (cons (current-buffer) t)
				     compile-callback))
    (let ((shell-cmd (concat command #\newline)))
      (insert shell-cmd)
      (when (start-process compile-proc compile-shell "-c" shell-cmd)
	(set! compile-last-type type-str)
	(set! compile-last-command command)
	compile-proc))))

(defun restart-compile-command ()
  "Rerun the previous command started in the *compilation* buffer."
  (interactive)
  (start-compile-command
   compile-last-command (compile-last-directory) compile-last-type))

(defun kill-compilation ()
  (interactive)
  (when compile-proc
    (interrupt-process compile-proc t)))

(defun stop-compilation ()
  (interactive)
  (when (process-running? compile-proc)
    (stop-process compile-proc t)))

(defun continue-compilation ()
  (interactive)
  (when (process-stopped? compile-proc)
    (continue-process compile-proc t)))

;;;###autoload
(defun compile (command)
  "Runs the COMMAND in the `*compilation*' buffer."
  (interactive (list (prompt-for-string "Compile command:" compile-command)))
  (set! compile-command command)
  (start-compile-command command *default-directory* "errors"))

;;;###autoload
(defun compile-directory (directory command)
  "Runs the COMMAND in the `*compilation*' buffer."
  (interactive (list (prompt-for-directory
		      "Compile directory:" t (compile-last-directory))
		     (prompt-for-string "Compile command:" compile-command)))
  (set! compile-command command)
  (start-compile-command command directory "errors"))


;; Grepping

;;;###autoload
(defun grep (arg)
  "Runs the shell command ARG (or the result of a prompt) and sends its
output to the `*compilation*' buffer. The process may still be executing
when this function returns."
  (interactive (list (prompt-for-string "Run grep command:"
					compile-last-grep)))
  (grep-directory arg))

;;;###autoload
(defun grep-directory (arg #!optional (directory *default-directory*))
  "Run M-x grep in a particular directory."
  (interactive (list (prompt-for-string "Run grep command:"
					compile-last-grep)
		     (prompt-for-directory
		      "In directory:" t (compile-last-directory))))
  (when arg
    (set! compile-last-grep arg)
    ;; Concat "/dev/null /dev/null" To stop a null command (i.e. "grep")
    ;; hanging on standard input
    (start-compile-command
     (concat arg " /dev/null /dev/null") directory "grep-hits")))

(defvar grep-buffer-regexp nil
  "Regular-expression which `grep-buffer' scans for")

;;;###autoload
(defun grep-buffer (#!optional regexp)
  "Scans the current buffer for all matches of REGEXP (or the contents of
variable `grep-buffer-regexp'). All hits are displayed in the `*compilation*'
buffer in a form that `goto-next-error' understands."
  (interactive "sRegular expression")
  (when regexp
    (set! grep-buffer-regexp regexp))
  (let ((buffer (compile-setup-buffer *default-directory*)))
    (when (and grep-buffer-regexp buffer)
      (let* ((scanpos (start-of-buffer))
	     (number 0)
	     (stream (cons buffer t)))
	(write stream #\newline)
	(let loop ()
	  (set! scanpos (re-search-forward grep-buffer-regexp scanpos))
	  (when scanpos
	    (format stream "%s:%d:%s\n"
		    (or (buffer-file-name) (buffer-name)) (pos-line scanpos)
		    (copy-area (start-of-line scanpos) (end-of-line scanpos)))
	    (set! number (+ number 1))
	    (set! scanpos (end-of-line scanpos))
	    (loop)))
	(goto-buffer buffer)
	(goto (start-of-buffer))
	number))))


;; Parsing compiler/grep output

;; This can be called while the process is still running, one problem though,
;; if the compiled file is modified and then a new error is found the line
;; numbers won't coincide like they normally would.
(defun compile-parse-errors ()
  (unless compile-parsed-errors
    (with-buffer (get-buffer "*compilation*")
      (let ((p compile-error-pos)
	    (dir-stack nil)
	    (current-dir *default-directory*)
	    errors last-line last-file)
	(while (/= (pos-line p) (buffer-length))
	  (cond ((looking-at compile-error-regexp p)
		 ;; Parse the error
		 (let ((line (string->number
			      (expand-last-match compile-line-expand)))
		       (file (expand-file-name
			      (expand-last-match compile-file-expand)
			      current-dir)))
		   (unless (and last-line (= line last-line)
				(file-name= file last-file))
		     (set! errors (cons (cons (make-mark
					       (pos 0 (1- line)) file)
					      (pos-line p))
					errors))
		     (set! last-line line)
		     (set! last-file file))))
		((looking-at compile-push-directory-regexp p)
		 (set! dir-stack (cons current-dir dir-stack))
		 (set! current-dir (expand-file-name
				    (expand-last-match
				     compile-push-directory-expand)
				    current-dir)))
		((and (looking-at compile-pop-directory-regexp p) dir-stack)
		 (set! current-dir (car dir-stack))
		 (set! dir-stack (cdr dir-stack))))
	  (set! p (forward-line 1 p)))
	(when errors
	  (set! compile-errors (append! (reverse! errors)))
	  (set! compile-errors-exist t))
	(if compile-proc
	    (set! compile-error-pos p)
	  (set! compile-parsed-errors t))))))

;;;###autoload
(defun next-error ()
  "Moves the cursor to the file and line of the next error displayed in the
`*compilation*' buffer."
  (interactive)
  (compile-parse-errors)
  (if compile-errors
      (let ((err (car compile-errors)))
	(goto-mark (car err))
	(when (and (cdr err)
		   (looking-at compile-error-regexp (pos 0 (cdr err))
			       (get-buffer "*compilation*")))
	  (message (expand-last-match compile-error-expand)))
	(set! compile-errors (cdr compile-errors)))
    (error "No %s%s%s"
	   (if compile-errors-exist "more " "")
	   (or compile-last-type "")
	   (if compile-proc " yet" ""))))
