;;;; telnet.jl -- Telnet session-in-a-buffer
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

(require 'shell)
(provide 'telnet)


;; Configuration

(defvar telnet-program "telnet"
  "Program to start for a telnet session.")

(defvar telnet-echos t
  "Should be t when the remote telnet will echo back all input sent to it.")

(defvar telnet-password-regexp "^Password: *"
  "Regexp matching the string found when the remote host prompts for
a password. When this is found, the password will be prompted for
confidentially, then sent to the system.")

(defvar telnet-prompt-regexp "^[^#$%>\n]*[#$%>] *"
  "Regexp matching a prompt from the remote telnet.")


;; Program

(defvar telnet-last-output nil
  "A position defining the end of the last output from the telnet process,
or nil.")
(make-variable-buffer-local 'telnet-last-output)

(defvar telnet-last-input nil
  "A position defining the end of the last input sent to the telnext process,
or nil.")
(make-variable-buffer-local 'telnet-last-input)

(defvar telnet-keymap (copy-sequence shell-keymap))
(unbind-keys telnet-keymap "RET")
(bind-keys telnet-keymap
  "RET" 'telnet-send-line)

;; Initialise a telnet buffer. ARG is the process argument, DIR the
;; new value of default-directory
(defun telnet-init (arg dir)
  (set-buffer-special nil t)
  (setq default-directory dir
	mildly-special-buffer t
	shell-program telnet-program
	shell-prompt-regexp telnet-prompt-regexp
	shell-program-args (list arg)
	shell-output-stream `(lambda (o) (with-buffer ,(current-buffer)
					   (funcall 'telnet-filter o)))))

;;;###autoload
(defun telnet (host &optional port)
  "Start a telnet session, connecting to the remote system called HOST,
optionally using port number PORT."
  (interactive (list (prompt-for-string "Host:")
		     (and current-prefix-arg (prompt-for-number "Port:"))))
  (let*
      ((buffer-name (concat "*telnet-" host "*"))
       (buffer (get-buffer buffer-name))
       (dir default-directory)
       (arg (if port
		(format nil "%s:%d" host port)
	      host)))
    (goto-other-view)
    (if (or (not buffer) (with-buffer buffer shell-process))
	(progn
	  (goto-buffer (open-buffer buffer-name t))
	  (telnet-init arg dir)
	  (shell-mode)
	  (setq major-mode 'telnet-mode)
	  (setq mode-name "Telnet"
		keymap-path (cons 'telnet-keymap (delq 'shell-keymap
						       keymap-path))))
      (goto-buffer buffer)
      (telnet-init arg dir)
      (shell-start-process))))

;; All output from the telnet process goes though this function, by
;; the time it's called the current buffer will be the buffer associated
;; with the process associated with the output
(defun telnet-filter (output)
  (goto (end-of-buffer))
  (unless (stringp output)
    (setq output (make-string 1 output)))
  (when (and telnet-echos telnet-last-input telnet-last-output
	     (> telnet-last-input telnet-last-output))
    (delete-area telnet-last-output telnet-last-input)
    (setq telnet-last-input nil))
  (let
      ((start (cursor-pos)))
    (setq telnet-last-output (insert output))
    (while (setq start (char-search-forward ?\r start))
      (delete-area start (forward-char 1 start))))
  (when (and (not (zerop (pos-col telnet-last-output)))
	     (looking-at telnet-password-regexp
			 (start-of-line telnet-last-output) nil t))
    ;; Found a password
    (write shell-process (concat (or (pwd-prompt "Password:")) ?\n))
    (setq telnet-last-output nil)))

(defun telnet-send-line ()
  "If the cursor is after the last output from the telnet subprocess, send
all text between the two positions to the subprocess. Otherwise, attempt
to identify a command on the current line, and send that."
  (interactive)
  (if (and telnet-last-output (<= telnet-last-output (cursor-pos)))
      (progn
	(insert "\n")
	(setq telnet-last-input (cursor-pos))
	(write shell-process (copy-area telnet-last-output telnet-last-input)))
    ;; Fall back to using shell-modes version, it might work
    (setq telnet-last-input nil)
    (shell-enter-line)))
