;;;; telnet.jl -- Telnet/rlogin sessions-in-a-buffer
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

(defvar rlogin-program "rlogin"
  "Program to start for an rlogin session.")

(defvar telnet-grab-passwords t
  "When t, any line of output matching telnet-password-regexp will be
treated as a password prompt. The password will be prompted for using a
secure mechanism, then sent to the subprocess.

If this value is an integer N, passwords will only be recognised in the
first N lines of the buffer.")

(defvar telnet-password-regexp "Password: *$"
  "Regexp matching what the remote host uses to prompt for passwords.")

(defvar telnet-echos t
  "Should be t when the remote telnet or rlogin will echo back all input
sent to it.")

(defvar telnet-prompt-regexp "^[^#$%>\n]*[#$%>] *"
  "Regexp matching a prompt from the remote telnet or rlogin session.")


;; Program variables

(defvar telnet-using-rlogin nil
  "Non-nil when the session in this buffer is actually using rlogin.")
(make-variable-buffer-local 'telnet-using-rlogin)


;; Functions

;; Initialise a telnet buffer. ARGS is the process arg list, DIR the
;; new value of default-directory
(defun telnet-init (host arg dir use-rlogin)
  (setq default-directory dir
	shell-program (if use-rlogin rlogin-program telnet-program)
	telnet-using-rlogin use-rlogin
	shell-prompt-regexp telnet-prompt-regexp
	shell-program-args (if arg
			       (if use-rlogin
				   (list "-l" arg host)
				 (list host (format nil "%d" arg)))
			     (list host))
	shell-echos telnet-echos
	shell-output-filter telnet-filter)
  (call-hook 'telnet-mode-hook))

;;;###autoload
(defun telnet (host &optional arg use-rlogin)
  "Start a telnet session, connecting to the remote system called HOST,
optionally using port number ARG. When USE-RLOGIN is t, use the rlogin
mechanism, not telnet, in this case ARG may be a string specifying the
user name to use.

When called interactively, HOST will be prompted for, ARG will only be
prompted for if a prefix argument is given."
  (interactive
   (let
       ((arg current-prefix-arg))
     (list (prompt-for-string "Host:")
	   (and arg (prompt-for-number "Port:")))))
  (let*
      ((name (concat (if use-rlogin "*rlogin-" "*telnet-")
		     (if arg
			 (if use-rlogin
			     (format nil "%s@%s" arg host)
			   (format nil "%s:%d" host arg))
		       host)
		     "*"))
       (buffer (get-buffer name))
       (dir default-directory))
    (goto-other-view)
    (if (or (not buffer) (with-buffer buffer shell-process))
	(progn
	  (goto-buffer (open-buffer name t))
	  (telnet-init host arg dir use-rlogin)
	  (shell-mode)
	  (setq major-mode 'telnet-mode
		mode-name (if use-rlogin "rlogin" "Telnet")))
      (goto-buffer buffer)
      (clear-buffer)
      (telnet-init host arg dir use-rlogin)
      (shell-start-process))))

;;;###autoload
(defun rlogin (host &optional user)
  "Use the rlogin command to connect to run a shell on the remote host HOST,
optionally as the user called USER. When called interactively, HOST will
be prompted for, USER will only be prompted for if a prefix argument is
given. See the `telnet' function for more details."
  (interactive
   (let
       ((arg current-prefix-arg))
     (list (prompt-for-string "Host:")
	   (and arg (prompt-for-string "User:")))))
  (telnet host user t))

;; All output from the telnet process goes though this function, by
;; the time it's called the current buffer will be the buffer associated
;; with the process associated with the output
(defun telnet-filter (output)
  ;; Delete any carriage returns
  (while (string-match "\r" output)
    (setq output (concat (substring output 0 (match-start))
			 (substring output (match-end)))))
  ;; Look for password prompts
  (when (and (or (eq telnet-grab-passwords t)
		 (< (pos-line (cursor-pos)) telnet-grab-passwords))
	     (string-match telnet-password-regexp output nil t))
    ;; Found a password, cancelling the prompt will _not_ send anything,
    ;; in case this isn't actually a password prompt..
    (insert output)
    (setq output nil)
    (let
	((pass (pwd-prompt "Password: (Ctrl-g to ignore)")))
      (when pass
	(write shell-process pass)
	(write shell-process ?\n))))
  output)
