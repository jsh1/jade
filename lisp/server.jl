;;;; server.jl -- Lisp code for the edit server
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

(provide 'server)

(defvar server-open-window nil
  "Determines which window a client file gets displayed in. There are three
possible values,
  nil	- Use the current window
  other - Use the `other' window
  t	- Open a new window")

;;;###autoload
(defun server-open-file (file line-number)
  "This function is called by the editor's main event loop when a client
process asks us to edit a file -- its job is to load the specified file
into a new buffer and display it at line LINE-NUMBER."
  (let
      (buf win)
    (unless (setq buf (get-file-buffer file))
      (unless (setq buf (open-file file))
	(server-reply file 10)
	(return)))
    (setq win
      (cond
       ((eq server-open-window 'other)
	(other-window))
       ((null server-open-window)
	(current-window))
       (t
	(open-window))))
    (with-window win
      (goto-buffer buf)
      (goto (pos 0 line-number))
      (message (format nil "Client file `%s'." file)))
    (with-buffer buf
      (add-hook 'kill-buffer-hook 'server-file-kill))
    buf))

;; Hooked into kill-buffer, replies to the client if necessary
(defun server-file-kill (buf)
  (when (server-reply buf)
    (message (format nil "Client file `%s' finished."
		     (buffer-file-name buf))))
  t)

;;;###autoload
(defun server-close-file ()
  "Tell the client program which asked us to edit the file in the current
buffer that we've finished with it. *The file will NOT be saved* automatically
-- you're supposed to do that before calling this function if you want to."
  (interactive)
  (if (server-reply)
      (progn
	(remove-hook 'kill-buffer-hook 'server-file-kill)
	(format t "Client file `%s' replied to." (buffer-file-name)))
    (error "This buffer has no client!")))
