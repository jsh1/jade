;;;; html-display.jl -- Simple HTML browser mode
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

(provide 'html-display)

(defvar html-display-details nil
  "Alist defining certain details about the parsed document.")
(make-variable-buffer-local 'html-display-details)

(defvar html-display-map
  (bind-keys (make-sparse-keymap)
    "SPC" 'next-screen
    "BS" 'prev-screen
    "RET" 'html-display-select
    "LMB-Click2" 'html-display-select
    "MMB-Click1" 'goto-mouse
    "MMB-Off" 'html-display-mouse-select
    "RMB-Click1" 'kill-current-buffer
    "TAB" 'html-display-next-link
    "M-TAB" 'html-display-previous-link
    "M-?" 'html-display-describe-link
    "a" 'html-display-goto-anchor
    "l" 'kill-current-buffer
    "b" 'kill-current-buffer
    "q" 'html-display-quit
    "g" 'find-url))

(fset 'html-display-map 'keymap)
;;;###autoload (autoload-keymap 'html-display-map "html-display")

;;;###autoload
(defun html-display (source &optional url other-view)
  (interactive "bBuffer with HTML:\nsURL of document\nP")
  (let*
      ((output (make-buffer "*html-display*"))
       (details (html-decode source output)))
    (when (assq 'title details)
      (set-buffer-name output (cdr (assq 'title details))))
    (when (and (or (not url) (string= url "")) (buffer-file-name source))
      (setq url (concat "file:/"
			(canonical-file-name (buffer-file-name source)))))
    (when url
      (setq details (cons (cons 'url url) details)))
    (when (and (not (assq 'base details)) url)
      ;; Strip the URL to find the base
      (if (string-match "/[^/]*$" url)
	  (setq details (cons (cons 'base (substring
					   url 0 (1+ (match-start)))) details))
	(setq details (cons (cons 'base url) details))))
    (set-buffer-modified output nil)
    (set-buffer-read-only output t)
    (with-view (if other-view
		   (if (viewp other-view) other-view (other-view))
		 (current-view))
      (goto-buffer output)
      (goto (start-of-buffer))
      (setq html-display-details details)
      (setq mode-name "HTML-Display"
	    major-mode 'html-display-mode
	    local-keymap html-display-map)
      (call-hook 'html-display-hook))))

(defun html-display-current-link (&optional failable)
  (let
      ((e (get-extent)))
    ;; can't use extent-get since that searches up the stack
    (while (and e (not (memq 'html-anchor-params (extent-plist e))))
      (setq e (extent-parent e)))
    (or e (if failable
	      nil
	    (error "No link here!")))))

(defun html-display-quit ()
  (interactive)
  (let
      ((html-buffers (filter #'(lambda (b)
				 (with-buffer b
				   (eq major-mode 'html-display-mode)))
			     buffer-list)))
    (mapc 'kill-buffer html-buffers)))


;; Moving between links

(defun html-display-next-link (count)
  "Move to the COUNT'th next link part in the current buffer."
  (interactive "p")
  (while (> count 0)
    (let
	((start (cursor-pos))
	 tem)
      (setq tem (html-display-current-link t))
      (when tem
	(setq start (extent-end tem)))
      (goto (catch 'foo
	      (map-extents #'(lambda (e)
			       (when (and (setq tem (extent-get
						     e 'html-anchor-params))
					  (assq 'href tem))
				 (throw 'foo (extent-start e))))
			   start (end-of-buffer)))))
    (setq count (1- count)))
  (while (< count 0)
    (let
	((end (cursor-pos))
	 tem)
      (setq tem (html-display-current-link t))
      (when tem
	(setq end (forward-char -1 (extent-start tem))))
      (goto (let
		(last)
	      (map-extents #'(lambda (e)
			       (when (and (setq tem (extent-get
						     e 'html-anchor-params))
					  (assq 'href tem))
				 (setq last (extent-start e))))
			   (start-of-buffer) end)
	      last)))
    (setq count (1+ count)))
  (and (html-display-current-link t)
       (html-display-describe-link)))

(defun html-display-previous-link (count)
  "Move to the COUNT'th previous link part in the current buffer."
  (interactive "p")
  (html-display-next-link (- count)))

(defun html-display-goto-anchor (name)
  "Move to the anchor called NAME in the current HTML display."
  (interactive (list (let
			 ((prompt-list-fold-case t))
		       (prompt-from-list
			(mapcar 'car (cdr (assq 'anchors
						html-display-details)))
			"Anchor name:"))))
  (let
      ((anchor (assoc name (cdr (assq 'anchors html-display-details)))))
    (or anchor (error "No anchor called `%s'" name))
    (goto (cdr anchor))))


;; Selecting links

(defun html-display-select (&optional other-view)
  (interactive "P")
  (let*
      ((e (html-display-current-link))
       (params (extent-get e 'html-anchor-params))
       (href (cdr (or (assq 'href params) (error "Not a link!"))))
       (base (cdr (assq 'base html-display-details))))
    (cond ((string-match "^[a-z]+:" href))
	  ((string-match "^/" href)
	   ;; Relative to the root of the web site?
	   (or (string-match "^[a-z]+://[^/]+"
			     (or base (error "No known URL base")))
	       (error "Can't find root URL: %s" base))
	   (setq href (concat (expand-last-match "\\0") href)))
	  (t
	   ;; Relative to the current base
	   (setq href (concat (or base (error "No known URL base")) href))))
    (when other-view
      (goto-other-view))
    (find-url href)))

(defun html-display-mouse-select (&optional other-view)
  (interactive "P")
  (goto-mouse)
  (html-display-select other-view))

(defun html-display-select-other-view ()
  (interactive)
  (html-display-select t))

(defun html-display-describe-link ()
  (interactive)
  (let*
      ((e (html-display-current-link))
       (params (extent-get e 'html-anchor-params))
       (href (cdr (or (assq 'href params) (error "Not a link!")))))
    (message href)))