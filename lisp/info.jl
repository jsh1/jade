;;;; info.jl -- Info browser
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

(provide 'info)
(require 'prompt)

;;; Limitations:
;;; - No support for `*' node name.
;;; - Doesn't work 100% with info files formatted by emacs. For best results
;;;   makeinfo has to be used.
;;; - No editing of nodes.

(defvar info-directory-list '("/usr/info" "/usr/local/info/" "~/info")
  "List of directories to search for info files if they can't be found as-is.")

(defvar info-menu-face bold-face
  "Face used to highlight menu items in Info mode.")

(defvar info-xref-face underline-face
  "Face used to highlight cross references in Info mode.")

(defvar info-suffixes '(("" . nil)
			(".gz" . (auto-compression-mode t))
			(".Z" . (auto-compression-mode t)))
  "List of (SUFFIX . FORM) combinations. When searching for an info file try
each SUFFIX in turn. When one matches evaluate FORM so that the file can be
decoded (through the `read-file-hook').")

(defvar info-documentation-files '("librep"))
(make-variable-buffer-local 'info-documentation-files)

(defvar info-function-index-node "Function Index")
(make-variable-buffer-local 'info-function-index-node)

(defvar info-variable-index-node "Variable Index")
(make-variable-buffer-local 'info-variable-index-node)

(defvar info-keymap
  (bind-keys (make-keymap)
    "SPC" 'next-screen
    "BS" 'prev-screen
    "1" 'info-menu-nth
    "2" 'info-menu-nth
    "3" 'info-menu-nth
    "4" 'info-menu-nth
    "5" 'info-menu-nth
    "6" 'info-menu-nth
    "7" 'info-menu-nth
    "8" 'info-menu-nth
    "9" 'info-menu-nth
    "b" 'start-of-buffer
    "d" '(info "(dir)Top")
    "f" 'info-follow-ref
    "h" '(info "(info)Help")
    "g" 'info-goto-node
    "l" 'info-last
    "m" 'info-menu
    "n" 'info-next
    "p" 'info-prev
    "q" 'info-quit
    "x" 'bury-buffer
    "u" 'info-up
    "v" 'info-visit-file
    "?" 'describe-mode
    "HELP" 'describe-mode
    "RET" 'info-goto-link
    "button1-click2" 'info-goto-link
    "button2-click1" '(progn (goto-mouse) (info-goto-link))
    "TAB" 'info-next-link
    "Meta-TAB" 'info-prev-link
    "Shift-TAB" 'info-prev-link)
  "Keymap for Info.")

(defvar info-popup-menus '("Info"
			   ("Goto next node" info-next)
			   ("Goto previous node" info-prev)
			   ("Goto parent node" info-up)
			   ("Goto last visited node" info-last)
			   ()
			   ("Choose menu entry" info-menu)
			   ("Follow link" info-goto-link)
			   ("Find next link" info-next-link)
			   ("Find previous link" info-prev-link)
			   ()
			   ("Open info file..." info-visit-file)
			   ("Quit info" info-quit)
			   ("Bury buffer" bury-buffer)))

(defvar info-tags-buffer nil
  "Buffer for storing the current Info file's tag table.")
(make-variable-buffer-local 'info-tags-buffer)

(defvar info-history '()
  "List of `(FILE NODE POS)' showing how we got to the current node.")
(make-variable-buffer-local 'info-history)

(defvar info-file-name nil
  "The true name (in the filesystem, minus possible suffix) of the root
of the current Info file.")
(make-variable-buffer-local 'info-file-name)

(defvar info-file-suffix nil
  "The suffix of the current Info file.")
(make-variable-buffer-local 'info-file-suffix)

(defvar info-node-name nil
  "The name of the current Info node.")
(make-variable-buffer-local 'info-node-name)

(defvar info-indirect-list nil
  "List of `(START-OFFSET . FILE-NAME)' saying where the current Info file
is split.")
(make-variable-buffer-local 'info-indirect-list)

(defvar info-has-tags-p nil
  "t when we were able to load a tag table for this Info file.")
(make-variable-buffer-local 'info-has-tags-p)

(put 'info-error 'error-message "Info")

;; Read the indirect list (if it exists) and tag table from the file FILENAME.
;; Indirect list ends up in `info-indirect-list', tag table is read into the
;; `info-tags-buffer' buffer. `info-has-tags-p' is set to t if a tags table
;; was loaded.
(defun info-read-tags (filename suffix)
  (let
      ((dir (file-name-directory filename)))
    (clear-buffer)
    (setq info-indirect-list nil
	  info-has-tags-p nil)
    ;; Get the file into the Info buffer
    (read-file-into-buffer (concat filename suffix))
    ;; Scan for tag table or indirect list
    (let
	((p (re-search-forward "^(Tag Table:|Indirect:) *$"
				 (pos 0 0) nil t)))
      (when (and p (looking-at "Indirect:" p nil t))
	;; Parse the indirect list
	(setq p (forward-line 1 p))
	(while (and (/= (get-char p) ?\^_)
		    (looking-at "^(.*): ([0-9]+)$" p nil t))
	  (setq info-indirect-list
		(cons (cons (read (cons (current-buffer) (match-start 2)))
			    (concat dir (copy-area (match-start 1)
						   (match-end 1))))
		      info-indirect-list))
	  (setq p (forward-line 1 p)))
	(setq info-indirect-list (nreverse info-indirect-list)))
      ;; Now look for the tag table
      (when (setq p (re-search-forward "^Tag table: *$" p nil t))
	;; Copy this into the tags buffer
	(let
	    ((end (char-search-forward ?\^_ p)))
	  (unless end
	    (setq end (end-of-buffer)))
	  (clear-buffer info-tags-buffer)
	  (insert (copy-area p end) nil info-tags-buffer)
	  (setq info-has-tags-p t))))))

;; Read the `dir' file, if multiple `dir' files exist concatenate them
(defun info-read-dir ()
  (let
      ((read-dir nil)
       (path info-directory-list))
    (clear-buffer)
    (while path
      (let
	  ((name (expand-file-name "dir" (car path))))
	(when (file-exists-p name)
	  (if read-dir
	      (let
		  ((spos (cursor-pos)))
		(insert-file name)
		;; lose all text from the beginning of the file to the
		;; first menu item, unless this is the first dir file
		(when (re-search-forward "^\\* Menu:" spos nil t)
		  (delete-area spos (forward-line 1 (match-start)))))
	    (insert-file name)
	    (setq read-dir t))
	  (unless (equal (cursor-pos) (start-of-line))
	    (split-line))))
      (setq path (cdr path)))
    (unless read-dir
      (signal 'info-error '("Can't find `dir' file")))
    ;; Don't associate a file name with DIR since it's probably an amalgam
    (set-buffer-file-name nil nil)
    (set-buffer-modified nil nil)
    (setq buffer-file-modtime (cons 0 0)
	  info-file-name "dir"
	  info-node-name "Top"
	  info-has-tags-p nil)
    (goto (or (char-search-forward ?\^_ (start-of-buffer))
		   (start-of-buffer)))
    t))

;; Record the file, node and cursor-position in the `info-history' list
;; for the `info-last' command.
(defun info-remember ()
  (when (and info-file-name info-node-name)
    (setq info-history (cons (list info-file-name
				   info-node-name
				   (cursor-pos))
			     info-history))))

;; Find the actual file for the info-file FILENAME. If an uncompressor
;; is needed to load the file it will be loaded. The returned filename
;; is without the suffix, which is stored in info-file-suffix
(defun info-locate-file (filename)
  (if (and info-file-name (or (not filename) (equal filename "")))
      info-file-name
    (let*
	((lcase-name (translate-string (copy-sequence filename)
				       downcase-table))
	 (path info-directory-list)
	 suffixes files)
      (catch 'foo
	(while path
	  (setq files (list (expand-file-name filename (car path))
			    (expand-file-name (concat filename ".info")
					      (car path))
			    (expand-file-name lcase-name (car path))
			    (expand-file-name (concat lcase-name ".info")
					      (car path))))
	  (while files
	    (setq suffixes info-suffixes)
	    (while suffixes
	      (when (file-exists-p (concat (car files) (car (car suffixes))))
		(setq info-file-suffix (car (car suffixes)))
		(when (cdr (car suffixes))
		  ;; Activate the uncompressor if necessary
		  (eval (cdr (car suffixes))))
		(throw 'foo (car files)))
	      (setq suffixes (cdr suffixes)))
	    (setq files (cdr files)))
	  (setq path (cdr path)))
	(signal 'info-error (list "Can't find Info document" filename))))))

;; Display the node NODENAME. NODENAME can contain a file name. If no node
;; is specified go to `Top' node.
;; This depends on some magic for locating the node text. It only works 100%
;; with `makeinfo' generated files.
(defun info-find-node (nodename)
  (let
      ((inhibit-read-only t)
       filename file-location)
    (unrestrict-buffer)
    (if (string-match "^\\((.*)\\)(.*)$" nodename)
	(setq filename (expand-last-match "\\1")
	      nodename (expand-last-match "\\2"))
      (unless (setq filename info-file-name)
	(error "File containing node `%s' isn't specified" nodename)))
    (when (string= nodename "")
      (setq nodename "Top"))
    (if (string-match "^dir$" filename nil t)
	(info-read-dir)
      (setq file-location (info-locate-file filename))
      (when (not (string= info-file-name filename))
	(info-read-tags file-location info-file-suffix)
	(setq info-file-name filename))
      (if (not info-has-tags-p)
	  (progn
	    ;; No tag list
	    (unless (string= info-file-name filename)
	      (read-file-into-buffer (concat file-location info-file-suffix)))
	    (when (re-search-forward (concat "^File:.* Node: *"
					    (quote-regexp nodename))
				    (start-of-buffer))
	      (goto (start-of-line (match-start)))))
	(if (re-search-forward (concat "^Node: "
				      (quote-regexp nodename)
				      ?\^?)
			      (pos 0 0) info-tags-buffer t)
	    (let
		((lst info-indirect-list)
		 (offset (read (cons info-tags-buffer (match-end))))
		 subfile)
	      (if (null lst)
		  ;; No indirect list
		  (setq offset (+ offset 2)
			subfile file-location)
		;; Indirect list, chase down the list for the
		;; correct file to use
		(catch 'info
		  (while (cdr lst)
		    (when (< offset (car (car (cdr lst))))
		      (setq subfile (car lst))
		      (throw 'info))
		    (setq lst (cdr lst))
		    (setq subfile (car lst))))
		;; Use some magic to calculate the physical position of the
		;; node. This seems to work?
		(if (eq subfile (car info-indirect-list))
		    (setq offset (+ offset 2))
		  (setq offset (+ (- offset (car subfile))
				  (car (car info-indirect-list)) 2)))
		(setq subfile (cdr subfile)))
	      (unless (string= (buffer-file-name)
			       (concat subfile info-file-suffix))
		(read-file-into-buffer (concat subfile info-file-suffix)))
	      (goto (offset-to-pos offset)))
	  (signal 'info-error (list "Can't find node" nodename)))))
    ;; Now cursor should be at beginning of node text. Make sure
    (let
	((p (char-search-backward ?\^_)))
      (when (and p (looking-at (concat "^File:.*Node: "
					 (quote-regexp nodename))
				 (forward-line 1 p)))
	(goto (match-start)))
      (setq p (or (char-search-forward ?\^_ (forward-char))
		    (end-of-buffer nil t)))
      (restrict-buffer (cursor-pos) p)
      (info-highlight-buffer))
    (setq info-node-name nodename)
    (buffer-status-id (concat "Info: " ?( info-file-name ?) info-node-name))
    t))

;; Make some extents
(defun info-highlight-buffer ()
  (delete-all-extents)
  (let
      ((tem (start-of-buffer)))
    (while (re-search-forward "\\*[\t\n ]*note[\t\n ]+([^:]+)" tem nil t)
      (setq tem (match-end 1))
      (make-extent (match-start 1) tem (list 'face info-xref-face
					     'mouse-face active-face)))
    (when (re-search-forward "^\\* menu:" (start-of-buffer) nil t)
      (setq tem (match-end))
      (while (re-search-forward "^\\*[\t ]+([^:\n]+)" tem)
	(setq tem (match-end 1))
	(make-extent (match-start 1) tem (list 'face info-menu-face
					       'mouse-face active-face))))))

;; Return a list of all node names matching START in the current tag table
(defun info-list-nodes (start)
  (let
      ((regexp (concat "^Node: (" (quote-regexp start) ".*)\^?"))
       (lst ()))
    (with-buffer info-tags-buffer
      (goto (start-of-buffer))
      (while (re-search-forward regexp nil nil t)
	(setq lst (cons (expand-last-match "\\1") lst))
	(goto (match-end))))
    lst))

;; "prompt" variant. LIST-FUN is a function to call the first time a list
;; of possible completions is required.
(defun info-prompt (info-list-fun &optional title default start)
  (unless title
    (setq title "Select node"))
  (when default
    (setq title (concat title " (default: " default ")")))
  (unless start
    (setq start ""))
  (let*
      ((prompt-completion-function (make-closure
				    `(lambda (w)
				       (or prompt-list
					   (with-buffer ,(current-buffer)
					     (setq prompt-list
						   (funcall info-list-fun))))
				       (prompt-complete-from-list w))))
       (prompt-validate-function (make-closure
				  `(lambda (w)
				     (or prompt-list
					 (with-buffer ,(current-buffer)
					   (setq prompt-list
						 (funcall info-list-fun))))
				     (prompt-validate-from-list w))))
       (prompt-list-fold-case t)
       (completion-fold-case t)
       ;;(prompt-word-regexps prompt-def-regexps)
       (prompt-list '())
       (res (prompt title start)))
    (if (equal res "")
	default
      res)))

;;;###autoload
(defun info (&optional start-node new-buffer)
  "Start the Info viewer. If START-NODE is given it specifies the node to
show, otherwise the current node is used (or `(dir)' if this is the first
time that `info' has been called)."
  (interactive "\nP")
  (goto-buffer (open-buffer "*Info*" new-buffer))
  (setq local-keymap 'info-keymap
	major-mode 'info-mode
	mode-name "Info"
	popup-local-menus info-popup-menus
	auto-save-p nil)
  (buffer-record-undo nil)
  (set-buffer-read-only nil t)
  (unless info-tags-buffer
    (setq info-tags-buffer (make-buffer "*Info-tags*"))
    (with-buffer info-tags-buffer
      (buffer-record-undo nil)))
  (cond
   (start-node
    (info-remember)
    (info-find-node start-node))
   ((and (buffer-file-name) info-node-name)
    (when (time-later-p (file-modtime (buffer-file-name)) buffer-file-modtime)
      (info-find-node (concat info-node-name))))
   (t
    (info-find-node "(dir)"))))

(defun info-quit ()
  "Exit the info browzer."
  (interactive)
  (kill-buffer (current-buffer)))

;; The *Info* buffer has this function as its major-mode so that `Ctrl-h m'
;; displays some meaningful text
(defun info-mode ()
  "Info mode:\n
This mode is used to browse through the Info tree of documentation. The
local bindings are:\n
\\{info-keymap}")

;; Prompt for the name of a node and find it.
(defun info-goto-node (node)
  (interactive "sGoto node: ")
  (when node
    (info-remember)
    (info-find-node node)))

;; Prompt for the name of a file, then go to it's Top node.
(defun info-visit-file (file)
  (interactive "fInfo file to display:")
  (info-remember)
  (info-find-node (concat ?\( file ?\) "Top")))

;; Returns the node name of the menu item on the current line
(defun info-parse-menu-line ()
  (when (or (looking-at "^\\* (.*[^ ]+)[ ]*::" (start-of-line))
	    (looking-at "^\\* .+:[\t ]*((\\([^ ]+\\)|)([^,.]+|))\\."
			(start-of-line)))
    (expand-last-match "\\1")))

;; Return a list of the names of all menu items
(defun info-list-menu-items ()
  (let
      ((lst ())
       (opos (restriction-start))
       name)
    (while (re-search-forward "^\\* ([a-zA-Z0-9]+[^:.]*)" opos)
      (setq name (expand-last-match "\\1"))
      (setq opos (match-end))
      (unless (string-match "^menu$" name nil t)
	(setq lst (cons name lst))))
    lst))

;; Position the cursor at the start of the menu.
(defun info-goto-menu-start ()
  (when (or (re-search-backward "^\\* Menu:" nil nil t)
	    (re-search-forward "^\\* Menu:" nil nil t))
    (goto (forward-line 1 (match-start)))))

;; Goto the ITEM-INDEX'th menu item.
(defun info-menu-nth (item-index)
  (interactive (list (- (aref (current-event-string) 0) ?0)))
  (unless (info-goto-menu-start)
    (signal 'info-error (list "Can't find menu")))
  (while (and (> item-index 0) (re-search-forward "^\\* .*:"))
    (goto (match-end))
    (setq item-index (1- item-index)))
  (when (/= item-index 0)
    (signal 'info-error (list "Can't find menu node")))
  (goto (start-of-line))
  (let
      ((nodename (info-parse-menu-line)))
    (if nodename
	(progn
	  (info-remember)
	  (info-find-node nodename))
      (signal 'info-error (list "Menu line malformed")))))

;; Prompt for the name of a menu item (with a default) and find it's node.
(defun info-menu (&optional menu-name)
  (interactive)
  (when (info-goto-menu-start)
    (unless menu-name
      (setq menu-name (and (looking-at "^\\* ([^:.]+)" (start-of-line))
			   (expand-last-match "\\1")))
      (save-excursion
	(setq menu-name (info-prompt info-list-menu-items
				     "Menu item:" menu-name)))))
  (when menu-name
    (if (re-search-forward (concat "^\\* " (quote-regexp menu-name) ?:)
			   nil nil t)
	(progn
	  (goto (match-start))
	  (let
	      ((node-name (info-parse-menu-line)))
	    (if node-name
		(progn
		  (info-remember)
		  (info-find-node node-name))
	      (signal 'info-error (list "Menu line malformed")))))
      (signal 'info-error (list "Can't find menu" menu-name)))))

;; Retrace our steps one node.
(defun info-last ()
  (interactive)
  (if info-history
      (progn
	(let
	    ((hist (car info-history)))
	  (setq info-history (cdr info-history))
	  (when (info-find-node (concat ?( (car hist) ?) (nth 1 hist)))
	    (goto (nth 2 hist))
	    t)))
    (message "No more history")
    (beep)))

(defun info-next ()
  (interactive)
  (info-find-link "Next"))

(defun info-prev ()
  (interactive)
  (info-find-link "Prev"))

(defun info-up ()
  (interactive)
  (info-find-link "Up"))

(defun info-find-link (link-type)
  (let*
      ((regexp (concat ".*" link-type ": ([^,\n]*)(,|[\t ]*$)"))
       new-node)
    (when (looking-at regexp (start-of-buffer) nil t)
      (setq new-node (expand-last-match "\\1")))
    (if new-node
	(progn
	  (info-remember)
	  (info-find-node new-node))
      (message (concat "No " link-type " node"))
      (beep))))

;; Check this line for a menuitem of an xref, if one exists find its node
(defun info-goto-link ()
  (interactive)
  (let
      (node)
    (unless (setq node (info-parse-menu-line))
      (if (re-search-backward "\\*Note" nil nil t)
	  (progn
	    (goto (match-start))
	    (setq node (cdr (info-parse-ref))))
	(signal 'info-error '("Nothing on this line to go to"))))
    (info-remember)
    (info-find-node node)))

;; Move the cursor to the next menuitem or xref
(defun info-next-link ()
  (interactive)
  (let
      ((p (re-search-forward "(^\\* |\\*Note)" (forward-char) nil t)))
    (while (and p (looking-at "\\* Menu:" p nil t))
      (setq p (re-search-forward "(^\\* |\\*Note)"
				  (forward-char 1 p) nil t)))
    (when p
      (goto p))))

;; Move the cursor to the previous menuitem or xref
(defun info-prev-link ()
  (interactive)
  (let
      ((p (re-search-backward "(^\\* |\\*Note)" (forward-char -1) nil t)))
    (while (and p (looking-at "\\* Menu:" p nil t))
      (setq p (re-search-backward "(^\\* |\\*Note)"
				  (forward-char -1 p) nil t)))
    (when p
      (goto p))))

;; Parse the cross-reference under the cursor into a cons-cell containing
;; its title and node. This is fairly hairy since it has to cope with refs
;; crossing line boundarys.
(defun info-parse-ref ()
  (when (looking-at "\\*Note *" nil nil t)
    (let
	((p (match-end))
	 end ref-title ref-node)
      (if (setq end (re-search-forward "[\t ]*:"))
	  (progn
	    (while (> (pos-line end) (pos-line p))
	      (let
		  ((bit (copy-area p (re-search-forward "[\t ]*$" p))))
		(unless (equal bit "")
		  (setq ref-title (cons ?\  (cons bit ref-title)))))
	      (setq p (re-search-forward "[^\n\t ]" (match-end)))
	      (unless p
		(signal 'info-error '("Malformed reference"))))
	    (setq ref-title (apply concat (nreverse (cons (copy-area p end)
							  ref-title)))
		  p (forward-char 1 end))
	    (if (= (get-char p) ?:)
		(setq ref-node ref-title)
	      (when (looking-at " +" p)
		(setq p (match-end)))
	      (if (setq end (re-search-forward "[\t ]*[:,.]" p))
		  (progn
		    (while (> (pos-line end) (pos-line p))
		      (let
			  ((bit (copy-area p (re-search-forward "[\t ]*$"
								 p))))
			(unless (equal bit "")
			  (setq ref-node (cons ?\  (cons bit ref-node))))
			(setq p (re-search-forward "[^\n\t ]" (match-end))))
		      (unless p
			(signal 'info-error '("Malformed reference"))))
		    (setq ref-node (apply concat (nreverse (cons (copy-area
								  p end)
								 ref-node)))))
		(signal 'info-error '("Malformed reference")))))
	(signal 'info-error '("Malformed reference")))
      (when (and ref-title ref-node)
	(cons ref-title ref-node)))))

;; This should give you a prompt with all xrefs in the node to complete from,
;; currently it just finds the node of the next xref
(defun info-follow-ref ()
  (interactive)
  (unless (looking-at "\\*Note" nil nil t)
    (goto (re-search-forward "\\*Note" nil nil t)))
  (let
      ((ref (info-parse-ref)))
    (when ref
      (info-remember)
      (info-find-node (cdr ref)))))

;;;###autoload
(defun info-visit-node (info-file node-name &optional menu-key)
  (info (concat ?\( info-file ?\) node-name))
  (when menu-key
    (info-menu menu-key)))

(defun info-index (info-files index-node key)
  (when (stringp info-files)
    (setq info-files (list info-files)))
  (catch 'out
    (mapc (lambda (f)
	    (condition-case nil
		(progn
		  (info-visit-node f index-node key)
		  (throw 'out t))
	      (info-error))) info-files)
    (signal 'info-error (list "Can't find index entry" key))))

;;;###autoload
(defun info-describe-function (function)
  (interactive
   (list (prompt-for-string "Describe function:" (symbol-at-point))))
  (when function
    (info-index info-documentation-files info-function-index-node function)
    (when (re-search-forward (format nil "^ - .* %s" (quote-regexp function)))
      (goto (match-start)))))

;;;###autoload
(defun info-describe-variable (variable)
  (interactive
   (list (prompt-for-string "Describe variable:" (symbol-at-point))))
  (when function
    (info-index info-documentation-files info-variable-index-node variable)
    (when (re-search-forward (format nil "^ - .* %s" (quote-regexp variable)))
      (goto (match-start)))))
