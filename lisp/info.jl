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

(defvar info-directory-list
  (if (amiga-p) '("INFO:") '("/usr/info" "/usr/local/info/" "~/info"))
  "List of directories to search for info files if they can't be found as-is.")

(defvar info-suffixes '(("" . nil)
			(".gz" . gzip)
			(".Z" . gzip))
  "List of (SUFFIX . PACKAGE) combinations. When searching for an info
file try each SUFFIX in turn. When one matches require PACKAGE to be
loaded so that the file can be decoded (through the `read-file-hook').")

(defvar info-keymap (make-keytab)
  "Keymap for Info.")

(defvar info-buffer (open-buffer "*Info*" t)
  "Buffer in which Info nodes are displayed.")
(set-buffer-special info-buffer t)

(defvar info-tags-buffer (make-buffer "*Info tags*")
  "Buffer for storing the current Info file's tag table.")
(set-buffer-special info-tags-buffer t)

(defvar info-history '()
  "List of `(FILE NODE POS)' showing how we got to the current node.")

(defvar info-file-name nil
  "The true name (in the filesystem, minus possible suffix) of the root
of the current Info file.")

(defvar info-file-suffix nil
  "The suffix of the current Info file.")

(defvar info-node-name nil
  "The name of the current Info node.")

(defvar info-indirect-list nil
  "List of `(START-OFFSET . FILE-NAME)' saying where the current Info file
is split.")

(defvar info-has-tags-p nil
  "t when we were able to load a tag table for this Info file.")

(defvar info-initialised nil
  "Protection against being loaded multiple times.")

(unless info-initialised
  (setq info-initialised t)
  (put 'info-error 'error-message "Info")
  (bind-keys info-keymap
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
    "b" 'goto-start-of-buffer
    "d" '(info "(dir)Top")
    "f" 'info-follow-ref
    "h" '(info "(info)Help")
    "g" 'info-goto-node
    "l" 'info-last
    "m" 'info-menu
    "n" 'info-next
    "p" 'info-prev
    "q" 'bury-buffer
    "u" 'info-up
    "v" 'info-visit-file
    "?" 'describe-mode
    "HELP" 'describe-mode
    "RET" 'info-goto-link
    "LMB-CLICK2" 'info-goto-link
    "TAB" 'info-next-link
    "Meta-TAB" 'info-prev-link
    "Shift-TAB" 'info-prev-link)
  (with-buffer info-buffer
    (setq keymap-path (cons 'info-keymap keymap-path)
	  major-mode 'info-mode
	  buffer-record-undo nil)
    (set-buffer-read-only info-buffer t)
    (setq auto-save-p nil))
  (with-buffer info-tags-buffer
    (setq buffer-record-undo nil)))

;; Read the indirect list (if it exists) and tag table from the file FILENAME.
;; Indirect list ends up in `info-indirect-list', tag table is read into the
;; `info-tags-buffer' buffer. `info-has-tags-p' is set to t if a tags table
;; was loaded.
(defun info-read-tags (filename suffix)
  (let
      ((dir (file-name-directory filename)))
    (with-buffer info-buffer
      (clear-buffer)
      (setq info-indirect-list nil
	    info-has-tags-p nil)
      ;; Get the file into the Info buffer
      (read-file-into-buffer (concat filename suffix))
      ;; Scan for tag table or indirect list
      (let
	  ((pos (re-search-forward "^(Tag Table:|Indirect:) *$"
				  (pos 0 0) info-buffer t)))
	(when (and pos (looking-at "Indirect:" pos nil t))
	  ;; Parse the indirect list
	  (setq pos (forward-line 1 pos))
	  (while (and (/= (get-char pos) ?\^_)
		      (looking-at "^(.*): ([0-9]+)$" pos nil t))
	    (setq info-indirect-list
		  (cons
		   (cons
		    (read (cons info-buffer (match-start 2)))
		    (concat dir (copy-area (match-start 1) (match-end 1))))
		   info-indirect-list))
	    (setq pos (forward-line 1 pos)))
	  (setq info-indirect-list (nreverse info-indirect-list)))
	;; Now look for the tag table
	(when (setq pos (re-search-forward "^Tag table: *$" pos nil t))
	  ;; Copy this into the tags buffer
	  (let
	      ((end (char-search-forward ?\^_ pos)))
	    (unless end
	      (setq end (end-of-buffer)))
	    (clear-buffer info-tags-buffer)
	    (insert (copy-area pos end) nil info-tags-buffer)
	    (setq info-has-tags-p t)))))))

;; Read the `dir' file, if multiple `dir' files exist concatenate them
(defun info-read-dir ()
  (let
      ((read-dir nil)
       (path info-directory-list))
    (clear-buffer)
    (while path
      (let
	  ((name (file-name-concat (expand-file-name (car path)) "dir")))
	(when (file-exists-p name)
	  (if read-dir
	      (let
		  ((spos (cursor-pos)))
		(insert-file name)
		;; lose all text from the beginning of the file to the
		;; first menu item, unless this is the first dir file
		(when (re-search-forward "^\\* Menu:" spos nil t)
		  (delete-area spos (forward-line 1 (match-start)))))
	    (read-file-into-buffer name)
	    (goto (end-of-buffer))
	    (setq read-dir t))
	  (unless (equal (cursor-pos) (start-of-line))
	    (split-line))))
      (setq path (cdr path)))
    (unless read-dir
      (signal 'info-error '("Can't find `dir' file")))
    (setq buffer-file-modtime (cons 0 0)
	  info-file-name "dir"
	  info-node-name "Top"
	  info-has-tags-p nil
	  mode-name "(dir)")
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
	 (path (cons "" info-directory-list))
	 suffixes files)
      (catch 'foo
	(while path
	  (setq files (list (file-name-concat (car path) filename)
			    (file-name-concat (car path) (concat filename
								 ".info"))
			    (file-name-concat (car path) lcase-name)
			    (file-name-concat (car path) (concat lcase-name
								 ".info"))))
	  (while files
	    (setq suffixes info-suffixes)
	    (while suffixes
	      (when (file-exists-p (concat (car files) (car (car suffixes))))
		(setq info-file-suffix (car (car suffixes)))
		(when (cdr (car suffixes))
		  ;; Load the uncompressor if necessary
		  (require (cdr (car suffixes))))
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
      ((filename (and (string-match "^\\((.*)\\).*$" nodename)
		      (expand-last-match "\\1")))
       (inhibit-read-only t)
       offset)
    (unrestrict-buffer)
    (when filename
      (unless (setq nodename (and (string-match "^\\(.*\\)(.+)$" nodename)
				  (expand-last-match "\\1")))
	(setq nodename "Top")))
    (if (and filename (member filename '("dir" "DIR" "Dir")))
	(info-read-dir)
      (setq filename (info-locate-file filename))
      (when (or (not (equal info-file-name filename))
		(time-later-p (file-modtime filename) buffer-file-modtime))
	(info-read-tags filename info-file-suffix)
	(setq info-file-name filename))
      (if (not info-has-tags-p)
	  (progn
	    ;; No tag list
	    (unless (string= info-file-name filename)
	      (read-file-into-buffer (concat filename info-file-suffix)))
	    (when (re-search-forward (concat "^File:.* Node: *"
					    (quote-regexp nodename))
				    (start-of-buffer))
	      (goto (start-of-line (match-start)))))
	(if (re-search-forward (concat "^Node: "
				      (quote-regexp nodename)
				      ?\^?)
			      (pos 0 0) info-tags-buffer t)
	    (let
		((list info-indirect-list)
		 (offset (read (cons info-tags-buffer (match-end))))
		 subfile)
	      (if (null list)
		  ;; No indirect list
		  (setq offset (+ offset 2)
			subfile info-file-name)
		;; Indirect list, chase down the list for the
		;; correct file to use
		(catch 'info
		  (while (cdr list)
		    (when (< offset (car (car (cdr list))))
		      (setq subfile (car list))
		      (throw 'info))
		    (setq list (cdr list))
		    (setq subfile (car list))))
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
	((pos (char-search-backward ?\^_)))
      (when (and pos (looking-at (concat "^File:.*Node: "
					 (quote-regexp nodename))
				 (forward-line 1 pos)))
	(goto (match-start)))
      (setq pos (or (char-search-forward ?\^_ (forward-char))
		    (end-of-buffer nil t)))
      (restrict-buffer (cursor-pos) pos))
    (setq info-node-name nodename
	  mode-name (concat ?( (file-name-nondirectory info-file-name)
			    ?) info-node-name))
    t))

;; Return a list of all node names matching START in the current tag table
(defun info-list-nodes (start)
  (let
      ((regexp (concat "^Node: (" (quote-regexp start) ".*)\^?"))
       (list ()))
    (with-buffer info-tags-buffer
      (goto (start-of-buffer))
      (while (re-search-forward regexp nil nil t)
	(setq list (cons (expand-last-match "\\1") list))
	(goto (match-end))))
    list))

;; "prompt" variant. LIST-FUN is a function to call the first time a list
;; of possible completions is required.
(defun info-prompt (list-fun &optional title default start)
  (unless title
    (setq title "Select node"))
  (when default
    (setq title (concat title " (default: " default ")")))
  (unless start
    (setq start ""))
  (let*
      ((prompt-completion-function
	#'(lambda (w)
	    (unless prompt-list
	      (with-buffer info-buffer
		(setq prompt-list (funcall list-fun))))
	    (prompt-complete-from-list w)))
       (prompt-validate-function
	#'(lambda (w)
	    (unless prompt-list
	      (with-buffer info-buffer
		(setq prompt-list (funcall list-fun))))
	    (prompt-validate-from-list w)))
       (prompt-list-fold-case t)
       ;;(prompt-word-regexps prompt-def-regexps)
       (prompt-list '())
       (res (prompt title start)))
    (if (equal res "")
	default
      res)))

;;;###autoload
(defun info (&optional start-node)
  "Start the Info viewer. If START-NODE is given it specifies the node to
show, otherwise the current node is used (or `(dir)' if this is the first
time that `info' has been called)."
  (interactive)
  (goto-buffer info-buffer)
  (cond
   (start-node
    (info-remember)
    (info-find-node start-node))
   ((and (buffer-file-name) info-node-name)
    (when (time-later-p (file-modtime (buffer-file-name)) buffer-file-modtime)
      (info-find-node info-node-name)))
   (t
    (info-find-node "(dir)"))))

;; The *Info* buffer has this function as its major-mode so that `Ctrl-h m'
;; displays some meaningful text
(defun info-mode ()
  "Info mode:\n
This mode is used to browse through the Info tree of documentation, special
commands are,\n
  `SPC'		Next screen of text
  `BS'		Previous screen
  `b'		Move to the start of this node
  `1' to `9'	Go to the Nth menu item in this node
  `d'		Find the `(dir)' node -- the root of Info
  `f'		Find the node of the next cross-reference in this node
  `g NODE RET'	Go to the node called NODE
  `h'		Display the Info tutorial, the node `(info)Help'
  `l'		Backtrack one node
  `m'		Choose a menu item from this node
  `n'		Find the `next' node
  `p'		Go to the `previous' node
  `u'		Display the parent node of this one
  `v'		Prompts for the name of a file, then loads that file into
		Info
  `q'		Quit Info
  `?', `HELP'	Display this command summary
  `RET',
  `LMB-CLICK2'	Go to the link (menu item or xref) on this line
  `TAB'		Put the cursor on the next link in this node
  `Meta-TAB'	Move to the previous link in this node")

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

;; Return a list of the names of all menu items. Starts searching from
;; the cursor position.
(defun info-list-menu-items ()
  (let
      ((list ())
       (opos (cursor-pos)))
    (while (re-search-forward "^\\* ([a-zA-Z0-9]+[^:.]*)" opos)
      (setq list (cons (expand-last-match "\\1") list))
      (setq opos (match-end)))
    list))

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
(defun info-menu ()
  (interactive)
  (let
      ((menu-name (and (looking-at "^\\* ([^:.]+)" (start-of-line))
		       (expand-last-match "\\1"))))
    (when (info-goto-menu-start)
      (let
	  ((opos (cursor-pos)))
	(setq menu-name (info-prompt 'info-list-menu-items
				     "Menu item:" menu-name))
	(goto opos)))
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
	(signal 'info-error (list "Can't find menu" menu-name))))))

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
    (unless (setq node (cdr (info-parse-ref)))
      (goto (start-of-line))
      (unless (setq node (info-parse-menu-line))
	(signal 'info-error '("Nothing on this line to go to"))))
    (info-remember)
    (info-find-node node)))

;; Move the cursor to the next menuitem or xref
(defun info-next-link ()
  (interactive)
  (let
      ((pos (re-search-forward "(^\\* |\\*Note)" (forward-char) nil t)))
    (while (and pos (looking-at "\\* Menu:" pos nil t))
      (setq pos (re-search-forward "(^\\* |\\*Note)"
				  (forward-char 1 pos) nil t)))
    (when pos
      (goto pos))))

;; Move the cursor to the previous menuitem or xref
(defun info-prev-link ()
  (interactive)
  (let
      ((pos (re-search-backward "(^\\* |\\*Note)" (forward-char -1) nil t)))
    (while (and pos (looking-at "\\* Menu:" pos nil t))
      (setq pos (re-search-backward "(^\\* |\\*Note)"
				  (forward-char -1 pos) nil t)))
    (when pos
      (goto pos))))

;; Parse the cross-reference under the cursor into a cons-cell containing
;; its title and node. This is fairly hairy since it has to cope with refs
;; crossing line boundarys.
(defun info-parse-ref ()
  (when (looking-at "\\*Note *" nil nil t)
    (let
	((pos (match-end))
	 end ref-title ref-node)
      (if (setq end (re-search-forward "[\t ]*:"))
	  (progn
	    (while (> (pos-line end) (pos-line pos))
	      (let
		  ((bit (copy-area pos (re-search-forward "[\t ]*$" pos))))
		(unless (equal bit "")
		  (setq ref-title (cons ?\  (cons bit ref-title)))))
	      (setq pos (re-search-forward "[^\n\t ]" (match-end)))
	      (unless pos
		(signal 'info-error '("Malformed reference"))))
	    (setq ref-title (apply 'concat (nreverse (cons (copy-area pos end)
							   ref-title)))
		  pos (forward-char 1 end))
	    (if (= (get-char pos) ?:)
		(setq ref-node ref-title)
	      (when (looking-at " +" pos)
		(setq pos (match-end)))
	      (if (setq end (re-search-forward "[\t ]*[:,.]" pos))
		  (progn
		    (while (> (pos-line end) (pos-line pos))
		      (let
			  ((bit (copy-area pos (re-search-forward "[\t ]*$"
								 pos))))
			(unless (equal bit "")
			  (setq ref-node (cons ?\  (cons bit ref-node))))
			(setq pos (re-search-forward "[^\n\t ]" (match-end))))
		      (unless pos
			(signal 'info-error '("Malformed reference"))))
		    (setq ref-node (apply 'concat (nreverse (cons (copy-area
								   pos end)
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
