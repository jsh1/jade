;;;; summary.jl -- Generic menu environment
;;;  Copyright (C) 1997 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'summary)

(defvar summary-keymap (make-keylist)
  "Base keymap for modes deriving from summary.")
(bind-keys summary-keymap
  "Ctrl-n" 'summary-next-item
  "Ctrl-f" 'summary-next-item
  "Down" 'summary-next-item
  "TAB" 'summary-next-item
  "Ctrl-p" 'summary-previous-item
  "Ctrl-b" 'summary-previous-item
  "Up" 'summary-previous-item
  "Meta-TAB" 'summary-previous-item
  "RET" 'summary-select-item
  "Ctrl-l" 'summary-update
  "x" 'summary-execute
  "d" 'summary-mark-delete
  "Ctrl-d" 'summary-mark-delete
  "DEL" 'summary-mark-delete
  "q" 'summary-quit
  "u" 'summary-unmark-item
  "LMB-Click2" 'summary-select-item)


;; Local variables

;; TAGS include:
;;
;;   select ITEM
;;	Called to select ITEM in an appropriate manner
;;
;;   delete ITEM
;;	Delete ITEM (called from the execute phase)
;;
;;   print ITEM
;;	Emit a one line summary of ITEM in the current buffer at the
;;	cursor position
;;
;;   list
;;	Return a list of all current items
;;
;;   current
;;	Return the *index* in the list of all current items, of the
;;	currently selected item (indexed from zero).
;;
;;   execute-begin
;;	Called before executing pending actions
;;
;;   execute-end
;;	Called after executing pending actions
;;
;;   after-marking ITEM
;;	Called after marking ITEM in some way (i.e. after adding
;;	a pending operation)
;;
;;   after-move INDEX
;;	Called after moving the cursor to the item at position INDEX
;;	in the most recently given list of items
;;
;;   after-update
;;	Called after updating the buffer display
;;
;;   on-quit
;;	Called before quitting the summary
;;
;; Only the select, print and list actions are required to be defined. All
;; others are optional.
(defvar summary-functions nil
  "Association list of functions to call to achieve a certain operation,
(TAG . FUNC). See summary.jl for details.")
(make-variable-buffer-local 'summary-functions)

(defvar summary-items nil
  "List of ITEMS relating the line-numbers of the menu to the item the
line represents.")
(make-variable-buffer-local 'summary-items)

(defvar summary-pending-ops nil
  "Association list of operations to execute at a later date.
(ITEM OPERATIONS...)")
(make-variable-buffer-local 'summary-pending-ops)

(defvar summary-first-line nil
  "Position of first entry in the menu.")
(make-variable-buffer-local 'summary-first-line)

(defvar summary-actual-keymap nil)
(make-variable-buffer-local 'summary-actual-keymap)


;; Entry point

;; Call this to initialise a summary menu in the current buffer. NAME
;; is put in mode-name, FUNCTIONS in summary-functions. If KEYMAP is
;; non-nil it is used, otherwise summary-keymap is.
;;  The current end of the buffer is taken as the point from which
;; the menu should be displayed.
(defun summary-mode (name functions &optional keymap)
  "Summary Mode:\n
This major mode provides a generic menu capability. It allows lists of
items to be displayed and manipulated."
  (when major-mode-kill
    (funcall major-mode-kill))
  (setq summary-functions functions
	summary-pending-ops nil
	summary-first-line (start-of-line (end-of-buffer))
	summary-actual-keymap (or keymap summary-keymap)
	major-mode 'summary-mode
	major-mode-kill 'summary-mode-kill
	mode-name name
	keymap-path (cons summary-actual-keymap keymap-path)
	buffer-record-undo nil
	buffer-undo-list nil)
  (set-buffer-read-only (current-buffer) t)
  (set-buffer-special (current-buffer) t)
  (add-hook 'unbound-key-hook 'nop)
  (call-hook 'summary-mode-hook)
  (summary-update))

(defun summary-mode-kill ()
  (setq major-mode nil
	major-mode-kill nil
	mode-name nil
	keymap-path (delq summary-actual-keymap keymap-path))
  (remove-hook 'unbound-key-hook 'nop))



;; Call the function FUNC with ARGS
(defmacro summary-dispatch (func &rest args)
  (cons 'funcall
	(cons (list 'cdr (list 'assq func 'summary-functions))
	      args)))

;; Returns t when function FUNC exists for the current summary
(defmacro summary-function-exists-p (func)
  (list 'assq func 'summary-functions))

;; Call the function FUNC with ARGS, only if it exists.
(defmacro summary-maybe-dispatch (func &rest args)
  (list 'and
	(list 'summary-function-exists-p func)
	(cons 'summary-dispatch (cons func args))))

(defmacro summary-get-item (index)
  "Return the item at position INDEX in the menu (from zero)."
  (list 'nth index 'summary-items))

(defun summary-get-index (item)
  "Return the index in the menu at which ITEM is displayed."
  (- (length summary-items) (length (memq item summary-items))))

;;The following works as well. I'm not sure which will be fastest; I think
;;the memq version -- there are byte codes for length and memq. But it
;;does take 2N steps as opposed to M, (M = position of ITEM,
;;N = length of list)...
;;(let
;;    ((items summary-items)
;;     (index 0))
;;  (while (and items (not (equal (car items) item)))
;;    (setq index (1+ index)
;;	    items (cdr items)))
;;  index))

(defun summary-current-index ()
  "Return the index of the item under the cursor."
  (let
      ((index (- (pos-line (cursor-pos)) (pos-line summary-first-line))))
    (if (< index 0)
	(error "No item on this line")
      index)))

(defmacro summary-current-item ()
  "Return the item under the cursor."
  '(summary-get-item (summary-current-index)))

(defun summary-set-current-item (item)
  "Set ITEM to be the current one."
  (summary-goto-item (summary-get-index item)))

(defun summary-highlight-index (index)
  "Highlight the item at position INDEX."
  (let*
      ((start (pos 0 (+ (pos-line summary-first-line) index)))
       (end (pos (car (view-dimensions)) (pos-line start))))
    (mark-block start end)))

(defmacro summary-get-pending-ops (item)
  "Return the list of operations pending on ITEM."
  (list 'assq item 'summary-pending-ops))

(defun summary-add-pending-op (item op)
  "Add OP to the list of operations to call on ITEM at a later date."
  (let
      ((existing (summary-get-pending-ops item)))
    (if existing
	(unless (memq op (cdr existing))
	  (setcdr existing (cons op (cdr existing)))
	  ;; Make sure that if there's a delete op in the
	  ;; list, that it's at the end, so that the item isn't
	  ;; possibly deleted before all the ops have had their way
	  (when (memq 'delete (cdr existing))
	    (setcdr existing (nconc (delq 'delete (cdr existing))
				    (list 'delete))))
	  (summary-update-item item))
      (setq summary-pending-ops (cons (cons item (cons op nil))
				      summary-pending-ops))
      (summary-update-item item))
    (summary-maybe-dispatch 'after-marking item)))

(defun summary-unmark-item (item)
  "Discard all operations pending on ITEM."
  (interactive (list (summary-current-item) t))
  (let
      ((ops (summary-get-pending-ops item)))
    (when ops
      (setq summary-pending-ops (delq ops summary-pending-ops))
      (summary-update-item item))
    (summary-maybe-dispatch 'after-marking item)))

(defun summary-update ()
  "Redraw the menu, after rebuilding the list of items. Loses the current
highlight."
  (interactive)
  (let
      ((inhibit-read-only t))
    (block-kill)
    (delete-area summary-first-line (end-of-buffer))
    (setq summary-items (summary-dispatch 'list))
    (goto summary-first-line)
    (let
	((items summary-items))
      (while items
	(summary-dispatch 'print (car items))
	(setq items (cdr items))
	(when items
	  (insert "\n")))
      (summary-goto-item (or (summary-maybe-dispatch 'current) 0))
      (summary-maybe-dispatch 'after-update))))

(defun summary-update-item (item)
  "Redraw the menu entry for ITEM."
  (let*
      ((inhibit-read-only t)
       (index (summary-get-index item))
       (old-cursor (cursor-pos)))
    (goto (pos 0 (+ (pos-line summary-first-line) index)))
    (if (= (pos-line (cursor-pos)) (pos-line (end-of-buffer)))
	(progn
	  (delete-area (cursor-pos) (end-of-line))
	  (summary-dispatch 'print item))
      (delete-area (cursor-pos) (forward-line))
      (summary-dispatch 'print item)
      (insert "\n"))
    (goto old-cursor)
    (summary-maybe-dispatch 'after-update)))

(defun summary-goto-item (index)
  "Move the cursor to the INDEX'th item (from zero) in the menu. Returns
t when this item actually exists."
  (interactive "p")
  (unless (or (< index 0)
	      (> index (- (pos-line (end-of-buffer))
			(pos-line summary-first-line))))
    (goto (pos 0 (+ (pos-line summary-first-line) index)))
    (summary-maybe-dispatch 'after-move index)
    t))

(defun summary-next-item (count)
  "Move the cursor to the next item."
  (interactive "p")
  (summary-goto-item (+ (summary-current-index) count)))

(defun summary-previous-item (count)
  "Move the cursor to the previous item."
  (interactive "p")
  (summary-goto-item (- (summary-current-index) count)))

(defun summary-select-item ()
  "Select the current menu item."
  (interactive)
  (summary-dispatch 'select (summary-current-item)))

(defun summary-execute (&optional types)
  "Perform all pending operations on the current summary menu. If TYPES is
non-nil it should be a list containing the operations which may be performed."
  (interactive)
  (let
      ((ops summary-pending-ops))
    (setq summary-pending-ops nil)
    ;; Send a `execute-start' command when it's defined. This
    ;; lets the underlying system start caching if it wants to.
    (summary-maybe-dispatch 'execute-start ops)
    ;; Now start executing the operations
    (while ops
      (let
	  ((item (car (car ops)))
	   (funcs (cdr (car ops))))
	(while funcs
	  (when (or (null types) (memq (car funcs) types))
	    (summary-dispatch (car funcs) item))
	  (setq funcs (cdr funcs))))
      (setq ops (cdr ops)))
    ;; Send a `execute-end' command when it's defined. This
    ;; lets the underlying system end caching and perform the
    ;; operations if necessary.
    (summary-maybe-dispatch 'execute-end))
  (summary-update))

(defun summary-mark-delete ()
  "Mark that the current item should be deleted."
  (interactive)
  (if (assq 'delete summary-functions)
      (let
	  ((item (summary-current-item)))
	(summary-add-pending-op item 'delete))
    (error "No delete operation in the menu.")))

(defun summary-quit ()
  "Leave the summary buffer."
  (interactive)
  (if (summary-function-exists-p 'on-quit)
      (summary-dispatch 'on-quit)
    (bury-buffer (current-buffer))))
