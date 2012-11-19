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

(defvar summary-keymap
  (bind-keys (make-sparse-keymap)
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
    "U" 'summary-unmark-all
    "m" 'summary-mark-item
    "Button1-Click2" 'summary-select-item)
  "Base keymap for modes deriving from summary.")

(defvar summary-mouse-map
  (bind-keys (make-sparse-keymap)
    "button2-click1" 'goto-mouse
    "button2-move" 'goto-mouse
    "button2-off" 'summary-select-mouse-item))


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
;;	a pending operation), but before updating the display
;;
;;   after-move INDEX
;;	Called after moving the cursor to the item at position INDEX
;;	in the most recently given list of items
;;
;;   after-update
;;	Called after updating the buffer display
;;
;;   with-extent EXTENT
;;	Called each time a new extent is created for a summary item
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

(defvar summary-assoc-item-function assq
  "Should be either assq or assoc, used for comparing items with their
alists. Normally assq will be best, in some case (items are strings)
assoc is needed.")
(make-variable-buffer-local 'summary-assoc-item-function)

(defvar summary-actual-keymap nil)
(make-variable-buffer-local 'summary-actual-keymap)


;; Entry point

;; Call this to initialise a summary menu in the current buffer. NAME
;; is put in mode-name, FUNCTIONS in summary-functions. If KEYMAP is
;; non-nil it is used, otherwise summary-keymap is.
;;  The current end of the buffer is taken as the point from which
;; the menu should be displayed.
;;  Note that a copy of the FUNCTIONS list is made, this allows easy
;; modification by hooks, without changing anything in other buffers
(defun summary-mode (name functions #!optional keymap)
  "Summary Mode:\n
This major mode provides a generic menu capability. It allows lists of
items to be displayed and manipulated."
  (when major-mode-kill
    (major-mode-kill))
  (setq summary-functions (copy-sequence functions)
	summary-pending-ops nil
	summary-first-line (start-of-line (end-of-buffer))
	summary-actual-keymap (or keymap summary-keymap)
	major-mode 'summary-mode
	major-mode-kill summary-mode-kill
	mode-name name
	local-keymap summary-actual-keymap)
  (set-buffer-undo-list nil)
  (set-buffer-record-undo nil)
  (truncate-lines t)
  (set-buffer-read-only (current-buffer) t)
  (add-hook 'unbound-key-hook nop)
  (call-hook 'summary-mode-hook)
  (summary-update))

(defun summary-mode-kill ()
  (setq major-mode nil
	major-mode-kill nil
	mode-name nil
	local-keymap nil)
  (remove-hook 'unbound-key-hook 'nop))



;; Call the function FUNC with ARGS
(defmacro summary-dispatch (func #!rest args)
  (cons 'funcall
	(cons (list 'cdr (list 'assq func 'summary-functions))
	      args)))

;; Returns t when function FUNC exists for the current summary
(defmacro summary-function-exists-p (func)
  (list 'assq func 'summary-functions))

;; Call the function FUNC with ARGS, only if it exists.
(defmacro summary-maybe-dispatch (func #!rest args)
  (list 'and
	(list 'summary-function-exists-p func)
	(cons 'summary-dispatch (cons func args))))

(defmacro summary-get-item (index)
  "Return the item at position INDEX in the menu (from zero)."
  (list 'nth index 'summary-items))

(defun summary-get-index (item)
  "Return the index in the menu at which ITEM is displayed, or nil if ITEM
isn't displayed in the summary."
  (let
      ((tail (memq item summary-items)))
    (if tail
	(- (length summary-items) (length tail))
      nil)))

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
       (end (pos (max (car (view-dimensions)) (line-length start))
		 (pos-line start)))
       extents)
    (map-extents (lambda (e)
		   (when (extent-get e 'summary-highlight)
		     (setq extents (cons e extents))))
		 (start-of-buffer) (extent-end (extent-root)))
    (mapc delete-extent extents)
    (make-extent start end
		 (list 'face highlight-face 'summary-highlight t))))

(defmacro summary-get-pending-ops (item)
  "Return the list of operations pending on ITEM."
  (list 'summary-assoc-item-function item 'summary-pending-ops))

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
	  (summary-maybe-dispatch 'after-marking item)
	  (summary-update-item item))
      (setq summary-pending-ops (cons (cons item (cons op nil))
				      summary-pending-ops))
      (summary-maybe-dispatch 'after-marking item)
      (summary-update-item item))))

(defun summary-unmark-item (item)
  "Discard all operations pending on ITEM."
  (interactive (list (summary-current-item) t))
  (let
      ((ops (summary-get-pending-ops item)))
    (when ops
      (setq summary-pending-ops (delq ops summary-pending-ops)))
    (summary-maybe-dispatch 'after-marking item)
    (when ops
      (summary-update-item item))))

(defun summary-unmark-all ()
  "Discard all pending operations."
  (interactive)
  (let
      ((old-ops summary-pending-ops))
    (setq summary-pending-ops nil)
    (when (summary-function-exists-p 'after-marking)
      (mapc (lambda (cell)
	      (summary-dispatch 'after-marking (car cell))) old-ops))
    (summary-update)))

(defun summary-update ()
  "Redraw the menu, after rebuilding the list of items. Loses the current
highlight."
  (interactive)
  (let
      ((inhibit-read-only t))
    (block-kill)
    (delete-area summary-first-line (end-of-buffer))
    (delete-all-extents)
    (setq summary-items (summary-dispatch 'list))
    (goto summary-first-line)
    (let
	((items summary-items)
	 extent)
      (while items
	(summary-dispatch 'print (car items))
	(setq extent (make-extent (start-of-line) (cursor-pos)
				  (list 'mouse-face active-face
					'mouse-keymap summary-mouse-map)))
	(summary-maybe-dispatch 'with-extent extent)
	(setq items (cdr items))
	(when items
	  (insert "\n")))
      (summary-goto-item (or (summary-maybe-dispatch 'current) 0))
      (set-buffer-modified nil nil)
      (summary-maybe-dispatch 'after-update))))

(defun summary-update-item (item)
  "Redraw the menu entry for ITEM."
  (let*
      ((inhibit-read-only t)
       (index (summary-get-index item))
       (old-cursor (cursor-pos)))
    (when index
      (goto (pos 0 (+ (pos-line summary-first-line) index)))
      (if (= (pos-line (cursor-pos)) (pos-line (end-of-buffer)))
	  (progn
	    (delete-area (cursor-pos) (end-of-line))
	    (summary-dispatch 'print item)
	    (make-extent (start-of-line) (cursor-pos)
			 (list 'mouse-face active-face
			       'mouse-keymap summary-mouse-map)))
	(delete-area (cursor-pos) (forward-line))
	(summary-dispatch 'print item)
	(make-extent (start-of-line) (cursor-pos)
		     (list 'mouse-face active-face
			   'mouse-keymap summary-mouse-map))
	(insert "\n"))
      (goto old-cursor)
      (set-buffer-modified nil nil)
      (summary-maybe-dispatch 'after-update))))

(defun summary-goto-item (index)
  "Move the cursor to the INDEX'th item (from zero) in the menu. Returns
t when this item actually exists."
  (interactive "p")
  (unless (or (null index)
	      (< index 0)
	      (> index (- (pos-line (end-of-buffer))
			(pos-line summary-first-line))))
    (goto (pos 0 (+ (pos-line summary-first-line) index)))
    (summary-maybe-dispatch 'after-move index)
    t))

(defun summary-next-item (count)
  "Move the cursor to the next item."
  (interactive "p")
  (summary-goto-item (condition-case nil
			 (+ (summary-current-index) count)
		       (error 0))))

(defun summary-previous-item (count)
  "Move the cursor to the previous item."
  (interactive "p")
  (summary-goto-item (condition-case nil
			 (- (summary-current-index) count)
		       (error 0))))

(defun summary-select-item ()
  "Select the current menu item."
  (interactive)
  (summary-dispatch 'select (summary-current-item)))

(defun summary-select-mouse-item ()
  "Select the menu item under the mouse cursor."
  (interactive)
  (goto-mouse)
  (summary-select-item))

(defun summary-execute (#!optional types)
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
	   (funcs (cdr (car ops)))
	   (kept '()))
	(while funcs
	  (if (and (or (null types) (memq (car funcs) types))
		   (not (eq (car funcs) 'mark)))
	      (progn
		(summary-dispatch (car funcs) item)
		(when (eq (car funcs) 'delete)
		  (setq kept nil)))	    
	    (setq kept (cons (car funcs) kept)))
	  (setq funcs (cdr funcs)))
	(when kept
	  (setq summary-pending-ops (cons (cons item kept)
					  summary-pending-ops))))
      (setq ops (cdr ops)))
    ;; Send a `execute-end' command when it's defined. This
    ;; lets the underlying system end caching and perform the
    ;; operations if necessary.
    (summary-maybe-dispatch 'execute-end))
  (summary-update))

(defun summary-mark-item (op #!optional item count)
  "Add operation OP to ITEM (or the current item) for future use. If ITEM
is nil and COUNT is a number, mark COUNT items starting with the current
item."
  (interactive (list 'mark nil (prefix-numeric-argument current-prefix-arg)))
  (if item
      ;; Told explicitly to mark _this_ item
      (summary-add-pending-op item op)
    ;; Mark COUNT items from the current. If COUNT is negative mark
    ;; backwards
    (let
	((start (summary-current-index)))
      (if count
	  (when (< count 0)
	    (setq count (- count)
		  start (max (1+ (- start count)) 0)))
	(setq count 1))
      (setq item (nthcdr start summary-items))
      (while (and (> count 0) item)
	(summary-add-pending-op (car item) op)
	(setq item (cdr item)
	      count (1- count))))))

(defun summary-mark-if (pred #!optional op)
  "Mark all items that satisfy the predicate function PRED, optionally
using tag OP (by default the `mark' tag)."
  (mapc (lambda (x)
	  (when (pred x)
	    (summary-mark-item (or op 'mark) x)))
	summary-items))

(defun summary-mark-delete (#!optional item count)
  "Mark that ITEM, or the current item, should be deleted."
  (interactive "\np")
  (if (assq 'delete summary-functions)
      (summary-mark-item 'delete item count)
    (error "No delete operation in this summary")))

(defun summary-map-marked-items (function #!optional preserve-marks)
  "Map FUNCTION over all marked items in the current buffer. Unless
PRESERVE-MARKS is t, all marks are unset. FUNCTION is called as
(FUNCTION MARKED-ITEM)."
  (mapc (lambda (o)
	  (when (memq 'mark (cdr o))
	    (unless preserve-marks
	      (rplacd o (delq 'mark (cdr o))))
	    (function (car o))))
	summary-pending-ops))

(defun summary-command-items ()
  "Return a list of items from the current summary buffer. Either all marked
items, or if no items are marked, the item under the cursor."
  (or (filter (lambda (x)
		(memq 'mark (summary-get-pending-ops x)))
	      summary-items)
      (let
	  ((arg (prefix-numeric-argument current-prefix-arg))
	   (current (summary-current-index)))
	(if (= arg 1)
	    (list (summary-get-item current))
	  (when (< arg 0)
	    (setq current (+ current arg 1)
		  arg (- arg))
	    (when (< current 0)
	      (setq arg (+ arg current)
		    current 0)))
	  (let
	      ((in (nthcdr current summary-items))
	       (out nil))
	    (while (and (> arg 0) in)
	      (setq out (cons (car in) out)
		    in (cdr in)
		    arg (1- arg)))
	    (nreverse out))))))

(defun summary-item-marked-p (item)
  "Returns t if ITEM is marked for future use."
  (memq 'mark (cdr (summary-get-pending-ops item))))

(defun summary-quit ()
  "Leave the summary buffer."
  (interactive)
  (if (summary-function-exists-p 'on-quit)
      (summary-dispatch 'on-quit)
    (bury-buffer (current-buffer))))
