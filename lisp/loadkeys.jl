;;;; loadkeys.jl -- Set up standard keybindings
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

(defvar global-keymap (make-keytab)
  "The root keymap.")

(defvar ctrl-x-keymap (make-keytab)
  "Default `Ctrl-x' keymap.")

(defvar ctrl-x-4-keymap (make-keylist)
  "Default `Ctrl-x 4' keymap.")

(defvar ctrl-x-5-keymap (make-keylist)
  "Default `Ctrl-x 5' keymap.")

(defvar ctrl-x-n-keymap (make-keylist)
  "Default `Ctrl-x n' keymap.")

(defvar ctrl-c-keymap nil
  "Hook to hang major mode `Ctrl-c' keymap from.")
(make-variable-buffer-local 'ctrl-c-keymap)

(defvar user-keymap (make-keylist)
  "Keymap for user-defined bindings, hung from `Ctrl-c'.")

(setq unbound-key-hook nil
      keymap-path '(global-keymap))

(make-variable-buffer-local 'keymap-path)
(make-variable-buffer-local 'unbound-key-hook)

(bind-keys global-keymap
  "Meta-0"	'(numeric-arg 0)
  "Meta-1"	'(numeric-arg 1)
  "Meta-2"	'(numeric-arg 2)
  "Meta-3"	'(numeric-arg 3)
  "Meta-4"	'(numeric-arg 4)
  "Meta-5"	'(numeric-arg 5)
  "Meta-6"	'(numeric-arg 6)
  "Meta-7"	'(numeric-arg 7)
  "Meta-8"	'(numeric-arg 8)
  "Meta-9"	'(numeric-arg 9)
  "Meta--"	'negative-arg
  "Up"		'previous-line
  "Down"	'next-line
  "Left"	'backward-char
  "Right"	'forward-char
  "Shift-Up"	'top-of-buffer
  "Shift-Down"	'bottom-of-buffer
  "Shift-Left"	'start-of-line
  "Shift-Right"	'end-of-line
  "Ctrl-Up"	'prev-screen
  "Ctrl-Down"	'next-screen
  "Meta-Left"	'backward-word
  "Meta-Right"	'forward-word
  "Meta-Up"	'backward-paragraph
  "Meta-Down"	'forward-paragraph
  "Ctrl-TAB"	'forward-tab
  "Meta-TAB"	'backward-tab
  "RET"		'split-line
  "Backspace"	'backspace-char
  "DEL"		'delete-char
  "Shift-Backspace" 'backward-kill-line
  "Shift-DEL"	'kill-line
  "Ctrl-DEL"	'kill-whole-line
  "Meta-DEL"	'kill-word
  "Meta-ESC"	'eval-and-print
  "Meta-Backspace" 'backward-kill-word
  "Ctrl-Meta-Backspace" 'backward-kill-exp
  "Help"	'help
  "Meta-Help"	'toggle-iconic
  "Insert"	'overwrite-mode
  "Home"	'start-of-buffer
  "End"		'end-of-buffer
  "Prior"	'prev-screen
  "Next"	'next-screen
  "Ctrl-@"	'set-auto-mark
  "Meta-@"	'mark-word
  "Meta-%"	'query-replace
  "Meta-!"	'shell-command
  "Meta-|"	'shell-command-on-area
  "Ctrl-a"	'start-of-line
  "Ctrl-b"	'backward-char
  "Meta-b"	'backward-word
  "Ctrl-Meta-b"	'backward-exp
  "Ctrl-c"	'(next-keymap-path '(ctrl-c-keymap user-keymap))
  "Meta-c"	'capitalize-word
  "Ctrl-Meta-c"	'abort-recursive-edit
  "Ctrl-d"	'delete-char
  "Meta-d"	'kill-word
  "Ctrl-e"	'end-of-line
  "Ctrl-f"	'forward-char
  "Meta-f"	'forward-word
  "Ctrl-Meta-f"	'forward-exp
  "Ctrl-h"	'help
  "Meta-h"	'paragraph-edges
  "Ctrl-i"	'insert-block
  "Meta-i"	'(insert "\t")
  "Meta-j"	'goto-line
  "Ctrl-k"	'kill-line
  "Ctrl-Meta-k"	'kill-exp
  "Ctrl-l"	'center-display
  "Meta-l"	'downcase-word
  "Ctrl-m"	'block-toggle
  "Ctrl-M"	'toggle-rect-blocks
  "Meta-m"	'(goto-glyph (indent-pos))
  "Ctrl-n"	'next-line
  "Meta-n"	'(progn (set-auto-mark) (goto (find-matching-bracket)))
  "Ctrl-o"	'open-line
  "Ctrl-p"	'previous-line
  "Ctrl-q"	'(next-keymap-path nil)
  "Meta-q"	'fill-paragraph
  "Ctrl-r"	'isearch-backward
  "Meta-r"	'goto-view-line
  "Ctrl-s"	'isearch-forward
  "Ctrl-t"	'transpose-chars
  "Meta-t"	'transpose-words
  "Ctrl-Meta-t"	'transpose-exps
  "Ctrl-u"	'universal-arg
  "Meta-u"	'upcase-word
  "Ctrl-v"	'next-screen
  "Meta-v"	'prev-screen
  "Ctrl-Meta-v"	'scroll-next-view
  "Ctrl-w"	'kill-block
  "Ctrl-W"	'delete-block
  "Meta-w"	'copy-block-as-kill
  "Ctrl-x"	'(next-keymap-path '(ctrl-x-keymap))
  "Meta-x"	'call-command
  "Ctrl-y"	'yank
  "Ctrl-Y"	'yank-rectangle
  "Meta-y"	'yank-next
  "Ctrl-z"	'toggle-iconic
  "Ctrl-SPC"	'block-toggle
  "Meta-SPC"	'just-spaces
  "Ctrl-]"	'abort-recursive-edit
  "Meta-["	'backward-paragraph
  "Meta-]"	'forward-paragraph
  "Ctrl-_"	'undo
  "Meta-<"	'start-of-buffer
  "Meta->"	'end-of-buffer
  "Meta-."	'find-tag
  "Meta-;"	'insert-comment
  "Meta-~"	'(set-buffer-modified nil nil)
  "Meta-\\"	'no-spaces
  "Ctrl-Meta-\\" 'indent-area
  "LMB-Click1"	'mouse-select
  "Ctrl-LMB-Click1" 'mouse-select
  "LMB-Click2"	'mouse-double-select
  "Ctrl-LMB-Click2" 'mouse-double-select
  "LMB-Move"	'mouse-select-drag-block
  "Ctrl-LMB-Move" 'mouse-select-drag-rect
  "MMB-Click1"	'yank-to-mouse
  "Meta-Shift-LMB-Click1" 'block-kill)

(bind-keys ctrl-x-keymap
  "Ctrl-b"	'buffer-summary
  "Ctrl-c"	'save-and-quit
  "Ctrl-f"	'find-file
  "Ctrl-Meta-f"	'find-url
  "Ctrl-l"	'downcase-area
  "Ctrl-o"	'delete-blank-lines
  "Ctrl-p"	'mark-page
  "Ctrl-q"	'toggle-buffer-read-only
  "Ctrl-r"	'find-file-read-only
  "Ctrl-s"	'save-file
  "Ctrl-t"	'transpose-lines
  "Ctrl-Meta-t"	'transpose-paragraphs
  "Ctrl-u"	'upcase-area
  "Ctrl-v"	'find-alternate-file
  "Ctrl-w"	'save-file-as
  "Ctrl-x"	'swap-cursor-and-auto-mark
  "0"		'delete-view
  "1"		'delete-other-views
  "2"		'split-view
  "4"		'(next-keymap-path '(ctrl-x-4-keymap))
  "5"		'(next-keymap-path '(ctrl-x-5-keymap))
  "b"		'switch-to-buffer
  "f"		'set-fill-column
  "."		'set-fill-prefix
  "h"		'mark-whole-buffer
  "i"		'insert-file
  "k"		'kill-buffer
  "m"		'mail-setup
  "n"		'(next-keymap-path '(ctrl-x-n-keymap))
  "o"		'goto-next-view
  "s"		'save-some-buffers
  "u"		'undo
  "`"		'next-error
  "#"		'server-close-file
  "["		'backward-page
  "]"		'forward-page
  "|"		'shell-command-on-buffer
  "^"		'enlarge-view
  "-"		'shrink-view-if-larger-than-buffer)

(bind-keys ctrl-x-n-keymap
  "p"		'restrict-to-page
  "w"		'unrestrict-buffer
  "n"		'restrict-buffer)

(bind-keys ctrl-x-4-keymap
  "Ctrl-f"	'(in-other-view 'find-file)
  "a"		'(in-other-view 'add-change-log-entry)
  "b"		'(in-other-view 'switch-to-buffer)
  "f"		'(in-other-view 'find-file)
  "h"		'(in-other-view 'help)
  "i"		'(in-other-view 'info)
  "`"		'(in-other-view 'next-error))

(bind-keys ctrl-x-5-keymap
  "Ctrl-f"	'(in-new-window 'find-file)
  "a"		'(in-new-window 'add-change-log-entry)
  "b"		'(in-new-window 'switch-to-buffer)
  "f"		'(in-new-window 'find-file)
  "h"		'(in-new-window 'help)
  "i"		'(in-new-window 'info)
  "`"		'(in-new-window 'next-error)
  "0"		'delete-window
  "2"		'make-window)

(defun self-insert (event-string)
  "This function can be bound to a key to make it insert its usual character
sequence. This is usually used to ``hide'' a previously bound definition of
the key."
  (interactive "E")
  (insert event-string))

(defun numeric-arg (digit)
  "Add a digit to the prefix-arg."
  (when (numberp digit)
    ;; Set the `next-keymap-path' to ensure echoing
    ;; continues. `prefix-arg' *must* be set after
    ;; `next-keymap-path' for this all to work!
    (next-keymap-path keymap-path)
    (setq prefix-arg (cond
		      ((numberp current-prefix-arg)
		       (+ (* current-prefix-arg 10) digit))
		      ((eq current-prefix-arg '-)
		       (- digit))
		      (t
		       digit)))))

(defun negative-arg (arg)
  "Negate the prefix-arg. Bound to `Meta--'. "
  (interactive "P")
  (next-keymap-path keymap-path)
  (setq prefix-arg (cond
		    ((numberp arg)
		     (* arg -1))
		    ((eq arg '-)
		     nil)
		    (t
		     '-))))

(defun universal-arg (arg)
  (interactive "P")
  (next-keymap-path keymap-path)
  (setq prefix-arg (cond
		    ((consp arg)
		     (rplaca arg (* 4 (car arg)))
		     arg)
		    (t
		     (cons 4 nil)))))
