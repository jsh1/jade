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

(setq mark-1 (make-mark)
      mark-2 (make-mark)
      mark-3 (make-mark))

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
  "Up"		'goto-prev-line
  "Down"	'goto-next-line
  "Left"	'goto-left-char
  "Right"	'goto-right-char
  "Shift-Up"	'(progn (set-auto-mark) (goto-char (pos nil 0)))
  "Shift-Down"	'(progn (set-auto-mark) (goto-char (pos nil (1- (buffer-length)))))
  "Shift-Left"	'goto-line-start
  "Shift-Right"	'goto-line-end
  "Ctrl-Up"	'prev-screen
  "Ctrl-Down"	'next-screen
  "Ctrl-Left"	'(goto-char (left-char 40))
  "Ctrl-Right"	'(goto-char (right-char 40))
  "Meta-Left"	'backward-word
  "Meta-Right"	'forward-word
  "Meta-Up"	'backward-paragraph
  "Meta-Down"	'forward-paragraph
  "Ctrl-TAB"	'goto-next-tab
  "Shift-TAB"	'goto-prev-tab
  "RET"		'split-line
  "Backspace"	'backspace-char
  "DEL"		'delete-char
  "Shift-Backspace" 'backward-kill-line
  "Shift-DEL"	'kill-line
  "Ctrl-DEL"	'kill-whole-line
  "Meta-DEL"	'kill-word
  "Meta-ESC"	'eval-and-print
  "Meta-TAB"	'goto-next-tab
  "Meta-Backspace" 'backward-kill-word
  "Ctrl-Meta-Backspace" 'backward-kill-exp
  "Help"	'help
  "Meta-Help"	'toggle-iconic
  "Insert"	'overwrite-mode
  "Home"	'goto-buffer-start
  "End"		'goto-buffer-end
  "Prior"	'prev-screen
  "Next"	'next-screen
  "Ctrl-@"	'set-auto-mark
  "Meta-@"	'mark-word
  "Meta-%"	'query-replace
  "Meta-!"	'shell-command
  "Meta-|"	'shell-command-on-area
  "Ctrl-a"	'goto-line-start
  "Ctrl-b"	'goto-prev-char
  "Meta-b"	'backward-word
  "Ctrl-Meta-b"	'backward-exp
  "Ctrl-c"	'(setq next-keymap-path '(ctrl-c-keymap user-keymap))
  "Meta-c"	'capitalize-word
  "Ctrl-Meta-c"	'abort-recursive-edit
  "Ctrl-d"	'delete-char
  "Meta-d"	'kill-word
  "Ctrl-e"	'goto-line-end
  "Ctrl-f"	'goto-next-char
  "Meta-f"	'forward-word
  "Ctrl-Meta-f"	'forward-exp
  "Ctrl-h"	'help
  "Meta-h"	'mark-paragraph
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
  "Ctrl-n"	'goto-next-line
  "Meta-n"	'(progn (set-auto-mark) (goto-char (match-brackets)))
  "Ctrl-o"	'open-line
  "Ctrl-p"	'goto-prev-line
  "Ctrl-q"	'(setq next-keymap-path t)
  "Meta-q"	'fill-paragraph
  "Ctrl-r"	'isearch-backward
  "Ctrl-s"	'isearch-forward
  "Ctrl-t"	'transpose-chars
  "Meta-t"	'transpose-words
  "Ctrl-Meta-t"	'transpose-exps
  "Ctrl-u"	'universal-arg
  "Meta-u"	'upcase-word
  "Ctrl-v"	'next-screen
  "Meta-v"	'prev-screen
  "Ctrl-w"	'kill-block
  "Ctrl-W"	'delete-block
  "Meta-w"	'copy-block-as-kill
  "Ctrl-x"	'(setq next-keymap-path '(ctrl-x-keymap))
  "Meta-x"	'call-command
  "Ctrl-y"	'yank
  "Ctrl-Y"	'yank-rectangle
  "Meta-y"	'yank-next
  "Ctrl-z"	'toggle-iconic
  "Ctrl-."	'rotate-buffers-forward
  "Ctrl-,"	'rotate-buffers-backward
  "Ctrl-SPC"	'block-toggle
  "Meta-SPC"	'just-spaces
  "Ctrl-]"	'abort-recursive-edit
  "Meta-["	'backward-paragraph
  "Meta-]"	'forward-paragraph
  "Ctrl-_"	'undo
  "Meta-<"	'(progn (set-auto-mark) (goto-buffer-start))
  "Meta->"	'(progn (set-auto-mark) (goto-buffer-end))
  "Meta-;"	'insert-comment
  "Meta-~"	'(set-buffer-modified nil nil)
  "Meta-\\"	'no-spaces
  "Ctrl-Meta-\\" 'indent-area
  "F1"		'(goto-mark mark-1)
  "F2"		'(goto-mark mark-2)
  "F3"		'(goto-mark mark-3)
  "Shift-F1"	'(set-mark mark-1 (cursor-pos) (current-buffer))
  "Shift-F2"	'(set-mark mark-2 (cursor-pos) (current-buffer))
  "Shift-F3"	'(set-mark mark-3 (cursor-pos) (current-buffer))
  "LMB-Click1"	'mouse-select
  "LMB-Click2"	'mouse-double-select
  "LMB-Move"	'mouse-select-drag
  "MMB-Click1"	'yank-to-mouse
  "RMB-Click1"	'toggle-iconic
  "Meta-Shift-LMB-Click1" 'block-kill)

(bind-keys ctrl-x-keymap
  "Ctrl-b"	'buffer-summary
  "Ctrl-c"	'save-and-quit
  "Ctrl-f"	'find-file
  "Ctrl-l"	'downcase-area
  "Ctrl-o"	'delete-blank-lines
  "Ctrl-p"	'mark-page
  "Ctrl-q"	'toggle-buffer-read-only
  "Ctrl-r"	'find-file-read-only
  "Ctrl-s"	'save-file
  "Ctrl-t"	'transpose-lines
  "Ctrl-u"	'upcase-area
  "Ctrl-v"	'find-alternate-file
  "Ctrl-w"	'save-file-as
  "Ctrl-x"	'swap-cursor-and-auto-mark
  "0"		'close-view
  "1"		'close-other-views
  "2"		'open-view
  "4"		'(setq next-keymap-path '(ctrl-x-4-keymap))
  "5"		'(setq next-keymap-path '(ctrl-x-5-keymap))
  "b"		'switch-to-buffer
  "f"		'set-fill-column
  "h"		'mark-whole-buffer
  "i"		'insert-file
  "k"		'kill-buffer
  "m"		'mail-setup
  "n"		'(setq next-keymap-path '(ctrl-x-n-keymap))
  "o"		'goto-next-view
  "s"		'save-some-buffers
  "u"		'undo
  "`"		'next-error
  "#"		'server-close-file
  "["		'backward-page
  "]"		'forward-page
  "|"		'shell-command-on-buffer)

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
  "0"		'close-window
  "2"		'open-window)

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
    (setq next-keymap-path keymap-path
	  prefix-arg (cond
		      ((numberp current-prefix-arg)
		       (+ (* current-prefix-arg 10) digit))
		      ((eq current-prefix-arg '-)
		       (- digit))
		      (t
		       digit)))))

(defun negative-arg (arg)
  "Negate the prefix-arg. Bound to `Meta--'. "
  (interactive "P")
  (setq next-keymap-path keymap-path
	prefix-arg (cond
		    ((numberp arg)
		     (* arg -1))
		    ((eq arg '-)
		     nil)
		    (t
		     '-))))

(defun universal-arg (arg)
  (interactive "P")
  (setq next-keymap-path keymap-path
	prefix-arg (cond
		    ((consp arg)
		     (rplaca arg (* 4 (car arg)))
		     arg)
		    (t
		     (cons 4 nil)))))
