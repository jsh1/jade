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

(defvar global-keymap (make-keymap)
  "The root keymap, active in all buffers.")

(defvar local-keymap nil
  "A keymap local to the current buffer.")
(make-variable-buffer-local 'local-keymap)

(defvar local-ctrl-c-keymap nil
  "A keymap hung from C-c, local to the current buffer.")
(make-variable-buffer-local 'local-ctrl-c-keymap)

(defvar overriding-local-keymap nil
  "When non-nil, a keymap to search instead of all extent, minor mode, or
local keymaps.")

(defvar ctrl-x-keymap (make-keymap)
  "Default `C-x' keymap.")

(defvar ctrl-x-4-keymap (make-sparse-keymap)
  "Default `C-x 4' keymap.")

(defvar ctrl-x-5-keymap (make-sparse-keymap)
  "Default `C-x 5' keymap.")

(defvar ctrl-x-n-keymap (make-sparse-keymap)
  "Default `C-x n' keymap.")

(defvar user-keymap (make-sparse-keymap)
  "Keymap for user-defined bindings, hung from `C-c'.")

(defvar unbound-key-hook nil
  "Called when no binding can be found for the current event.")
(make-variable-buffer-local 'unbound-key-hook)

(defvar minor-mode-keymap-alist nil
  "List of (SYMBOL . KEYMAP) defining minor-mode keybindings. If the value
of SYMBOL is non-nil, KEYMAP is used to search for bindings.")

(defvar current-command-from-mouse nil
  "When non-nil the current command was invoked through the mouse.")

(bind-keys global-keymap
  "M-0"	'(numeric-arg 0)
  "M-1"	'(numeric-arg 1)
  "M-2"	'(numeric-arg 2)
  "M-3"	'(numeric-arg 3)
  "M-4"	'(numeric-arg 4)
  "M-5"	'(numeric-arg 5)
  "M-6"	'(numeric-arg 6)
  "M-7"	'(numeric-arg 7)
  "M-8"	'(numeric-arg 8)
  "M-9"	'(numeric-arg 9)
  "M--"	'negative-arg
  "Up"		'previous-line
  "Down"	'next-line
  "Left"	'backward-char
  "Right"	'forward-char
  "S-Up"	'top-of-buffer
  "S-Down"	'bottom-of-buffer
  "S-Left"	'start-of-line
  "S-Right"	'end-of-line
  "C-Up"	'prev-screen
  "C-Down"	'next-screen
  "M-Left"	'backward-word
  "M-Right"	'forward-word
  "M-Up"	'backward-paragraph
  "M-Down"	'forward-paragraph
  "C-TAB"	'forward-tab
  "M-TAB"	'complete-at-point
  "RET"		'split-line
  "BS"		'backspace-char
  "DEL"		'delete-char
  "S-BS"	'backward-kill-line
  "S-DEL"	'kill-line
  "C-DEL"	'kill-whole-line
  "M-DEL"	'kill-word
  "M-ESC"	'eval-and-print
  "M-BS"	'backward-kill-word
  "C-M-BS"	'backward-kill-exp
  "Home"	'start-of-buffer
  "End"		'end-of-buffer
  "Prior"	'prev-screen
  "Next"	'next-screen
  "C-@"		'set-auto-mark
  "M-@"		'mark-word
  "M-%"		'query-replace
  "M-?"		'show-completions
  "M-/"		'show-completions
  "M-!"		'shell-command
  "M-|"		'shell-command-on-area
  "C-a"		'start-of-line
  "C-M-a"	'start-of-defun
  "C-b"		'backward-char
  "M-b"		'backward-word
  "C-M-b"	'backward-exp
  "C-c"		'local-ctrl-c-keymap
  "C-c"		'user-keymap
  "M-c"		'capitalize-word
  "C-M-c"	'abort-recursive-edit
  "C-d"		'delete-char
  "M-d"		'kill-word
  "C-e"		'end-of-line
  "C-M-e"	'end-of-defun
  "C-f"		'forward-char
  "M-f"		'forward-word
  "C-M-f"	'forward-exp
  "C-h"		'help
  "M-h"		'paragraph-edges
  "C-M-h"	'mark-defun
  "C-i"		'yank-block
  "M-i"		'(insert "\t")
  "M-j"		'goto-line
  "C-k"		'kill-line
  "C-M-k"	'kill-exp
  "C-l"		'center-display
  "M-l"		'downcase-word
  "C-m"		'block-toggle
  "C-M"		'toggle-rect-blocks
  "M-m"		'(goto-glyph (indent-pos))
  "C-n"		'next-line
  "M-n"		'find-matching-bracket
  "C-o"		'open-line
  "C-p"		'previous-line
  "C-q"		'quoted-insert
  "M-q"		'fill-paragraph
  "C-r"		'isearch-backward
  "M-r"		'goto-view-line
  "C-s"		'isearch-forward
  "C-t"		'transpose-chars
  "M-t"		'transpose-words
  "C-M-t"	'transpose-exps
  "C-u"		'universal-arg
  "M-u"		'upcase-word
  "C-v"		'next-screen
  "M-v"		'prev-screen
  "C-M-v"	'scroll-next-view
  "C-w"		'kill-block
  "C-W"		'delete-block
  "M-w"		'copy-block-as-kill
  "C-x"		'ctrl-x-keymap
  "M-x"		'call-command
  "C-y"		'yank
  "C-Y"		'yank-rectangle
  "M-y"		'yank-next
  "C-z"		'toggle-iconic
  "C-SPC"	'block-toggle
  "M-SPC"	'just-spaces
  "C-]"		'abort-recursive-edit
  "M-["		'backward-paragraph
  "M-]"		'forward-paragraph
  "C-_"		'undo
  "M-<"		'start-of-buffer
  "M->"		'end-of-buffer
  "M-."		'find-tag
  "M-,"		'tags-loop-continue
  "M-*"		'pop-tag-mark
  "M-;"		'insert-comment
  "M-~"		'(set-buffer-modified nil nil)
  "M-\\"	'no-spaces
  "\)"		'blinking-insert
  "}"		'blinking-insert
  "]"		'blinking-insert
  "C-\\"	'indent-area
  "C-M-\\"	'indent-defun
  "Button1-Click1" 'mouse-select
  "C-Button1-Click1" 'mouse-select
  "Button1-Click2" 'mouse-double-select
  "C-Button1-Click2" 'mouse-double-select
  "Button1-Move" 'mouse-select-drag-block
  "C-Button1-Move" 'mouse-select-drag-rect
  "Button2-Click1" 'yank-to-mouse
  "Button3-Click1" 'popup-menu
  "M-z"		'popup-menu-from-kbd)

(bind-keys ctrl-x-keymap
  "C-b"		'buffer-summary
  "C-c"		'save-and-quit
  "C-f"		'find-file
  "C-M-f"	'find-url
  "C-l"		'downcase-area
  "C-o"		'delete-blank-lines
  "C-p"		'mark-page
  "C-q"		'toggle-buffer-read-only
  "C-r"		'find-file-read-only
  "C-s"		'save-file
  "C-t"		'transpose-lines
  "C-M-t"	'transpose-paragraphs
  "C-u"		'upcase-area
  "C-v"		'find-alternate-file
  "C-w"		'save-file-as
  "C-x"		'swap-cursor-and-auto-mark
  "0"		'delete-view
  "1"		'delete-other-views
  "2"		'split-view
  "4"		'ctrl-x-4-keymap
  "5"		'ctrl-x-5-keymap
  "b"		'switch-to-buffer
  "f"		'set-fill-column
  "."		'set-fill-prefix
  "h"		'mark-whole-buffer
  "i"		'insert-file
  "k"		'kill-buffer
  "m"		'mail-setup
  "n"		'ctrl-x-n-keymap
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
  "C-f"		'(in-other-view 'find-file)
  "a"		'(in-other-view 'add-change-log-entry)
  "b"		'(in-other-view 'switch-to-buffer)
  "f"		'(in-other-view 'find-file)
  "h"		'(in-other-view 'help)
  "i"		'(in-other-view 'info)
  "`"		'(in-other-view 'next-error))

(bind-keys ctrl-x-5-keymap
  "C-f"		'(in-new-window 'find-file)
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
    (next-keymap-path 'global-keymap)
    (setq prefix-arg (cond
		      ((numberp current-prefix-arg)
		       (+ (* current-prefix-arg 10) digit))
		      ((eq current-prefix-arg '-)
		       (- digit))
		      (t
		       digit))
	  this-command last-command)))

(defun negative-arg (arg)
  "Negate the prefix-arg. Bound to `M--'. "
  (interactive "P")
  (next-keymap-path 'global-keymap)
  (setq prefix-arg (cond
		    ((numberp arg)
		     (* arg -1))
		    ((eq arg '-)
		     nil)
		    (t
		     '-))
	this-command last-command))

(defun universal-arg (arg)
  (interactive "P")
  (next-keymap-path 'global-keymap)
  (setq prefix-arg (cond
		    ((consp arg)
		     (rplaca arg (* 4 (car arg)))
		     arg)
		    (t
		     (cons 4 nil)))
	this-command last-command))
