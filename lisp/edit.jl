;;;; edit.jl -- High-level editing functions
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

(require 'ring)

(defvar word-regexp "[a-zA-Z0-9]"
  "Regular expression which defines a character in a word.")
(defvar word-not-regexp "[^a-zA-Z0-9]"
  "Regular expression which defines anything that is not in a word.")

(defvar paragraph-separate "^[\t\f ]*\n"
  "Regular expression matching a line separating paragraphs, but not matching
the start of a paragraph itself. The start of the paragraph is taken as the
character following the end of the match.")

(defvar paragraph-start "^[\t\f ]*$"
  "Regular expression matching the start of a paragraph. See also the variable
`paragraph-separate'.")

(defvar page-start "^\f"
  "Regular expression that matches the start of a page of text.")

(defvar indent-tabs-mode t
  "Indentation can insert tabs if this is non-nil.")

(make-variable-buffer-local 'word-regexp)
(make-variable-buffer-local 'word-not-regexp)
(make-variable-buffer-local 'paragraph-separate)
(make-variable-buffer-local 'paragraph-start)
(make-variable-buffer-local 'page-start)
(make-variable-buffer-local 'indent-tabs-mode)

(defvar toggle-read-only-function nil
  "May contain function to call when toggling a buffer between read-only
and writable.")
(make-variable-buffer-local 'toggle-read-only-function)

(defvar auto-mark nil
  "Buffer-local mark which some commands use to track the previous cursor
position.")
(make-variable-buffer-local 'auto-mark)

(defvar kill-ring-size 8
  "Maximum number of strings stored in the kill ring at any one time.")

(defvar mouse-yank-at-point t
  "When this variable is non-nil, any yanks by the command `yank-to-mouse'
will insert at the current cursor position, _without_ first moving the
cursor to the position of the mouse pointer.")

(defvar blink-matching-paren t
  "Defines whether or not characters with matching open delimiters (i.e. #\\),
#\\}, and #\\]) blink the opening delimiter when the closing delimiter is
entered. Blinking consists of temporarily moving the cursor to the opening
delimiter, or if this would require moving the cursor off the viewable area,
displaying the line containing the opening delimiter in the message area.

When this variable is nil, blinking is disabled. If a list, then blinking is
only enabled if the list contains the closing delimiter being blinked.")

(defvar blink-matching-delay 1
  "The number of seconds to delay when blinking parentheses.")


;; Some macros

(defmacro pos-col (p)
  "Return the column pointed to by position P."
  (list 'cdr p))

(defmacro pos-line (p)
  "Return the row pointed to by position P."
  (list 'car p))


;; Marks

(defun goto-mark (mark #!optional dont-set-auto)
  "Switches (if necessary) to the buffer containing MARK at the position
of the mark. If the file containing MARK is not in memory then we
attempt to load it by calling find-file. Unless DONT-SET-AUTO is non-nil
the auto-mark of the current buffer will be updated before the move."
  (when (markp mark)
    (let
	((file (mark-file mark))
	 (p (mark-pos mark)))
      (unless dont-set-auto
	(set-auto-mark))
      (if (string? file)
	  (find-file file)
	(goto-buffer file))
      (goto p))))

(defun set-auto-mark (#!optional p)
  "Sets the mark `auto-mark' to POS (or the current position)."
  (interactive)
  (if auto-mark
      (progn
	(set-mark-pos auto-mark (or p (cursor-pos)))
	(set-mark-file auto-mark (current-buffer)))
    (set! auto-mark (make-mark p)))
  (message "Set auto-mark."))

(defun swap-cursor-and-auto-mark ()
  "Sets the `auto-mark' to the current position and then sets the current
position (buffer and cursor-pos) to the old value of `auto-mark'."
  (interactive)
  (unless auto-mark
    (error "The auto mark isn't set in this buffer."))
  (let
      ((a-m-file (mark-file auto-mark))
       (a-m-pos (mark-pos auto-mark)))
    (set-auto-mark)
    (if (string? a-m-file)
	(find-file a-m-file)
      (goto-buffer a-m-file))
    (goto a-m-pos)))


;; Characters

(defun backward-char (#!optional count p buf)
  (interactive "@p")
  (forward-char (if count (- count) -1) p buf))

(defun transpose-chars (count)
  "Move the character before the cursor COUNT characters forwards."
  (interactive "p")
  (transpose-items forward-char backward-char count))

(defun backward-tab (#!optional count p size)
  (interactive "@p")
  (forward-tab (if count (- count) -1) p size))

(defmacro right-char (count p)
  "Return the position COUNT characters to the right of POS."
  (list 'pos (list '+ (list 'pos-col p) count)
	(list 'pos-line p)))

(defmacro left-char (count p)
  "Return the position COUNT characters to the left of POS."
  (list 'right-char (list '- count) p))


;; Lines

;; When moving up and down lines, the column that we are aiming for
(defvar goal-column nil)

;; The view that the goal-column refers to
(defvar goal-view nil)

(defun backward-line (#!optional count p)
  "Return the position of the line COUNT lines below position POS. (defaults
to one line from the cursor)."
  (interactive "@p")
  (forward-line (if count (- count) -1) p))

(defun next-line (count)
  "Return the position of the line COUNT lines below the cursor, adjusting
the column position to preserve the original position."
  (interactive "@p")
  (unless (and (eq? last-command 'next-line)
	       (eq? goal-view (current-view)))
    ;; Set new goal
    (set! goal-column (pos-col (char-to-glyph-pos (cursor-pos))))
    (set! goal-view (current-view)))
  (set! this-command 'next-line)
  (glyph-to-char-pos (pos goal-column (+ (pos-line (cursor-pos)) count))))

(defun previous-line (count)
  "Return the position of the line COUNT lines above the cursor, adjusting
the column position to preserve the original position."
  (interactive "@p")
  (next-line (- count)))

(defun split-line (#!optional count p)
  "Insert COUNT newline characters before position POS (or before the
cursor)."
  (interactive "p")
  (insert (if (or (not count) (= count 1))
	      "\n"
	    (make-string count #\newline)) p))

(defun kill-line (#!optional arg)
  "If the cursor is not at the end of the line kill the text from the cursor
to the end of the line, else kill from the end of the line to the start of
the next line."
  (interactive "P")
  (let
      ((count (prefix-numeric-argument arg))
       (start (cursor-pos))
       end)
    (cond
     ((null? arg)
      (set! end (if (>= start (end-of-line))
		    (start-of-line (forward-line))
		  (end-of-line))))
     ((> count 0)
      (set! end (start-of-line (forward-line count))))
     (t
      (set! end start)
      (set! start (start-of-line (forward-line count)))))
    (kill-area start end)))

(defun kill-whole-line (count)
  "Kill the whole of the current line."
  (interactive "p")
  (kill-area (start-of-line) (start-of-line (forward-line count))))

(defun backward-kill-line ()
  "Kill from the cursor to the start of the line."
  (interactive)
  (kill-area (if (zero? (pos-col (cursor-pos)))
		 (forward-char -1)
	       (start-of-line))
	     (cursor-pos)))

(defun goto-line (line)
  "Goto line number LINE. LINE counts from 1."
  (interactive "NLine: ")
  (set-auto-mark)
  (goto (pos nil (1- line))))

(defun goto-view-line (arg)
  "Move the cursor to column zero of the line in the centre of the currently
displayed view. If ARG is positive, the ARG'th row from the top of the view
will be found, if ARG is negative rows from the bottom of the view are
counted."
  (interactive "P")
  (goto (display-to-char-pos (pos 0 (if (null? arg)
					(quotient (cdr (view-dimensions)) 2)
				      (let
					  ((line (prefix-numeric-argument
						  arg)))
					(if (< line 0)
					    (+ (cdr (view-dimensions)) line)
					  line)))))))


;; Words

(defun forward-word (#!optional number p)
  "Return the position of first character after the end of this word.
NUMBER is the number of words to move, negative values mean go backwards.
If MOVE is t then the cursor is moved to the result."
  (interactive "@p")
  (unless number
    (set! number 1))
  (unless p (set! p (cursor-pos)))
  (cond
    ((< number 0)
      ;; go backwards
      (while (/= number 0)
	(set! p (forward-char -1 p))
	(when (looking-at word-not-regexp p)
	  ;; not in word
	  (set! p (or (re-search-backward word-regexp p)
		      (start-of-buffer))))
	;; in middle of word
	(set! p (or (re-search-backward word-not-regexp p)
		    (start-of-buffer)))
	(set! p (re-search-forward word-regexp p))
	(set! number (1+ number))))
    (t
      ;; forwards
      (while (/= number 0)
	(when (looking-at word-not-regexp p)
	  ;; already at end of a word
	  (set! p (or (re-search-forward word-regexp p)
		      (end-of-buffer))))
	(set! p (or (re-search-forward word-not-regexp p)
		    (end-of-buffer)))
	(set! number (1- number)))))
  p)

(defun backward-word (#!optional number p)
  "Basically `(forward-word -NUMBER POS)'"
  (interactive "@p")
  (forward-word (if number (- number) -1) p))

(defun kill-word (count)
  "Kills from the cursor to the end of the word."
  (interactive "p")
  (kill-area (cursor-pos) (forward-word count)))

(defun backward-kill-word (count)
  "Kills from the start of the word to the cursor."
  (interactive "p")
  (kill-area (forward-word (- count)) (cursor-pos)))

(defun word-start (#!optional p)
  "Returns the position of the start of *this* word."
  (interactive "@")
  (when (looking-at word-regexp p)
    (if (re-search-backward word-not-regexp p)
	(re-search-forward word-regexp (match-end))
      (re-search-forward word-regexp (start-of-buffer)))))

(defun in-word-p (#!optional p)
  "Returns t if POS is inside a word."
  (when (looking-at word-regexp p)
    t))

(defun mark-word (count #!optional p)
  "Marks COUNT words from POS."
  (interactive "p")
  (set-rect-blocks nil nil)
  (mark-block (or p (cursor-pos)) (forward-word count p)))

(defun transpose-words (count)
  "Move the word at (before) the cursor COUNT words forwards."
  (interactive "p")
  (transpose-items forward-word backward-word count))


;; Paragraphs

(defun forward-paragraph (count #!optional p)
  "Return the end of the COUNT'th paragraph. If the function is called
interactively, the cursor is set to this position."
  (interactive "@p")
  (unless p
    (set! p (cursor-pos)))
  (let
      ((sep-or-start (concat #\( paragraph-separate #\| paragraph-start #\) )))
    ;; Positive arguments
    (while (and (> count 0)
		(< p (end-of-buffer)))
      ;; Skip any lines at POS matching the separator
      (while (and (< (pos-line p) (buffer-length))
		  (looking-at paragraph-separate p))
	(set! p (forward-line 1 p)))
      ;; Search for the next separator or start
      (if (re-search-forward sep-or-start (forward-char 1 p))
	  (progn
	    (set! count (1- count))
	    (set! p (if (zero? count) (match-start) (match-end))))
	(set! p (end-of-buffer))
	(set! count 0)))
    ;; Negative arguments
    (while (and (< count 0)
		(> p (start-of-buffer)))
      ;; Skip any lines before POS matching the separator
      (while (and (< (pos-line p) (buffer-length))
		  (looking-at paragraph-separate (forward-line -1 p)))
	(set! p (forward-line -1 p)))
      ;; Search for the previous paragraph
      (if (re-search-backward sep-or-start (backward-char 1 p))
	  (let*
	      ((start (match-start))
	       (end (match-end))
	       (separator-match (looking-at paragraph-separate start)))
	    (set! count (1+ count))
	    (set! p (if separator-match end start)))
	(set! p (start-of-buffer))
	(set! count 0))))
  p)

(defun backward-paragraph (count #!optional p)
  "Returns the start of the COUNT'th previous paragraph. If the function is
called interactively, the cursor is set to this position."
  (interactive "@p")
  (forward-paragraph (- (or count 1)) p))

(defun paragraph-edges (count #!optional p mark)
  "Return (START . END), the positions defining the outermost characters of
the COUNT paragraphs around POS. Positive COUNTs mean to search forwards,
negative to search backwards.
When MARK is t, the block marks are set to START and END."
  (interactive "p\n\nt")
  (unless p
    (set! p (cursor-pos)))
  (let
      (start end)
    (if (> count 0)
	(progn
	  (set! start (forward-paragraph -1 (min (forward-char 1 p)
						 (end-of-buffer))))
	  (set! end (forward-paragraph count start)))
      (set! end (forward-paragraph 1 (max (forward-char -1 p)
					  (start-of-buffer))))
      (set! start (forward-paragraph count end)))
    (when mark
      (set-rect-blocks nil nil)
      (mark-block start end))
    (cons start end)))

(defun transpose-paragraphs (count)
  "Move the paragrah at (before) the cursor COUNT paragraphs forwards."
  (interactive "p")
  (transpose-items forward-paragraph backward-paragraph count))


;; Page handling

(defun forward-page (#!optional count p)
  "Return the position COUNT pages forwards. If COUNT is negative, go
backwards. When called interactively the cursor is set to the position."
  (interactive "@p")
  (unless count
    (set! count 1))
  (if (> count 0)
      (progn
	(when (looking-at page-start p)
	  (set! p (match-end)))
	(while (and (> count 0) (re-search-forward page-start p))
	  (set! p (match-end))
	  (set! count (1- count)))
	(when (= count 1)
	  (set! p (end-of-buffer))
	  (set! count 0)))
    (when (looking-at page-start (start-of-line p))
      (set! p (forward-line -1 p)))
    (while (and (< count 0) (re-search-backward page-start p))
      (set! p (if (= count -1)
		    (match-end)
		  (forward-char -1 (match-start))))
      (set! count (1+ count)))
    (when (= count -1)
      (set! p (start-of-buffer))
      (set! count 0)))
  p)

(defun backward-page (#!optional count p)
  "Basically (forward-page (- COUNT) POS)."
  (interactive "@p")
  (forward-page (- (or count 1)) p))

(defun mark-page ()
  "Set the block to mark the current page of text."
  (interactive)
  (let*
      ((end (forward-page 1))
       (start (backward-page 1 end)))
    (set-rect-blocks nil nil)
    (mark-block start end)))

(defun restrict-to-page ()
  "Restrict the buffer to the current page of text."
  (interactive)
  (let*
      ((end (forward-page 1))
       (start (backward-page 1 end)))
    (restrict-buffer start end)))


;; Block handling

(defun copy-block ()
  "If a block is marked in the current window, return the text it contains and
unmark the block."
  (when (blockp)
    (prog1 ((if (rect-blocks-p) copy-rectangle copy-area)
	    (block-start) (block-end))
      (block-kill))))

(defun cut-block ()
  "Similar to `copy-block' except the block is cut (copied then deleted) from
the buffer."
  (when (blockp)
    (prog1 ((if (rect-blocks-p) cut-rectangle cut-area)
	    (block-start) (block-end))
      (block-kill))))

(defun delete-block ()
  "Deletes the block marked in the current window (if one exists)."
  (interactive)
  (when (blockp)
    ((if (rect-blocks-p) delete-rectangle delete-area)
     (block-start) (block-end))
    (block-kill)))

(defun toggle-rect-blocks ()
  "Toggles the state of the flag saying whether blocks in this window are
marked sequentially (the default) or as rectangles."
  (interactive)
  (set-rect-blocks nil (not (rect-blocks-p))))

(defun kill-block ()
  "Kills the block marked in this window."
  (interactive)
  (kill-string (cut-block)))

(defun copy-block-as-kill ()
  "Kills the block marked in this window but doesn't actually delete it from
the buffer."
  (interactive)
  (kill-string (copy-block)))

(defun mark-block (start end)
  "Mark a block from START to END."
  (block-kill)
  (block-start start)
  (block-end end))

(defun mark-whole-buffer ()
  "Mark a block containing the whole of the buffer."
  (interactive)
  (set-rect-blocks nil nil)
  (mark-block (start-of-buffer) (end-of-buffer)))


(defun upcase-area (start end #!optional buffer)
  "Makes all alpha characters in the specified region of text upper-case."
  (interactive "-m\nM")
  (translate-area start end upcase-table buffer))

(defun downcase-area (start end #!optional buffer)
  "Makes all alpha characters in the specified region of text lower-case."
  (interactive "-m\nM")
  (translate-area start end downcase-table buffer))

(defun upcase-word (count)
  "Makes the next COUNT words from the cursor upper-case."
  (interactive "p")
  (let
      ((p (forward-word count)))
    (upcase-area (cursor-pos) p)
    (goto p)))

(defun capitalize-word (count)
  "The first character of the COUNT'th next word is made upper-case, the
rest lower-case."
  (interactive "p")
  (goto (forward-word (if (> count 0) (- count 1) count) nil))
  (unless (in-word-p)
    (goto (re-search-forward word-regexp)))
  (translate-area (cursor-pos) (forward-char) upcase-table)
  (goto (forward-char))
  (when (in-word-p)
    (downcase-word 1)))

(defun downcase-word (count)
  "Makes the word under the cursor lower case."
  (interactive "p")
  (let
      ((p (forward-word count)))
    (downcase-area (cursor-pos) p)
    (goto p)))


(defun mark-region ()
  "Sets the block-marks to the area between the cursor position and the
auto-mark"
  (interactive)
  (block-kill)
  (when (eq? (mark-file auto-mark) (current-buffer))
    (let
	((curs (cursor-pos)))
      (cond
       ((> curs (mark-pos auto-mark))
	(mark-block (mark-pos auto-mark) curs))
       (t
	(mark-block curs (mark-pos auto-mark)))))))


;; Killing

(defvar kill-ring (make-ring kill-ring-size)
  "The ring buffer containing killed strings.")

;; Cursor position at last kill
(defvar kill-last-cursor nil)

(defun kill-string (string)
  "Adds STRING to the kill storage. If the last command also kill'ed something
the string is added to."
  (when (and (string? string) (not (string=? string "")))
    (if (eq? last-command 'kill)
	(set-ring-head kill-ring (if (>= (cursor-pos) kill-last-cursor)
				     (concat (killed-string) string)
				   (concat string (killed-string))))
      (add-to-ring kill-ring string))
    (call-hook 'after-kill-hook))
  ;; this command did some killing
  (set! this-command 'kill)
  (set! kill-last-cursor (cursor-pos))
  string)

(defun killed-string (#!optional depth)
  "Returns the string in the kill-buffer at position DEPTH."
  (get-from-ring kill-ring (1+ (or depth 0))))

(defun kill-area (start end)
  "Kills a region of text in the current buffer from START to END."
  (interactive "-m\nM")
  (kill-string (cut-area start end)))

(defun copy-area-as-kill (start end)
  "Copies a region of text in the current buffer (from START to END) to the
kill storage."
  (interactive "-m\nM")
  (kill-string (copy-area start end)))


;; Yank

;; Last item in the kill-ring that was yanked
(defvar yank-last-item nil)

;; Start and end of the last yank insertion
(defvar yank-last-start nil)
(defvar yank-last-end nil)

(defun yank-get-string (#!optional no-hooks)
  ;; First call the pre-yank-hook. This is typically used by the window
  ;; system to return the current selection
  (let ((str (and (not no-hooks)
		  (call-hook 'pre-yank-hook nil 'or))))
    (if str
	(progn
	  (set! yank-last-item nil)
	  str)
      (if (zero? (ring-size kill-ring))
	  (error "Nothing to yank"))
      (set! yank-last-item 0)
      (killed-string))))

(defun yank (#!optional no-pre-yank-hooks)
  "Inserts text before the cursor. If running under X11, and a selection is
active, paste that; else yank the most recent entry in the kill-ring."
  (interactive "P")
  (set! yank-last-start (cursor-pos))
  (insert (yank-get-string no-pre-yank-hooks))
  (set! this-command 'yank))

(defun yank-block ()
  "If a block is marked in the current window, insert it before the current
cursor position, then unmark the block."
  (interactive)
  (when (blockp)
    (set! yank-last-start (cursor-pos))
    (if (rect-blocks-p)
	(insert-rectangle (copy-rectangle (block-start) (block-end)))
      (insert (copy-area (block-start) (block-end))))
    (set! yank-last-item nil)
    (set! this-command (if (rect-blocks-p) 'yank-rectangle 'yank))
    (block-kill)))

(defun yank-rectangle (no-pre-yank-hooks)
  "Similar to `yank' except that the inserted text is treated as a rectangle."
  (interactive "P")
  (set! yank-last-start (cursor-pos))
  (insert-rectangle (yank-get-string no-pre-yank-hooks))
  (set! this-command 'yank-rectangle))

(defun yank-next (count)
  "If the last command was a yank, replace the yanked text with the COUNT'th
next string in the kill ring."
  (interactive "p")
  (if (and (or (eq? last-command 'yank) (eq? last-command 'yank-rectangle))
	   (buffer-record-undo))
      (progn
	(set! yank-last-item (if yank-last-item
				 (+ yank-last-item count)
			       (1- count)))
	(when (>= yank-last-item (ring-size kill-ring))
	  (error "Nothing more to yank"))
	;; Using undo should allow rectangles to be replaced properly
	(undo)
	;; ALERT: major hack, doing this causes whatever had to be
	;; done to undo the last insertion to be lost forever. If
	;; this isn't done multiple yank-next's won't work. This
	;; isn't great, since undoing the multiple yanks doesn't
	;; work as you'd expect...
	(set-buffer-undo-list nil)
	(goto yank-last-start)
	((if (eq? last-command 'yank) insert insert-rectangle)
	 (killed-string yank-last-item))
	(set! this-command last-command))
    (error "Can't yank (last command wasn't yank, or no undo info)")))


;; Transposing

(defun transpose-items (forward-item backward-item count)
  "Transpose the areas defined by the functions FORWARD-ITEM and BACKWARD-
ITEM (in the style of `forward-word', `backward-word' etc).
COUNT is the number of items to drag the item at the cursor past.\n
What actually happens is that the item before the cursor is dragged forward
over the COUNT following items."
  (let
      (start1 start2 end1 end2)
    (while (> count 0)
      ;; go forwards
      (set! start1 (backward-item 1))
      (set! end1 (forward-item 1 start1))
      (set! end2 (forward-item 1 end1))
      (set! start2 (backward-item 1 end2))
      (transpose-1 start1 end1 start2 end2)
      (set! count (1- count)))
    (while (< count 0)
      ;; go backwards
      (set! start1 (backward-item 1))
      (set! end1 (forward-item 1 start1))
      (set! start2 (backward-item 1 start1))
      (set! end2 (forward-item 1 start2))
      (transpose-1 start1 end1 start2 end2)
      (set! count (1+ count)))))

(defun transpose-1 (start1 end1 start2 end2)
  (let
      (text1 text2)
    (if (< start2 start1)
	(progn
	  (set! text1 (cut-area start1 end1))
	  (set! text2 (copy-area start2 end2))
	  (insert text2 start1)
	  (delete-area start2 end2)
	  (goto (insert text1 start2)))
      (set! text1 (copy-area start1 end1))
      (set! text2 (cut-area start2 end2))
      (goto (insert text1 start2))
      (delete-area start1 end1)
      (insert text2 start1))))

(defun transpose-lines (count)
  "Move the line under the cursor COUNT lines forwards."
  (interactive "p")
  (transpose-items forward-line backward-line count))


(defun abort-recursive-edit (#!optional ret-val)
  "Exits the innermost recursive edit with a value of VALUE (or nil)."
  (interactive)
  (throw 'exit ret-val))

(defun top-level ()
  "Abort all recursive-edits."
  (interactive)
  (throw 'top-level nil))


;; Overwrite mode

(defvar overwrite-mode-enabled nil
  "Non-nil when overwrite-mode is enabled.")
(make-variable-buffer-local 'overwrite-mode-enabled)

(set! minor-mode-alist (cons '(overwrite-mode-enabled " Overwrite") minor-mode-alist))

(defun overwrite-mode ()
  "Minor mode to toggle overwrite/insert."
  (interactive)
  (if overwrite-mode-enabled
      (progn
	(set! overwrite-mode-enabled nil)
	(remove-hook 'unbound-key-hook overwrite-insert))
    (set! overwrite-mode-enabled t)
    (add-hook 'unbound-key-hook overwrite-insert)))

(defun overwrite-insert (#!optional str)
  (unless str
    (set! str (current-event-string)))
  (when str
    (let
	((len (string-length str)))
      (delete-area (cursor-pos) (forward-char len))
      (insert str))))


;; Miscellaneous editing commands

(defun quoted-insert (count)
  "Read the next event; if it's a digit, read two more digits then insert the
character whose octal value has been entered, otherwise simply insert the
event as read. COUNT copies of the same character are inserted."
  (interactive "p")
  (let
      ((first (next-event t)))
    (if (char-numeric? (string-ref first 0))
	;; Read two more digits
	(let*
	    ((second (next-event t))
	     (third (next-event t)))
	  (set! first (make-string count (+ (ash (- (string-ref first 0) #\0) 6)
					    (ash (- (string-ref second 0) #\0) 3)
					    (- (string-ref third 0) #\0))))
	  (or (< (string-ref first 0) 256)
	      (error "Character overflow")))
      (if (/= count 1)
	  (set! first (make-string count (string-ref first 0)))))
    (insert first)))

(defun backspace-char (count)
  "Delete COUNT characters preceding the cursor, if the cursor is past the
end of the line simply move COUNT characters to the left."
  (interactive "p")
  (let
      ((start (forward-char (- count))))
    (if (> (cursor-pos) (end-of-line))
	(if (> start (end-of-line))
	    (goto start)
	  (goto (end-of-line))
	  (delete-area start (cursor-pos)))
      (delete-area start (cursor-pos)))))
  
(defun delete-char (count)
  "Delete the character under the cursor."
  (interactive "p")
  (delete-area (cursor-pos) (forward-char count)))

(defun tab-with-spaces ()
  "Insert enough spaces before the cursor to move it to the next tab position."
  (interactive)
  (%indent-to (pos-col (forward-tab)) t))

(defun just-spaces (count)
  "Ensure that there are only COUNT spaces around the cursor."
  (interactive "p")
  (when (member (get-char) '(#\space #\tab))
    (let
	((p (re-search-backward "[^\t ]|^")))
      (when p
	(unless (zero? (pos-col p))
	  (set! p (forward-char 1 p)))
	(when (and p (looking-at "[\t ]+" p))
	  (delete-area (match-start) (match-end))
	  (goto (match-start))))))
  (unless (zero? count)
    (insert (make-string count #\space))))

(defun no-spaces ()
  "Delete all space and tab characters surrounding the cursor."
  (interactive)
  (just-spaces 0))

(defun open-line (count)
  "Break the current line creating COUNT new lines, leaving the cursor in
its original position."
  (interactive "p")
  (let
      ((opos (cursor-pos)))
    (insert (make-string count #\newline))
    (goto opos)))

(defun delete-blank-lines ()
  "Delete all but the bottom-most of the blank lines surrounding the cursor.
If the cursor isn't actually on a series of blank lines, the next series
is found and deleted. If the cursor is on a single blank line, the line is
deleted."
  (interactive)
  (unless (empty-line-p)
    (if (re-search-forward "^[\t ]*$")
	(goto (match-end))
      (error "End of buffer")))
  (let
      ((start (or (and (re-search-backward "^.*[^\t\n ].*\n")
		       (match-end))
		  (start-of-buffer)))
       (end (or (re-search-forward "^[\t ]*\n.*[^\t\n ].*$") (end-of-buffer))))
    (delete-area start (if (equal? start end)
			   (forward-line 1 end)
			 end))))

(defun toggle-buffer-read-only ()
  "Toggle the current buffer between being writable and read-only."
  (interactive)
  (if toggle-read-only-function
      (toggle-read-only-function)
    (set-buffer-read-only nil (not (buffer-read-only-p)))))

(defun top-of-buffer ()
  "Return the position of the first line in the buffer, leaving the column
the same."
  (interactive "!@")
  (pos nil (pos-line (start-of-buffer))))

(defun bottom-of-buffer ()
  "Return the position of the last line in the buffer, leaving the column
the same."
  (interactive "!@")
  (pos nil (pos-line (end-of-buffer))))

(defun blinking-insert (#!optional char)
  (interactive "E")
  (when char
    (insert (if (integer? char)
		(make-string 1 char)
	      char)))
  (when (and blink-matching-paren (or (not (list? blink-matching-paren))
				      (memq (get-char (forward-char -1))
					    blink-matching-paren)))
    (condition-case nil
	(let
	    ((match (find-matching-bracket (forward-char -1))))
	  (when match
	    (if (char-to-display-pos match)
		(save-excursion
		  (goto match)
		  (sit-for blink-matching-delay))
	      (message (format nil "Matches: %S"
			       (copy-area (start-of-line match)
					  (end-of-line match)))))))
      (error
       (message "[No matching bracket]")))))

(defun set-indent-pos (pos)
  "Sets the indentation of the line pointed to by POS to the column pointed
to by POS by putting the optimal sequence of TAB and SPC characters at the
start of the line. If `indent-tabs-mode' is nil, only SPC characters will
be inserted."
  (%set-indent-pos pos nil (not indent-tabs-mode)))

(defun indent-to (column)
  "Inserts enough TAB and SPC characters to move the cursor to glyph column
COLUMN. If `indent-tabs-mode' is nil only SPC characters are used. COLUMN
counts from zero."
  (%indent-to column (not indent-tabs-mode)))

(defun indent-to-next-tab ()
  "Calls (indent-to COL) to move to the next TAB stop."
  (interactive)
  (let ((col (pos-col (char-to-glyph-pos (cursor-pos))))
	(tab-width (tab-size)))
    (indent-to (* (1+ (quotient col tab-width)) tab-width))))


;; Some macros

(defmacro save-restriction (#!rest forms)
  "Evaluate FORMS, restoring the original buffer restriction when they
finish (as long as the original buffer still exists)."
  (let
      ((start (gensym))
       (end (gensym)))
    `(let
	 (,start ,end)
       (when (buffer-restricted-p)
	 (set! ,start (make-mark (restriction-start)))
	 (set! ,end (make-mark (restriction-end))))
       (unwind-protect
	   (progn ,@forms)
	 (if (and ,start (mark-resident-p ,start))
	     (restrict-buffer (mark-pos ,start)
			      (mark-pos ,end)
			      (mark-file ,start))
	   (unrestrict-buffer))))))
      
(defmacro save-excursion (#!rest forms)
  "Evaluate FORMS, ensuring that the original current buffer and the original
position in this buffer are restored afterwards, even in case of a non-local
exit occurring (as long as the original buffer wasn't killed)."
  (let
      ((mark (gensym)))
    `(let
	 ((,mark (make-mark)))
       (unwind-protect
	   (progn ,@forms)
	 (when (mark-resident-p ,mark)
	   (goto-mark ,mark t))))))

(defmacro map-extents-at (function #!optional position buffer)
  "Call (FUNCTION EXTENT) for all extents containing location POSITION in
BUFFER (defaulting to the current position in the current buffer), working
from the innermost outwards."
  (let
      ((e (gensym)))
    `(let
	 ((,e (get-extent ,position ,buffer)))
       (while ,e
	 (,function ,e)
	 (set! ,e (extent-parent ,e))))))


;; Mouse dragging etc

(defvar mouse-select-pos nil)
(defvar mouse-dragging nil)

(defun mouse-pos ()
  "Return the position of the character underneath the mouse pointer in
the current view. Returns nil if no such character can be found."
  (interactive "@")
  (let ((p (raw-mouse-pos)))
    (when p
      (set! p (translate-pos-to-view p)))
    (and (posp p) (display-to-char-pos p))))

(defun mouse-view-pos ()
  "Return (VIEW . POS) defining the character position of the mouse, or nil."
  (let* ((p (raw-mouse-pos))
	 (mouse-view (find-view-by-pos p)))
    (when mouse-view
      (set! p (translate-pos-to-view p mouse-view))
      (when (posp p)
	(cons mouse-view (display-to-char-pos p mouse-view))))))

(defun goto-mouse ()
  "Move the cursor to the view and position under the mouse pointer, returns
the position, or nil if no position, or t if in the status line of the
current view."
  (interactive)
  (let* ((raw-pos (raw-mouse-pos))
	 (mouse-view (find-view-by-pos raw-pos)))
    (when mouse-view
      (unless (eq? (current-view) mouse-view)
	(set-current-view mouse-view))
      (set! raw-pos (translate-pos-to-view raw-pos))
      (if (posp raw-pos)
	  (goto (display-to-char-pos raw-pos))
	raw-pos))))
 
(defun mouse-select ()
  (interactive)
  (let
      ((p (goto-mouse)))
    (if (eq? p t)
	(progn
	  (set! mouse-select-pos 'status)
	  (set! mouse-dragging 'view))
      (when p
	(set! mouse-select-pos p)
	(set! mouse-dragging nil)
	(when (and (blockp) (>= p (block-start)) (< p (block-end)))
	  (block-kill))))))

(defun mouse-double-select ()
  (interactive)
  (unless mouse-dragging
    (set! mouse-dragging 'words)))

(defun mouse-select-drag (drag-rectangle)
  (if (eq? mouse-dragging 'view)
      ;; Resize the current view
      (let
	  ((new-height (- (pos-line (raw-mouse-pos))
			  (pos-line (view-position)))))
	(set-view-dimensions nil nil new-height))
    ;; Mark a block
    (let
	((p (mouse-pos)))
      (when (posp p)
	(when (eq? mouse-dragging 'words)
	  (set! p (forward-word (if (> p mouse-select-pos) 1 -1) p)))
	(goto p)
	(if (equal? p mouse-select-pos)
	    (when (and (blockp) (>= p (block-start)) (< p (block-end)))
	      (block-kill))
	  (set! mouse-dragging (or mouse-dragging t))
	  (block-kill)
	  (set-rect-blocks nil drag-rectangle)
	  (block-start (if (eq? mouse-dragging 'words)
			   (if (>= p mouse-select-pos)
			       (or (word-start mouse-select-pos)
				   mouse-select-pos)
			     (if (in-word-p mouse-select-pos)
				 (forward-word 1 mouse-select-pos)
			       mouse-select-pos))
			 mouse-select-pos))
	  (block-end p))))))

(defun mouse-select-drag-block ()
  (interactive)
  (mouse-select-drag nil))

(defun mouse-select-drag-rect ()
  (interactive)
  (mouse-select-drag t))

(defun yank-to-mouse ()
  "Yanks to the position under the mouse cursor. The cursor is left at the
end of the inserted text."
  (interactive)
  (unless mouse-yank-at-point
    (goto-mouse))
  (yank))
