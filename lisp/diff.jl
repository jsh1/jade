;;;; diff.jl -- Visual interpretation of diff(1) output
;;;  Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
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

;; TODO:
;;
;; + Add support for more of the countless diff(1) output formats
;;   (most important: context and unified diffs)
;;
;; + The equation is `FILE1 + PATCH = FILE2'. Currently we work from
;;   the PATCH being the unknown. But if we had the PATCH plus either
;;   of the other two files the sole unknown item can be deduced.
;;   So if I had a file and a patch from somewhere, I want to be able
;;   see the effect of the patch with the two files side-by-side

(require 'prompt)			;for prompt-title-face
(provide 'diff)


;; Configuration

(defvar diff-program "diff"
  "Program used to compare two files.")

(defface diff-added-face
  "Face used to display added text in diff listings."
  (set-face-attribute diff-added-face 'background "lightgreen"))

(defface diff-deleted-face
  "Face used to display removed text in diff listings."
  (set-face-attribute diff-deleted-face 'background "pink"))

(defface diff-changed-face
  "Face used to display text that has been modified in diff listings."
  (set-face-attribute diff-changed-face 'background "lightgoldenrod"))

(defface diff-marker-face
  "Face used to show where an insertion or deletion occurs in diff listings."
  (set-face-attribute diff-marker-face 'background "#eeeee1"))

(defvar diff-keymap (bind-keys (make-sparse-keymap)
		      "SPC" 'diff-next
		      "n" 'diff-next
		      "BS" 'diff-previous
		      "p" 'diff-previous
		      "C-g" 'diff-quit
		      "q" 'diff-quit
		      "ESC" 'diff-quit))


;; Dynamic variables, bound whilst in the diff-display function

;; Buffers of the source and destination files, and the buffer containing
;; the diff output
(defvar diff-src-buffer nil)
(defvar diff-dest-buffer nil)
(defvar diff-diff-buffer nil)

;; Extents in the src and dest buffer or nil
(defvar diff-src-extent nil)
(defvar diff-dest-extent nil)

;; The position in the diff output of the current hunk
(defvar diff-hunk-pos nil)


;; Entry points

;;;###autoload
(defun diff (file1 file2)
  "Compare two files, FILE1 and FILE2, then display the differences
interactively. When called interactively FILE1 and FILE2 are prompted for.
While comparing files the following commands are available:\n
\\{diff-keymap}"
  (interactive "fOld file:\nfNew file:")
  (let
      ((diff-output (diff-compare-files file1 file2)))
    (when diff-output
      (let
	  ((buffer1 (find-file file1 t))
	   (buffer2 (find-file file2 t)))
	(diff-display buffer1 buffer2 diff-output)))))

;;;###autoload
(defun diff-backup (file)
  "Compare FILE with its backup file. See the `diff' command for more details."
  (interactive (list (prompt-for-file "File to diff with its backup:"
				      t (buffer-file-name))))
  (let
      ((name (concat file ?~)))
    (diff name file)
    (let
	((buf (get-file-buffer name)))
      (and buf (kill-buffer buf)))))

;;;###autoload
(defun diff-auto-save (file)
  "Compare FILE with its auto-saved version. See the `diff' command for more
details."
  (interactive (list (prompt-for-file "File to diff with its auto-save:"
				      t (buffer-file-name))))
  (let
      ((name (make-auto-save-name file)))
    (diff file name)
    (let
	((buf (get-file-buffer name)))
      (and buf (kill-buffer buf)))))

;;;###autoload
(defun diff-buffers (buffer1 buffer2)
  "Interactively display the differences between BUFFER1 and BUFFER2. When
called interactively the buffers will be prompted for. See the `diff'
command for more details."
  (interactive "bOld buffer:\nbNew buffer:")
  (let
      ((file1 (make-temp-name))
       (file2 (make-temp-name)))
    (unwind-protect
	(when (and (with-buffer buffer1
		     (write-buffer-contents file1))
		   (with-buffer buffer2
		     (write-buffer-contents file2)))
	  (let
	      ((diff-output (diff-compare-files file1 file2)))
	    (when diff-output
	      (diff-display buffer1 buffer2 diff-output))))
      (and (file-exists-p file1)
	   (delete-file file1))
      (and (file-exists-p file2)
	   (delete-file file2)))))


;; Low-level code

;; Return the buffer containing the output, or nil if no difference
(defun diff-compare-files (file1 file2)
  (let*
      ((process (make-process (make-buffer "*diff-output*")))
       exit-value)
    (call-process process nil diff-program
		  (local-file-name file1) (local-file-name file2))
    (setq exit-value (process-exit-value process))
    (cond
     ((or (null exit-value) (= exit-value 2))
      (error "Diff command failed"))
     ((= exit-value 0)
      (message "[No differences]")
      nil)
     (t
      (process-output-stream process)))))

(defun diff-configure-views (src-range dst-range)
  (when (> (window-view-count) 3)
    (mapc #'(lambda (v)
	      (unless (minibuffer-view-p v)
		(delete-view v)))
	  (nthcdr 2 (window-view-list))))
  (when (<= (window-view-count) 2)
    (split-view (car (window-view-list))))
  (with-view (car (window-view-list))
    (goto-buffer diff-src-buffer)
    (goto (forward-line (max 0 (car src-range)) (start-of-buffer)))
    (center-display nil (max 1 (quotient (- (cdr (view-dimensions))
					    (- (cdr src-range)
					       (car src-range))) 2))))
  (with-view (nth 1 (window-view-list))
    (goto-buffer diff-dest-buffer)
    (goto (forward-line (max 0 (car dst-range)) (start-of-buffer)))
    (center-display nil (max 1 (quotient (- (cdr (view-dimensions))
					    (- (cdr dst-range)
					       (car dst-range))) 2)))))

;;;###autoload
(defun diff-display (diff-src-buffer diff-dest-buffer diff-diff-buffer)
  (let
      ((diff-hunk-pos (start-of-buffer diff-diff-buffer))
       (diff-src-extent nil)
       (diff-dest-extent nil)
       (minibuf (make-buffer "*diff*"))
       (esc-means-meta nil))
    (with-view (minibuffer-view)
      (with-buffer minibuf
	(setq local-keymap diff-keymap)
	(make-extent (start-of-buffer)
		     (insert "Visual diff: (n, p, q) ")
		     (list 'face prompt-title-face))
	(setq read-only t)
	(diff-display-hunk)
	(unwind-protect
	    (catch 'diff-exit
	      (recursive-edit))
	  (when diff-src-extent
	    (delete-extent diff-src-extent))
	  (when diff-dest-extent
	    (delete-extent diff-dest-extent)))))))

(defun diff-parse-range (p cell)
  (let
      (start end)
    (unless (looking-at "([0-9]+)(,([0-9]+))?" p diff-diff-buffer)
      (error "Malformed diff output!"))
    (setq start (1- (string->number (expand-last-match "\\1"))))
    (if (match-start 2)
	(setq end (1- (string->number (expand-last-match "\\3"))))
      (setq end start))
    (setcar cell start)
    (setcdr cell end)
    (match-end)))

(defun diff-display-hunk ()
  (when diff-src-extent
    (delete-extent diff-src-extent))
  (when diff-dest-extent
    (delete-extent diff-dest-extent))
  (let
      ((left-range (cons))
       (right-range (cons))
       (tem diff-hunk-pos)
       command)
    (setq tem (diff-parse-range diff-hunk-pos left-range))
    (setq command (get-char tem diff-diff-buffer))
    (diff-parse-range (forward-char 1 tem diff-diff-buffer) right-range)
    (cond
     ((= command ?a)
      (setq command 'add))
     ((= command ?c)
      (setq command 'change))
     ((= command ?d)
      (setq command 'delete))
     (t
      (error "Unknown command in diff output")))
    (with-buffer diff-src-buffer
      (setq diff-src-extent (make-extent
			     (if (eq command 'add)
				 (if (>= (car left-range) 0)
				     (end-of-line (forward-line
						   (car left-range)
						   (start-of-buffer)))
				   (start-of-buffer))
			       (forward-line (car left-range)
					     (start-of-buffer)))
			     (forward-line (1+ (cdr left-range))
					   (start-of-buffer))
			     (list 'face (cond
					  ((eq command 'change)
					   diff-changed-face)
					  ((eq command 'delete)
					   diff-deleted-face)
					  ((eq command 'add)
					   diff-marker-face))))))
    (with-buffer diff-dest-buffer
      (setq diff-dest-extent (make-extent
			      (if (eq command 'delete)
				  (if (>= (car right-range) 0)
				     (end-of-line (forward-line
						   (car right-range)
						   (start-of-buffer)))
				   (start-of-buffer))
				(forward-line (car right-range)
					      (start-of-buffer)))
			      (forward-line (1+ (cdr right-range))
					    (start-of-buffer))
			      (list 'face (cond
					   ((eq command 'add)
					    diff-added-face)
					   ((eq command 'change)
					    diff-changed-face)
					   ((eq command 'delete)
					    diff-marker-face))))))
    (diff-configure-views left-range right-range)))

(defun diff-quit ()
  "Quit the diff session."
  (interactive)
  (throw 'diff-exit t))

(defun diff-next (count)
  "Display the COUNT'th next hunk of changes in the diff output."
  (interactive "p")
  (while (> count 0)
    (or (re-search-forward
	 "^[0-9]+" (forward-line 1 diff-hunk-pos) diff-diff-buffer)
	(error "End of diff output"))
    (setq diff-hunk-pos (match-start))
    (setq count (1- count)))
  (while (< count 0)
    (or (re-search-backward
	 "^[0-9]+" (forward-line -1 diff-hunk-pos) diff-diff-buffer)
	(error "Start of diff output"))
    (setq diff-hunk-pos (match-start))
    (setq count (1+ count)))
  (diff-display-hunk))

(defun diff-previous (count)
  "Display the COUNT'th previous hunk of changes in the diff output."
  (interactive "p")
  (diff-next (- count)))
