;;;; tags.jl -- Basic tags file support
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

(provide 'tags)


;; Variables

(defvar tags-file-name nil
  "File name of current TAGS table, or nil.")

(defvar tags-last-found nil
  "String defining the last tag moved to, or nil.")

(defvar tags-last-found-pos nil
  "Position in tags table of tags-last-found, or nil.")

(defvar tags-marks nil
  "List of locations from which tags were found (as marks).")

(defvar tags-fold-case t
  "When t, case isn't important when searching tags tables.")

;; If non-nil, (FUNCTION . ARGS) that will restart the previous tags-search
;; or tags-query-replace command
(defvar tags-continue-command nil)

(defvar tags-history-length 16)

;; list of all files referenced in this TAGS file, or nil
(defvar tags-cached-file-list nil)
(make-variable-buffer-local 'tags-cached-file-list)


;; Code

;; Ensure that tags-file-name is set; signal an error if still not after
;; prompting the user
(defun tags-find-table ()
  (or tags-file-name
      (let
	  ((default (expand-file-name "TAGS")))
	(setq tags-last-found-pos nil)
	(setq tags-file-name (prompt-for-file "Tags table to load:" t
					      default default)))
      (error "No tags table selected")))

(defun tags-trim-history ()
  (let ((marks-end (nthcdr tags-history-length tags-marks)))
    (when marks-end
      (rplacd marks-end nil))))

;;;###autoload
(defun visit-tag-table (name)
  "Signal that all tags searches should be carried out in tags table NAME."
  (interactive "fTags table:")
  (setq tags-last-found-pos nil
	tags-file-name name))

;;;###autoload
(defun find-tag (name)
  "Search for a tag containing NAME, in the currently visited tags table. A
prefix argument greater than one says to find the next tag using the same
string that the last tag was found with; a negative prefix arg says to
move back to the previously found tag."
  (interactive
   (list (if (null? current-prefix-arg)
	     ;; Find a new tag
	     (or (prompt-for-string "Find tag:" (symbol-at-point))
		 (error "No tag specified"))
	   (if (< (prefix-numeric-argument current-prefix-arg) 0)
	       ;; Negative arg, pop last tag off stack
	       'pop
	     ;; Move to the next occurrence of the last tag
	     'push))))
  (tags-find-table)
  (tags-trim-history)			;should be after, but easier here..
  (catch 'return
    (let
	((tags-buffer (find-file tags-file-name t))
	 (original-mark (make-mark))
	 start)
      (cond
       ((eq? name 'pop)
	(pop-tag-mark)
	(throw 'return (cursor-pos)))
       ((eq? name 'push)
	(setq name tags-last-found
	      start tags-last-found-pos)))
      (while (setq start (search-forward name (or start (pos 0 0))
					 tags-buffer tags-fold-case))
	(if (or (zero? (pos-line start))
		(= #\FF (get-char (forward-char -1 (start-of-line start)
					       tags-buffer) tags-buffer)))
	    ;; We're looking at the filename, not a tag, continue searching
	    (setq start (start-of-line (forward-line 1 start)))
	  ;; Move to the tag's position. First find the file name
	  (let*
	      ((file (and (or (re-search-backward "^\f\n([^,\n]+)"
						  start tags-buffer)
			      (error "Can't find start of tags section"))
			  (expand-file-name
			   (expand-last-match "\\1")
			   (with-buffer tags-buffer *default-directory*))))
	       tag-pos tag-line tag-name)
	    ;; This switches to the new file's buffer
	    (or (find-file file)
		(error "Can't open file %S" file))
	    (unless (looking-at "^(.+)\177((.+)\001|)([0-9]+)"
				(start-of-line start) tags-buffer)
	      (error "Malformed tag line at %S" start))
	    (setq tag-pos (pos 0 (1- (string->number
				      (copy-area (match-start 4)
						 (match-end 4) tags-buffer))))
		  tag-line (copy-area (match-start 1)
				      (match-end 1) tags-buffer)
		  tag-name (if (null? (match-start 3))
			       ;; No definitive name
			       tag-line
			     (copy-area (match-start 3) (match-end 3)
					tags-buffer)))
	    ;; Check that the tag _does_actually_ occur at the position
	    ;; it's supposed to.
	    (if (buffer-compare-string tag-line tag-pos tags-fold-case)
		;; Yep, easy
		(goto tag-pos)
	      ;; No, search for it
	      (if (or (search-forward tag-line tag-pos)
		      (search-backward tag-line tag-pos))
		  (goto (match-start))
		;; Can't find it. Signal an error
		(error "Tag %S has been deleted; rerun etags")))
	    (message tag-name)
	    ;; break out of the loop
	    (setq tags-last-found name
		  tags-last-found-pos (end-of-line start tags-buffer)
		  tags-marks (cons original-mark tags-marks))
	    (throw 'return (cursor-pos)))))
      (error "Tag %S not found" name))))

(defun pop-tag-mark ()
  "Return to the buffer and position from which the last tag was found."
  (interactive)
  (if (null? tags-marks)
      (message "[No more tag marks]")
    (goto-mark (car tags-marks))
    (setq tags-marks (cdr tags-marks))))


;; Tags searching

(define (tags-file-list)
  (define (uniquify lst)
    (let (out)
      (mapc (lambda (x)
	      (unless (member x out)
		(setq out (cons x out)))) lst)
      out))
  (tags-find-table)
  (with-buffer (find-file tags-file-name t)
    (unless tags-cached-file-list
      (let ((point (start-of-buffer))
	    (files '()))
	(while (and (setq point (char-search-forward #\page point))
		    (looking-at "\f\n([^,]+)" point))
	  (setq point (match-end))
	  (setq files (cons (expand-last-match "\\1") files)))
	(setq tags-cached-file-list (sort (uniquify files)))))
    tags-cached-file-list))

;; Returns the next file in tag table (after CURRENT-FILE if defined), or nil
(defun tags-next-file (#!optional current-file)
  (tags-find-table)
  (with-buffer (find-file tags-file-name t)
    (if current-file
	(progn
	  ;; Try to make current file relative to tags table location
	  (when (string-match
		 (concat #\^ (quote-regexp
			     (file-name-as-directory
			      (canonical-file-name *default-directory*)))
			 "(.*)$")
		 (canonical-file-name current-file))
	    (setq current-file (expand-last-match "\\1")))
	  (cadr (member current-file (tags-file-list))))
      (car (tags-file-list)))))

;; Map (FUNCTION BUFFER POINT) over all files in the current tags-table.
;; MARK provides an optional start point
(defun tags-map-buffers (function #!optional mark)
  (let
      (file buffer kill-this-buffer point)
    (when mark
      (if (mark-resident-p mark)
	  (progn
	    (setq buffer (mark-file mark))
	    (setq file (buffer-file-name buffer)))
	(setq file (mark-file mark)))
      (setq point (mark-pos mark)))
    (unless file
      (setq file (tags-next-file)))
    (while file
      (let ((real-file (expand-file-name
			file (file-name-directory tags-file-name))))
	(unless buffer
	  (setq buffer (get-file-buffer real-file))
	  (if buffer
	      (setq kill-this-buffer nil)
	    (setq buffer (find-file real-file t))
	    (setq kill-this-buffer t)))
	(unless point
	  (setq point (start-of-buffer buffer)))
	(function buffer point)
	(when (and kill-this-buffer (not (buffer-modified-p buffer)))
	  (kill-buffer buffer))
	(setq buffer nil)
	(setq point nil)
	(setq file (tags-next-file file))))))

;;;###autoload
(defun tags-search (regexp #!optional mark)
  "Search through all files in the current tags table for an occurrence of the
regular expression REGEXP. Further matches may be found though the use of the
`tags-loop-continue' function."
  (interactive "sRegexp:")
  (require 'isearch)			;for case-fold-search
  (setq tags-continue-command nil)
  (catch 'out
    (tags-map-buffers
     (lambda (buffer point)
       (message (format nil "Searching `%s'..." (buffer-file-name buffer)) t)
       (when (re-search-forward regexp point buffer case-fold-search)
	 (goto-buffer buffer)
	 (goto (match-start))
	 (setq tags-continue-command
	       `(,tags-search ,regexp ,(make-mark (match-end))))
	 (throw 'out t)))
     mark)
    (message "[No matches]")))

;;;###autoload
(defun tags-query-replace (from to #!optional mark)
  "Query replace through all files in the current tags table for an occurrence
of the regular expression FROM, possibly replacing it with TO. Further matches
may be found though the use of the `tags-loop-continue' function."
  (interactive "sQuery replace regexp: \nsQuery replace `%s' with: ")
  (require 'replace)
  (setq tags-continue-command nil)
  (catch 'out
    (let
	((do-all nil))
      (tags-map-buffers
       (lambda (buffer point)
	 (message
	  (format nil "Searching `%s'..." (buffer-file-name buffer)) t)
	 (when (re-search-forward from point buffer case-fold-search)
	   (with-buffer buffer
	     (goto (match-start))
	     (if do-all
		 (while (re-search-forward from nil nil case-fold-search)
		   (goto (replace-last-match to)))
	       (setq point (query-replace from to))
	       (cond ((eq? point 'rest)
		      (setq do-all t))
		     ((null? point)
		      (setq tags-continue-command
			    `(,tags-query-replace ,from ,to ,(make-mark)))
		      (throw 'out t)))))))
       mark))))

(defun tags-loop-continue ()
  "Continue the most recent `tags-search' or `tags-query-replace' command, from
the position where it left of."
  (interactive)
  (if tags-continue-command
      (apply (car tags-continue-command) (cdr tags-continue-command))
    (error "No tags-loop to continue")))
