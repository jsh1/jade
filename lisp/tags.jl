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

(defvar tags-history nil
  "List of previously found tags (as marks).")

(defvar tags-fold-case t
  "When t, case isn't important when searching tags tables.")


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
   (list (let
	     ((arg (prefix-numeric-argument current-prefix-arg)))
	   (cond
	    ((< arg 0)
	     ;; Negative arg, pop last tag off stack
	     'pop)
	    ((> arg 1)
	     ;; Move to the next occurrence of the last tag
	     'push)
	    (t
	     ;; Find a new tag
	     (or (prompt-for-string "Find tag:")
		 (error "No tag specified")))))))
  (tags-find-table)
  (catch 'return
    (let
	((tags-buffer (find-file tags-file-name t))
	 start)
      (cond
       ((eq name 'pop)
	(while (and tags-history
		    (eq (mark-file (car tags-history)) (current-buffer))
		    (equal (mark-pos (car tags-history)) (cursor-pos)))
	  ;; Lose the top mark, we're already there
	  (setq tags-history (cdr tags-history)))
	(if tags-history
	    (progn
	      (goto-mark (car tags-history))
	      (setq tags-history (cdr tags-history)))
	  (error "No previous tag"))
	(throw 'return (cursor-pos)))
       ((eq name 'push)
	(setq name tags-last-found
	      start tags-last-found-pos)))
      (while (setq start (search-forward name (or start (pos 0 0))
					 tags-buffer tags-fold-case))
	(if (or (zerop (pos-line start))
		(= ?\f (get-char (forward-char -1 (start-of-line start)
					       tags-buffer) tags-buffer)))
	    ;; We're looking at the filename, not a tag, continue searching
	    (setq start (start-of-line (forward-line 1 start)))
	  ;; Move to the tag's position. First find the file name
	  (let*
	      ((file (and (or (re-search-backward "^\f\n([^,\n]+)"
						  start tags-buffer)
			      (error "Can't find start of tags section"))
			  (expand-file-name
			   (file-name-nondirectory (expand-last-match "\\1"))
			   (with-buffer tags-buffer default-directory))))
	       ;; This also switches to buffer
	       (buffer (or (find-file file)
			   (error "Can't open file %S" file)))
	       tag-pos tag-line tag-name)
	    (unless (looking-at "^(.+)\177((.+)\001|)([0-9]+)"
				(start-of-line start) tags-buffer)
	      (error "Malformed tag line at %S" start))
	    (setq tag-pos (pos 0 (1- (read-from-string
				      (copy-area (match-start 4)
						 (match-end 4) tags-buffer))))
		  tag-line (copy-area (match-start 1)
				      (match-end 1) tags-buffer)
		  tag-name (if (null (match-start 3))
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
		  tags-history (cons (make-mark) tags-history))
	    (throw 'return (cursor-pos)))))
      (error "Tag %S not found" name))))
