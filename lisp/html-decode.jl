;;;; html-decode.jl -- Simple HTML displayer
;;;  Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'html-decode)

;; Global details
(defvar html-decode-title nil)

(defvar html-decode-face-alist '((default . default-face)
				 (link . underline-face)
				 (h1 . bold-face)
				 (h2 . underline-face)
				 (h3 . underline-face)
				 (h4 . underline-face)
				 (h5 . underline-face)
				 (h6 . underline-face)
				 (em . bold-face)
				 (strong . bold-face)
				 (cite . italic-face)
				 (dfn . italic-face)
				 (code . default-face)
				 (samp . default-face)
				 (var . italic-face)
				 (kbd . default-face)
				 (tt . default-face)
				 (i . italic-face)
				 (b . bold-face)
				 (big . bold-face)
				 (small . default-face)
				 (s . default-face)
				 (u . underline-face)))

(defvar html-decode-blockquote-indent 4)

;; Current environment
(defvar html-decode-indent nil)
(defvar html-decode-width nil)
(defvar html-decode-fill nil)
(defvar html-decode-callback nil)
(defvar html-decode-display nil)

(defvar html-decode-environment '(html-decode-indent html-decode-width
				  html-decode-fill html-decode-callback
				  html-decode-display))

(defvar html-decode-stack nil)

;; Could be nil, space, line, paragraph
(defvar html-decode-pending nil)
(defvar html-decode-pending-fill nil)

(defvar html-decode-pending-weight '((nil . 0)
				     (space . 1)
				     (line . 2)
				     (paragraph . 3)))


;; High-level parsing

;;;###autoload
(defun html-decode (source dest)
  (let
      ((html-decode-indent 0)
       (html-decode-width (- (car (view-dimensions)) 8))
       (html-decode-fill 'left)
       (html-decode-callback 'html-decode-global-fun)
       (html-decode-display nil)
       (html-decode-stack nil)
       (point (start-of-buffer source))
       end tag)
    (while (< point (end-of-buffer source))
      (setq end (or (char-search-forward ?< point source)
		    (end-of-buffer source)))
      (when (> end point)
	;; Found the next tag, output everything from POINT to END
	;; using the current style.
	(funcall html-decode-display point end source dest)
	(setq point end))
      ;; Then decode the command
      (setq tag (html-decode-tag point source))
      (setq point (car tag))
      (setq tag (cdr tag))
      (funcall (or (get (car tag) html-decode-callback)
		   'html-decode-unknown-tag)
	       tag dest))
    (html-decode-add-pending 'line)
    (html-decode-output-pending dest)))


;; Low-level parsing

(defvar html-decode-char-name-alist '(("lt" . ?<)
				      ("gt" . ?>)
				      ("amp" . ?&)
				      ("quot" . ?\")))

(defun html-decode-string (string)
  (let
      ((point 0)
       start end out)
    (while (string-match "&(#([0-9]+|[0-9a-fA-F]+)|[a-z]+);" string point)
      (setq start (match-start)
	    end (match-end))
      (let
	  (char)
	(if (= (aref string (1+ start)) ?#)
	    ;; Ascii code
	    (if (= (aref string (+ start 2)) ?x)
		;; hex
		(setq char (read-from-string (concat
					      "0x" (expand-last-match "\\2"))))
	      ;; decimal
	      (setq char (read-from-string (expand-last-match "\\2"))))
	  ;; named character
	  (setq char (cdr (assoc (expand-last-match "\\1")
				 html-decode-char-name-alist))))
	(setq out (cons char (cons (substring string point start) out)))
	(setq point end)))
    (if (= point 0)
	string
      (setq out (cons (substring string point) out))
      (apply 'concat (nreverse out)))))

;; Decode the run of text from START to END in buffer SOURCE to point in
;; buffer DEST, assuming that there are no tags in this section
(defun html-decode-run (start end source dest)
  (html-decode-output-pending dest)
  (cond
   ((eq html-decode-fill nil)
    ;; <pre> text
    (insert (copy-area start end source) nil dest))
   (t
    ;; right/left/centered currently, justify not supported
    (let*
	((col (pos-col (char-to-glyph-pos (with-buffer dest
					    (cursor-pos)) dest)))
	 word-end next-word spaces pending-spaces string)
      ;; Work a word of input at a time.
      (while (looking-at "([^< \t\f\n]+)([ \t\f\n]*)" start source)
	(setq word-end (min (match-end 1) end))
	(setq next-word (match-end))
	(setq spaces (not (equal (match-start 2) (match-end 2))))
	(setq string (html-decode-string (copy-area start word-end source)))
	(when (and (> (+ col (length string)) html-decode-width)
		   (/= col html-decode-indent))
	  ;; Too wide, break the line
	  (insert "\n" nil dest)
	  (setq pending-spaces nil)
	  (setq col 0)
	  (with-buffer dest
	    (html-decode-justify-line html-decode-fill (forward-line -1))))
	(when (and (zerop col) (not (zerop html-decode-indent)))
	  (with-buffer dest
	    (indent-to html-decode-indent))
	  (setq col html-decode-indent))
	(when pending-spaces
	  (insert " " nil dest)
	  (setq col (1+ col)))
	(insert string nil dest)
	(setq col (+ col (length string)))
	(setq pending-spaces spaces)
	(setq start next-word))
      (when pending-spaces
	(html-decode-add-pending 'space))))))

;; Returns (END NAME ENTERINGP PARAMS...) or nil
(defun html-decode-tag (point source)
  (let
      (name entering params tem)
    (cond
     ((looking-at "<[ \t\n\f]*(/?)[ \t\n\f]*([a-zA-Z0-9_-]+)[ \t\n\f]*"
		  point source)
      (setq entering (equal (match-end 1) (match-start 1)))
      (setq name (intern (translate-string (expand-last-match "\\2")
					   downcase-table)))
      (setq point (match-end))
      (while (looking-at "[ \t\n\f]*([a-zA-Z0-9_-]+)[ \t\n\f]*=[ \t\n\f]*"
			 point source)
	(setq tem (intern (translate-string (expand-last-match "\\1")
					    downcase-table)))
	(setq point (match-end))
	(if (= (get-char point source) ?\")
	    (let
		((stream (cons source point)))
	      (setq tem (cons tem (read stream)))
	      (setq point (cdr stream)))
	  (or (looking-at "[^> \t\n\f]+" point source)
	      (error "Can't find param value: %s, %s" source point))
	  (setq tem (cons tem (expand-last-match "\\0")))
	  (setq point (match-end)))
	(setq params (cons tem params))))
     ((looking-at "<[ \t\n\f]*!" point source)
      (setq point (or (char-search-forward ?> point source)
		      (error "Unterminated comment: %s, %s" point source))))
     (t
      (error "Malformed tag: %s, %s" point source)))
    (or (looking-at "[ \t\n\f]*>([ \t\n\f]*)" point source)
	(error "No closing greater-than character %s, %s" point source))
    (setq point (match-end))
    (when (not (equal (match-start 1) (match-end 1)))
      (html-decode-add-pending 'space))
    (list* point name entering (nreverse params))))


;; Helper functions

;; EXIT-FUNC is called (EXIT-FUNC TAG TAG-PARAMS DATA...) when the
;; tag is exited.
(defun html-decode-push-env (tag &optional exit-func &rest data)
  (let
      ((symbol (car tag))
       (params (cdr (cdr tag)))
       (env (mapcar #'(lambda (sym)
			(cons sym (symbol-value sym)))
		    html-decode-environment)))
    (setq html-decode-stack (cons (list* exit-func env symbol params data)
				  html-decode-stack))))

(defun html-decode-pop-env (tag)
  (let
      (item done)
  (while (and (car html-decode-stack) (not done))
    (setq item (car html-decode-stack))
    (setq html-decode-stack (cdr html-decode-stack))
    (when (eq (nth 2 item) (car tag))
      (setq done t))
    ;; restore the environment
    (mapc #'(lambda (pair)
	      (set (car pair) (cdr pair))) (nth 1 item))
    ;; then call the exit function
    (and (car item)
	 (apply (car item) (nthcdr 2 item))))))

(defmacro html-decode-tag-entering-p (tag)
  `(car (cdr ,tag)))

(put 'html-decode-tag-with 'lisp-indent 3)
(defmacro html-decode-tag-with (tag dest entry-form exit-func)
  `(if (html-decode-tag-entering-p ,tag)
       (progn
	 (html-decode-push-env ,tag ,exit-func
			       ,dest (with-buffer ,dest (cursor-pos)))
	 ,entry-form)
     (html-decode-pop-env ,tag)))

(defun html-decode-pop-to-block (tag)
  (let
      ((top (car html-decode-stack)))
    (when (and top (eq (nth 1 top) tag))
      ;; topmost tag is what we're looking for. assume it's an
      ;; unclosed item, so pop it
      (html-decode-pop-env tag))))

(defmacro html-decode-elt-face (name)
  `(or (symbol-value (cdr (assq ,name html-decode-face-alist))) default-face))

(defun html-decode-add-pending (type)
  (let
      ((weight (cdr (assq type html-decode-pending-weight)))
       (current-weight (cdr (assq html-decode-pending
				  html-decode-pending-weight))))
    (when (> weight current-weight)
      (setq html-decode-pending type)
      (setq html-decode-pending-fill html-decode-fill))))

(defun html-decode-output-pending (dest)
  (unless (null html-decode-pending)
    (let
	((original (with-buffer dest (cursor-pos)))
	 new)
      (cond
       ((eq html-decode-pending 'space)
	(setq new (insert " " nil dest)))
       ((eq html-decode-pending 'line)
	(with-buffer dest
	  (unless (equal (cursor-pos) (start-of-buffer))
	    (html-decode-justify-line html-decode-pending-fill)
	    (setq new (insert "\n")))))
       ((eq html-decode-pending 'paragraph)
	(with-buffer dest
	  (unless (equal (cursor-pos) (start-of-buffer))
	    (html-decode-justify-line html-decode-pending-fill)
	    (setq new (insert "\n\n"))))))
      (setq html-decode-pending nil)
      (when new
	;; work back up the stack, fixing the start position of
	;; regions that thought they started at the original position
	;; before pending output changed it..
	;; XXX this assumes that the second DATA argument given to
	;; XXX html-decode-push-env is the position in the output
	(catch 'foo
	  (mapc #'(lambda (frame)
		    (if (equal original (nth (+ 3 2) frame))
			;; a match, so update it to the new position
			(rplaca (nthcdr (+ 3 2) frame) new)
		      ;; no others can have either, so abort
		      (throw 'foo t)))
		html-decode-stack))))))

(defun html-decode-insert (string dest)
  (html-decode-output-pending dest)
  (insert string nil dest))

(defun html-decode-justify-line (fill-type &optional pos)
  (unless (eq fill-type 'left)
    (let*
	((spos (indent-pos pos))
	 (epos (char-to-glyph-pos (re-search-forward
				   " *$" (start-of-line pos))))
	 (len (- (pos-col epos) (pos-col spos))))
      (when (> (match-end) (match-start))
	(delete-area (match-start) (match-end)))
      (cond
       ((<= len 0))
       ((> len html-decode-width)
	(set-indent-pos (start-of-line pos)))
       (t
	(cond
	 ((eq fill-type 'center)
	  (set-indent-pos (pos (/ (- html-decode-width len) 2)
			       (pos-line spos))))
	 ((eq fill-type 'right)
	  (set-indent-pos (pos (- html-decode-width len)
			       (pos-line spos))))))))))

(defun html-decode-tag:align (tag)
  (let
      ((align (cdr (assq 'align (nthcdr 2 tag)))))
    (when align
      (setq html-decode-fill (intern (translate-string
				      align downcase-table))))))


;; Global tags

(put 'html 'html-decode-global-fun 'html-decode:html)
(put 'html 'html-decode-html-fun 'html-decode:html)
(defun html-decode:html (tag)
  (if (html-decode-tag-entering-p tag)
      (progn
	(html-decode-push-env tag)
	(setq html-decode-callback 'html-decode-html-fun))
    (html-decode-pop-env tag)))

(defun html-decode-unknown-tag (tag)
  (format (stderr-file) "warning: unknown HTML tag: %s\n" tag))


;; Tags in html section

(put 'head 'html-decode-html-fun 'html-decode:head)
(put 'head 'html-decode-head-fun 'html-decode:head)
(defun html-decode:head (tag)
  (if (html-decode-tag-entering-p tag)
      (progn
	(html-decode-push-env tag)
	(setq html-decode-callback 'html-decode-head-fun))
    (html-decode-pop-env tag)))

(put 'body 'html-decode-html-fun 'html-decode:body)
(put 'body 'html-decode-body-fun 'html-decode:body)
(defun html-decode:body (tag)
  (if (html-decode-tag-entering-p tag)
      (progn
	(html-decode-push-env tag)
	;;; XXX look for color parameters
	(setq html-decode-callback 'html-decode-body-fun)
	(setq html-decode-display 'html-decode-run))
    (html-decode-pop-env tag)))


;; Tags in head section

(put 'title 'html-decode-head-fun 'html-decode:title)
(defun html-decode:title (tag)
  (if (html-decode-tag-entering-p tag)
      (progn
	(html-decode-push-env tag)
	(setq html-decode-display
	      #'(lambda (start end source dest)
		  ;; XXX clean this up properly
		  (setq html-decode-title (copy-area start end source)))))
    (html-decode-pop-env tag)))


;; Tags in body section

(put 'p 'html-decode-body-fun 'html-decode:p)
(defun html-decode:p (tag dest)
  (when (html-decode-tag-entering-p tag)
    (html-decode-pop-to-block 'p))
  (html-decode-tag-with tag dest
      (progn
	(html-decode-tag:align tag)
	(html-decode-add-pending 'paragraph))
    #'(lambda ()
	(html-decode-add-pending 'paragraph))))

(put 'br 'html-decode-body-fun 'html-decode:br)
(defun html-decode:br (tag dest)
  (html-decode-add-pending 'line))

(put 'a 'html-decode-body-fun 'html-decode:a)
(defun html-decode:a (tag dest)
  (html-decode-tag-with tag dest
      nil
    #'(lambda (name params dest start)
	(with-buffer dest
	  (make-extent start (cursor-pos)
		       (list 'face (html-decode-elt-face 'link)
			     'html-params params))))))

(put 'h1 'html-decode-body-fun 'html-decode:h1)
(defun html-decode:h1 (tag dest)
  (html-decode-header 'h1 tag dest))

(put 'h2 'html-decode-body-fun 'html-decode:h2)
(defun html-decode:h2 (tag dest)
  (html-decode-header 'h2 tag dest))

(put 'h3 'html-decode-body-fun 'html-decode:h3)
(defun html-decode:h3 (tag dest)
  (html-decode-header 'h3 tag dest))

(put 'h4 'html-decode-body-fun 'html-decode:h4)
(defun html-decode:h4 (tag dest)
  (html-decode-header 'h4 tag dest))

(put 'h5 'html-decode-body-fun 'html-decode:h5)
(defun html-decode:h5 (tag dest)
  (html-decode-header 'h5 tag dest))

(put 'h6 'html-decode-body-fun 'html-decode:h6)
(defun html-decode:h6 (tag dest)
  (html-decode-header 'h6 tag dest))

(defun html-decode-header (name tag dest)
  (html-decode-tag-with tag dest
      (progn
	(html-decode-tag:align tag)
	(setq html-decode-fill 'center))
    #'(lambda (name params dest start)
	(with-buffer dest
	  (make-extent start (cursor-pos)
		       (list 'face (html-decode-elt-face name)
			     'html-params params))))))

(put 'em 'html-decode-body-fun 'html-decode:em)
(defun html-decode:em (tag dest)
  (html-decode-fragment 'em tag dest))

(put 'strong 'html-decode-body-fun 'html-decode:strong)
(defun html-decode:strong (tag dest)
  (html-decode-fragment 'strong tag dest))

(put 'cite 'html-decode-body-fun 'html-decode:cite)
(defun html-decode:cite (tag dest)
  (html-decode-fragment 'cite tag dest))

(put 'dfn 'html-decode-body-fun 'html-decode:dfn)
(defun html-decode:dfn (tag dest)
  (html-decode-fragment 'dfn tag dest))

(put 'code 'html-decode-body-fun 'html-decode:code)
(defun html-decode:code (tag dest)
  (html-decode-fragment 'code tag dest))

(put 'samp 'html-decode-body-fun 'html-decode:samp)
(defun html-decode:samp (tag dest)
  (html-decode-fragment 'samp tag dest))

(put 'kbd 'html-decode-body-fun 'html-decode:kbd)
(defun html-decode:kbd (tag dest)
  (html-decode-fragment 'kbd tag dest))

(put 'var 'html-decode-body-fun 'html-decode:var)
(defun html-decode:var (tag dest)
  (html-decode-fragment 'var tag dest))

(put 'tt 'html-decode-body-fun 'html-decode:tt)
(defun html-decode:tt (tag dest)
  (html-decode-fragment 'tt tag dest))

(put 'i 'html-decode-body-fun 'html-decode:i)
(defun html-decode:i (tag dest)
  (html-decode-fragment 'i tag dest))

(put 'b 'html-decode-body-fun 'html-decode:b)
(defun html-decode:b (tag dest)
  (html-decode-fragment 'b tag dest))

(put 'big 'html-decode-body-fun 'html-decode:big)
(defun html-decode:big (tag dest)
  (html-decode-fragment 'big tag dest))

(put 'small 'html-decode-body-fun 'html-decode:small)
(defun html-decode:small (tag dest)
  (html-decode-fragment 'small tag dest))

(put 's 'html-decode-body-fun 'html-decode:s)
(defun html-decode:s (tag dest)
  (html-decode-fragment 's tag dest))

(put 'u 'html-decode-body-fun 'html-decode:u)
(defun html-decode:u (tag dest)
  (html-decode-fragment 'u tag dest))

(defun html-decode-fragment (name tag dest)
  (html-decode-tag-with tag dest
      nil
    #'(lambda (name params dest start)
	(with-buffer dest
	  (make-extent start (cursor-pos)
		       (list 'face (html-decode-elt-face name)
			     'html-params params))))))

(put 'pre 'html-decode-body-fun 'html-decode:pre)
(defun html-decode:pre (tag dest)
  (html-decode-tag-with tag dest
      (progn
	(html-decode-add-pending 'paragraph)
	(html-decode-tag:align tag)
	(setq html-decode-fill nil))
    #'(lambda ()
	(html-decode-add-pending 'paragraph))))

(put 'blockquote 'html-decode-body-fun 'html-decode:blockquote)
(defun html-decode:blockquote (tag dest)
  (html-decode-tag-with tag dest
      (progn
	(html-decode-add-pending 'paragraph)
	(html-decode-tag:align tag)
	(setq html-decode-fill 'left
	      html-decode-indent (+ html-decode-blockquote-indent
				    html-decode-indent)))
    #'(lambda ()
	(html-decode-add-pending 'paragraph))))

(put 'q 'html-decode-body-fun 'html-decode:q)
(defun html-decode:q (tag dest)
  (if (html-decode-tag-entering-p tag)
      (html-decode-insert "``" dest)
    (html-decode-insert "''" dest)))

(put 'table 'html-decode-body-fun 'html-decode:table)
(defun html-decode:table (tag dest)
  (html-decode-tag-with tag dest
      (progn
	(html-decode-add-pending 'paragraph)
	(setq html-decode-fill 'left))
    #'(lambda ()
	(html-decode-add-pending 'paragraph))))

(put 'tr 'html-decode-body-fun 'html-decode:tr)
(defun html-decode:tr (tag dest)
  (html-decode-add-pending 'paragraph))

(put 'td 'html-decode-body-fun 'html-decode:td)
(defun html-decode:td (tag dest)
  (html-decode-add-pending 'line))

(put 'img 'html-decode-body-fun 'html-decode:img)
(defun html-decode:img (tag dest)
  (let
      ((params (cdr (cdr tag))))
    (if (assq 'alt params)
	(html-decode-insert (cdr (assq 'alt params)) dest)
      (html-decode-insert "[inline image]" dest))))

(put 'center 'html-decode-body-fun 'html-decode:center)
(defun html-decode:center (tag dest)
  (html-decode-tag-with tag dest
      (progn
	(html-decode-add-pending 'paragraph)
	(setq html-decode-fill 'center))
    #'(lambda ()
	(html-decode-add-pending 'paragraph))))
