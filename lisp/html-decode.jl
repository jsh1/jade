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

;; Commentary:
;;
;; This file provides a basic mechanism for rendering HTML source
;; in one buffer into another buffer. The single public entrypoint
;; is called as:
;;
;;	(html-decode SOURCE-BUFFER DEST-BUFFER)
;;
;; All text in SOURCE-BUFFER is translated, inserted from the current
;; position in DEST-BUFFER.
;;
;; TODO:
;;  * Definition lists (DL, DT, DD)
;;  * FONT tags?
;;  * Forms
;;  * Implicitly detect HTML, HEAD, and BODY tags


;; Configuration

(defface html-decode-link-face "Face used to display HTML links"
  (set-face-attribute html-decode-link-face 'foreground "darkblue")
  (set-face-attribute html-decode-link-face 'underline t))

(defvar html-decode-face-alist '((default . default-face)
				 (link . html-decode-link-face)
				 (h1 . underline-face)
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
				 (u . underline-face))
  "List of (TAG . FACE) defining the FACE to display text fragments
in TAG with.")

(defvar html-decode-blockquote-indent 4
  "Size of indent for <BLOCKQUOTE> regions.")

(defvar html-decode-list-indent 4
  "Size of indent for each list level.")

(defvar html-decode-echo-unknown-tags nil
  "When non-nil, print all unknown tags encountered to stderr.")

(defvar html-decode-echo-warnings t
  "When non-nil, print warnings about badly written HTML to stderr.")


;; Global variables

;; Title of the document
(defvar html-decode-title nil)

;; Base HREF
(defvar html-decode-base nil)

;; Tags that don't require an end-tag
(defvar html-decode-optional-close-tags '(p li dt dd))

(defvar html-decode-block-tags 
  '(head body base p h1 h2 h3 h4 h5 h6 pre blockquote table center
    ul ol dir menu dfn))

;; Current environment
(defvar html-decode-indent nil)
(defvar html-decode-width nil)
(defvar html-decode-fill nil)
(defvar html-decode-callback nil)
(defvar html-decode-display nil)
(defvar html-decode-list-type nil)
(defvar html-decode-list-id nil)
(defvar html-decode-list-depth nil)

(defvar html-decode-def-environ '(html-decode-environment
				  html-decode-indent html-decode-list-type
				  html-decode-list-id html-decode-list-depth))

;; Variables whose values are recorded when pushing and popping tags
(defvar html-decode-environment nil)

;; Stack of (EXIT-FUNC ENV-ALIST TAG-SYMBOL PARAM-ALIST DEST DATA...)
(defvar html-decode-stack nil)

;; Position in input after current item
(defvar html-decode-point nil)

;; Source buffer
(defvar html-decode-source nil)

;; Could be nil, space, line, paragraph, list. In order of importance,
;; i.e. paragraph overrides line, but line overrides space.
(defvar html-decode-pending nil)
(defvar html-decode-pending-fill nil)
(defvar html-decode-list-pending nil)

;; Weights giving precedence to pending output types
(defvar html-decode-pending-weight '((nil . 0)
				     (space . 1)
				     (line . 2)
				     (paragraph . 3)))


;; High-level parsing

;;;###autoload
(defun html-decode (source dest)
  "Render a representation of the HTML data stored in buffer SOURCE, to the
current position in buffer DEST. Returns an alist defining certain details
of the document, currently only `title' and `base' keys are defined."
  (let
      ((html-decode-environment html-decode-def-environ)
       (html-decode-indent 0)
       (html-decode-width (- (car (view-dimensions)) 8))
       (html-decode-fill 'left)
       (html-decode-callback 'html-decode-global-fun)
       (html-decode-display nil)
       (html-decode-list-type nil)
       (html-decode-list-id nil)
       (html-decode-list-depth 0)
       (html-decode-stack nil)
       (html-decode-pending nil)
       (html-decode-pending-fill nil)
       (html-decode-title nil)
       (html-decode-base nil)
       (html-decode-point (start-of-buffer source))
       (html-decode-source source)
       end tag)
    (while (< html-decode-point (end-of-buffer source))
      (setq end (or (char-search-forward ?< html-decode-point source)
		    (end-of-buffer source)))
      (when (> end html-decode-point)
	;; Found the next tag, output everything from POINT to END
	;; using the current style.
	(if html-decode-display
	    (funcall html-decode-display
		     (prog1 html-decode-point (setq html-decode-point end))
		     end source dest)
	  (setq html-decode-point end)))
      ;; Then decode the command
      (setq tag (html-decode-tag html-decode-point source))
      (setq html-decode-point (car tag))
      (setq tag (cdr tag))
      (funcall (or (get (car tag) html-decode-callback)
		   'html-decode-unknown-tag)
	       tag dest))
    (html-decode-add-pending 'line)
    (html-decode-output-pending dest)
    (nconc (and html-decode-title (list (cons 'title html-decode-title)))
	   (and html-decode-base (list (cons 'base html-decode-base))))))


;; Low-level parsing

(defvar html-decode-char-name-alist '(("lt" . ?<)
				      ("gt" . ?>)
				      ("amp" . ?&)
				      ("quot" . ?\")))

;; Given some body text STRING, expand any quoted characters
(defun html-decode-string (string)
  (let
      ((point 0)
       start end out)
    (while (string-match "&(#([0-9]+|[0-9a-fA-F]+)|[a-z]+);?" string point)
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
  (let
      ((original (with-buffer dest (cursor-pos)))
       tem)
    (cond
     ((eq html-decode-fill nil)
      ;; <pre> text
      (insert (html-decode-string (copy-area start end source)) nil dest))
     (t
      ;; right/left/centered currently, justify not supported
      (let*
	  ((col (pos-col (char-to-glyph-pos (with-buffer dest
					      (cursor-pos)) dest)))
	   word-end next-word spaces pending-spaces string)
	;; Work a word of input at a time.
	(while (looking-at "([^< \t\r\f\n]+)([ \t\r\f\n]*)" start source)
	  (setq word-end (min (match-end 1) end))
	  (setq next-word (match-end))
	  (setq spaces (not (equal (match-start 2) (match-end 2))))
	  (setq string (html-decode-string (copy-area start word-end source)))
	  (when (and (> (+ col (length string)) html-decode-width)
		     (/= col html-decode-indent))
	    ;; Too wide, break the line
	    (setq tem (with-buffer dest (cursor-pos)))
	    (insert "\n" nil dest)
	    (setq pending-spaces nil)
	    (setq col 0)
	    (when (equal original tem)
	      (html-decode-patch-starts original (with-buffer dest
						   (cursor-pos))
					html-decode-stack)
	      (setq original tem))
	    (when html-decode-pending-fill
	      (with-buffer dest
		(html-decode-justify-line html-decode-fill (forward-line -1)))
	      (setq html-decode-pending-fill nil)))
	  (when (and (zerop col) (not (zerop html-decode-indent)))
	    (with-buffer dest
	      (setq tem (cursor-pos))
	      (indent-to html-decode-indent)
	      (when (equal original tem)
		(html-decode-patch-starts original (cursor-pos)
					  html-decode-stack)))
	    (setq col html-decode-indent))
	  (when pending-spaces
	    (insert " " nil dest)
	    (setq col (1+ col)))
	  (insert string nil dest)
	  (setq col (+ col (length string)))
	  (setq pending-spaces spaces)
	  (setq html-decode-pending-fill t)
	  (setq start next-word))
	(when pending-spaces
	  (html-decode-add-pending 'space)))))))

;; Returns (END NAME ENTERINGP PARAMS...) or nil
(defun html-decode-tag (point source)
  (let
      (name entering params tem)
    (cond
     ((looking-at "<[ \t\r\n\f]*(/?)[ \t\r\n\f]*([a-zA-Z0-9_-]+)[ \t\r\n\f]*"
		  point source)
      (setq entering (equal (match-end 1) (match-start 1)))
      (setq name (intern (translate-string (expand-last-match "\\2")
					   downcase-table)))
      (setq point (match-end))
      (while (looking-at "[ \t\r\n\f]*([a-zA-Z0-9_-]+)[ \t\r\n\f]*(=[ \t\r\n\f]*)?"
			 point source)
	(setq tem (intern (translate-string (expand-last-match "\\1")
					    downcase-table)))
	(setq point (match-end))
	(unless (equal (match-start 2) (match-end 2))
	  (if (or (= (get-char point source) ?\")
		  (= (get-char point source) ?\'))
	      (let
		  ((end (or (char-search-forward (get-char point source)
						 (forward-char 1 point source)
						 source)
			    (error "Unterminated string: %s, %s"
				   source point))))
		(setq tem (cons tem (copy-area (forward-char 1 point source)
					       end source)))
		(setq point (forward-char 1 end source)))
	    (or (looking-at "[^> \t\r\n\f]+" point source)
		(error "Can't find param value: %s, %s" source point))
	    (setq tem (cons tem (expand-last-match "\\0")))
	    (setq point (match-end))))
	(setq params (cons tem params))))
     ((looking-at "<![ \t\r\n\f]*" point source)
      (setq point (match-end))
      (while (not (looking-at "[ \t\r\n\f]*>" point source))
	(if (looking-at "[ \t\r\n\f]*--" point source)
	    ;; skip comment
	    (setq point (and (or (search-forward "--" (match-end) source)
				 (error "Unterminated comment: %s, %s"
					source point))
			     (match-end)))
	  ;; skip random declaration garbage?
	  (setq point (or (re-search-forward "--|>" point source)
			  (error "Unterminated declaration: %s, %s"
				 source point))))))
     (t
      (error "Malformed tag: %s, %s" point source)))
    (or (looking-at "[ \t\r\n\f]*>([ \t\r\n\f]*)" point source)
	(error "No closing greater-than character %s, %s" point source))
    (setq point (match-end))
    (when (not (equal (match-start 1) (match-end 1)))
      (html-decode-add-pending 'space))
    (list* point name entering (nreverse params))))


;; Helper functions

;; Record that VAR needs to be stashed in the next saved environment
(defun html-decode-add-env (var)
  (unless (memq var html-decode-environment)
    (setq html-decode-environment (cons var html-decode-environment))))

;; EXIT-FUNC is called (EXIT-FUNC TAG-SYMBOL TAG-PARAMS DATA...)
;; when the tag is exited.
(defun html-decode-push-env (tag dest &optional exit-func &rest data)
  (let*
      ((symbol (car tag))
       (params (cdr (cdr tag)))
       (env (mapcar #'(lambda (sym)
			(cons sym (symbol-value sym)))
		    html-decode-environment))
       (frame (list* exit-func env symbol params dest data)))
    ;; If leaving this level would cause the fill style to
    ;; be lost, and the tag is a block tag, then justify
    ;; the line now
    (when (and html-decode-pending-fill
	       (memq 'html-decode-fill html-decode-environment)
	       (memq (car tag) html-decode-block-tags))
      (with-buffer dest
	(let
	    ((old (glyph-to-char-pos (indent-pos))))
	  (html-decode-justify-line html-decode-fill)
	  (html-decode-patch-starts old (glyph-to-char-pos (indent-pos))
				    (cons frame html-decode-stack))
	  (setq html-decode-pending-fill nil))))
    (setq html-decode-environment html-decode-def-environ)
    (setq html-decode-stack (cons frame html-decode-stack))))

(defun html-decode-close-frame (tag-sym dest)
  ;; If leaving this level would cause the fill style to
  ;; be lost, and the tag is a block tag, then justify
  ;; the line now
  (let
      ((frame (car html-decode-stack)))
    (setq html-decode-stack (cdr html-decode-stack))
    (when (and html-decode-pending-fill
	       (assq 'html-decode-fill (nth 1 frame))
	       (memq tag-sym html-decode-block-tags))
      (with-buffer dest
	(let
	    ((old (glyph-to-char-pos (indent-pos))))
	  (html-decode-justify-line html-decode-fill)
	  (html-decode-patch-starts old (glyph-to-char-pos (indent-pos))
				    (cons frame html-decode-stack))
	  (setq html-decode-pending-fill nil))))
    ;; restore the environment
    (mapc #'(lambda (pair)
	      (set (car pair) (cdr pair))) (nth 1 frame))
    ;; then call the exit function
    (and (car frame)
	 (apply (car frame) (nthcdr 2 frame)))))

;; Pop all environments back to the first tag of type TAG
(defun html-decode-pop-env (tag dest)
  (let
      (item)
    (when (and (not (memq (car tag) html-decode-optional-close-tags))
	       (memq (nth 2 (car html-decode-stack))
		     html-decode-optional-close-tags))
      ;; Close the trailing optional tags
      (html-decode-pop-optional tag dest))
    (if (not (eq (car tag) (nth 2 (car html-decode-stack))))
	(and html-decode-echo-warnings
	     (format (stderr-file)
		     "warning: badly nested HTML: %s, %s, %s, %s\n"
		     html-decode-source html-decode-point
		     (car tag) (nth 2 (car html-decode-stack))))
      (html-decode-close-frame (car tag) dest))))

;; Are we entering or leaving TAG
(defmacro html-decode-tag-entering-p (tag)
  `(car (cdr ,tag)))

;; Saves the environment, then executes ENTRY-FORM when the TAG is
;; being entered, or restores the environment and calls
;; (EXIT-FUNC TAG-SYMBOL TAG-PARAMS DEST START-POINT)
;; when exiting. START-POINT is the position in the output buffer
;; where the first output from this tag was inserted.
(put 'html-decode-tag-with 'lisp-indent 3)
(defmacro html-decode-tag-with (tag dest entry-form exit-func)
  `(if (html-decode-tag-entering-p ,tag)
       (progn
	 (html-decode-push-env ,tag ,dest ,exit-func
			       (with-buffer ,dest (cursor-pos)))
	 ,entry-form)
     (html-decode-pop-env ,tag ,dest)))

;; Should be called when entering TAG whose end tag is optional. It
;; will forcibly close any optional tags back to the innermost entering
;; tag of the same type as TAG
(defun html-decode-pop-optional (tag dest)
  (let
      ((frame (car html-decode-stack)))
    ;; Is there a contiguous sequence of optional close tags
    ;; between the top of the stack and the first level of
    ;; type TAG
    (catch 'foo
      (while (and html-decode-stack
		  (memq (nth 2 frame) html-decode-optional-close-tags))
	(if (eq (nth 2 frame) (car tag))
	    (progn
	      (setq html-decode-stack (cdr html-decode-stack))
	      (throw 'foo t))
	  (html-decode-close-frame (car tag) dest)
	  (setq frame (car html-decode-stack)))))))

;; Return the face to be used for a tag of type NAME
(defmacro html-decode-elt-face (name)
  `(or (symbol-value (cdr (assq ,name html-decode-face-alist))) default-face))

;; Record that before outputting any text, something of type TYPE should
;; be output first
(defun html-decode-add-pending (type)
  (let
      ((weight (cdr (assq type html-decode-pending-weight)))
       (current-weight (cdr (assq html-decode-pending
				  html-decode-pending-weight))))
    (when (> weight current-weight)
      (setq html-decode-pending type))))

;; Flush any pending output, so that text can be inserted in the output buffer
(defun html-decode-output-pending (dest)
  (when (or html-decode-pending html-decode-list-pending)
    (let
	((original (with-buffer dest (cursor-pos)))
	 new)
      (unless (or (null html-decode-pending) (eq html-decode-pending 'space))
	(when html-decode-pending-fill
	  (with-buffer dest
	    (html-decode-justify-line html-decode-fill))
	  (setq html-decode-pending-fill nil)))
      (cond
       ((eq html-decode-pending 'space)
	(setq new (insert " " nil dest)))
       ((eq html-decode-pending 'line)
	(with-buffer dest
	  (setq new (insert "\n"))))
       ((eq html-decode-pending 'paragraph)
	(with-buffer dest
	  (setq new (insert "\n\n")))))
      (when html-decode-list-pending
	(with-buffer dest
	  (html-decode-insert-list html-decode-list-type
				   html-decode-list-id)
	  (setq html-decode-list-id (1+ html-decode-list-id))
	  (setq new (cursor-pos))))
      (setq html-decode-pending nil)
      (setq html-decode-pending-fill nil)
      (setq html-decode-list-pending nil)
      (when new
	(html-decode-patch-starts original new html-decode-stack)))))

(defun html-decode-patch-starts (old new stack)
  ;; work back up STACK, fixing the start position of regions that
  ;; thought they started at the original position before pending
  ;; output changed it..
  ;; XXX this assumes that the first DATA argument given to
  ;; XXX html-decode-push-env is the position in the output
  (catch 'foo
    (mapc #'(lambda (frame)
	      (if (equal old (nth (+ 4 1) frame))
		  ;; a match, so update it to the new position
		  (rplaca (nthcdr (+ 4 1) frame) new)
		;; no others can have either, so abort
		(throw 'foo t))) stack)))

;; Insert STRING in output buffer DEST, flushing output first
(defun html-decode-insert (string dest)
  (html-decode-output-pending dest)
  (insert string nil dest)
  (setq html-decode-pending-fill t))

;; Justify the line at POS, according to FILL-TYPE
(defun html-decode-justify-line (fill-type &optional pos)
  (unless (or (null fill-type) (eq fill-type 'left))
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

;; Insert a list header of type TYPE, given that this is the ID'th
;; entry in the current list
(defun html-decode-insert-list (type id)
  (indent-to (- html-decode-indent 2))
  (cond
   ((eq type 'ul)
    (insert "* "))
   ((eq type 'ol)
    (format (current-buffer) "%d. " id)))
  (setq html-decode-pending-fill t))

;; Decode the ALIGN attribute from TAG, acting on its value
(defun html-decode-tag:align (tag)
  (let
      ((align (cdr (assq 'align (nthcdr 2 tag)))))
    (when align
      (setq html-decode-fill (intern (translate-string
				      align downcase-table))))))

;; Must be called before the html-decode-tag-with call
(defun html-decode-pre-tag:align (tag)
  (when (and (html-decode-tag-entering-p tag)
	     (assq 'align (nthcdr 2 tag)))
    (html-decode-add-env 'html-decode-fill)))


;; Global tags

(put 'html 'html-decode-global-fun 'html-decode:html)
(put 'html 'html-decode-html-fun 'html-decode:html)
(defun html-decode:html (tag dest)
  (if (html-decode-tag-entering-p tag)
      (progn
	(html-decode-add-env 'html-decode-callback)
	(html-decode-push-env tag dest)
	(setq html-decode-callback 'html-decode-html-fun))
    (html-decode-pop-env tag dest)))

(defun html-decode-unknown-tag (tag)
  (when html-decode-echo-unknown-tags
    (format (stderr-file) "warning: unknown HTML tag: %s\n" tag)))


;; Tags in html section

(put 'head 'html-decode-global-fun 'html-decode:head)
(put 'head 'html-decode-html-fun 'html-decode:head)
(put 'head 'html-decode-head-fun 'html-decode:head)
(defun html-decode:head (tag dest)
  (if (html-decode-tag-entering-p tag)
      (progn
	(html-decode-add-env 'html-decode-callback)
	(html-decode-push-env tag dest)
	(setq html-decode-callback 'html-decode-head-fun))
    (html-decode-pop-env tag dest)))

(put 'body 'html-decode-global-fun 'html-decode:body)
(put 'body 'html-decode-html-fun 'html-decode:body)
(put 'body 'html-decode-body-fun 'html-decode:body)
(defun html-decode:body (tag dest)
  (if (html-decode-tag-entering-p tag)
      (progn
	(html-decode-add-env 'html-decode-callback)
	(html-decode-add-env 'html-decode-display)
	(html-decode-push-env tag dest)
	;;; XXX look for color parameters
	(setq html-decode-callback 'html-decode-body-fun)
	(setq html-decode-display 'html-decode-run))
    (html-decode-pop-env tag dest)))

(put 'frameset 'html-decode-global-fun 'html-decode:frameset)
(put 'frameset 'html-decode-html-fun 'html-decode:frameset)
(put 'frameset 'html-decode-frameset-fun 'html-decode:frameset)
(defun html-decode:frameset (tag dest)
  (if (html-decode-tag-entering-p tag)
      (progn
	(html-decode-add-env 'html-decode-callback)
	(html-decode-push-env tag dest)
	(setq html-decode-callback 'html-decode-frameset-fun)
	(html-decode-add-pending 'paragraph))
    (html-decode-pop-env tag dest)
    (html-decode-add-pending 'paragraph)))


;; Tags in head section

(put 'title 'html-decode-global-fun 'html-decode:title)
(put 'title 'html-decode-html-fun 'html-decode:title)
(put 'title 'html-decode-head-fun 'html-decode:title)
(defun html-decode:title (tag dest)
  (if (html-decode-tag-entering-p tag)
      (progn
	(html-decode-add-env 'html-decode-display)
	(html-decode-push-env tag dest)
	(setq html-decode-display
	      #'(lambda (start end source dest)
		  ;; XXX clean this up properly
		  (setq html-decode-title (copy-area start end source)))))
    (html-decode-pop-env tag dest)))

(put 'base 'html-decode-head-fun 'html-decode:base)
(defun html-decode:base (tag)
  (when (html-decode-tag-entering-p tag)
    (let
	((href (cdr (assq 'href (nthcdr 2 tag)))))
      (when href
	(setq html-decode-base href)))))


;; Tags in body section

(put 'p 'html-decode-body-fun 'html-decode:p)
(defun html-decode:p (tag dest)
  (when (html-decode-tag-entering-p tag)
    (html-decode-pop-optional tag dest))
  (html-decode-pre-tag:align tag)
  (html-decode-tag-with tag dest
      (progn
	(html-decode-tag:align tag)
	(html-decode-add-pending 'paragraph))
    #'(lambda (name params dest start)
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
			     'html-anchor-params params))))))

(put 'h1 'html-decode-body-fun 'html-decode-header)
(put 'h2 'html-decode-body-fun 'html-decode-header)
(put 'h3 'html-decode-body-fun 'html-decode-header)
(put 'h4 'html-decode-body-fun 'html-decode-header)
(put 'h5 'html-decode-body-fun 'html-decode-header)
(put 'h6 'html-decode-body-fun 'html-decode-header)

(defun html-decode-header (tag dest)
  (html-decode-pre-tag:align tag)
  (html-decode-tag-with tag dest
      (progn
	(html-decode-tag:align tag)
	(html-decode-add-pending 'paragraph))
    #'(lambda (name params dest start)
	(with-buffer dest
	  (make-extent start (cursor-pos)
		       (list 'face (html-decode-elt-face name)
			     'html-params params)))
	(html-decode-add-pending 'paragraph))))

(put 'em 'html-decode-body-fun 'html-decode-fragment)
(put 'strong 'html-decode-body-fun 'html-decode-fragment)
(put 'cite 'html-decode-body-fun 'html-decode-fragment)
(put 'dfn 'html-decode-body-fun 'html-decode-fragment)
(put 'code 'html-decode-body-fun 'html-decode-fragment)
(put 'samp 'html-decode-body-fun 'html-decode-fragment)
(put 'kbd 'html-decode-body-fun 'html-decode-fragment)
(put 'var 'html-decode-body-fun 'html-decode-fragment)
(put 'tt 'html-decode-body-fun 'html-decode-fragment)
(put 'i 'html-decode-body-fun 'html-decode-fragment)
(put 'b 'html-decode-body-fun 'html-decode-fragment)
(put 'big 'html-decode-body-fun 'html-decode-fragment)
(put 'small 'html-decode-body-fun 'html-decode-fragment)
(put 's 'html-decode-body-fun 'html-decode-fragment)
(put 'u 'html-decode-body-fun 'html-decode-fragment)

(defun html-decode-fragment (tag dest)
  (html-decode-tag-with tag dest
      nil
    #'(lambda (name params dest start)
	(with-buffer dest
	  (make-extent start (cursor-pos)
		       (list 'face (html-decode-elt-face name)
			     'html-params params))))))

(put 'pre 'html-decode-body-fun 'html-decode:pre)
(defun html-decode:pre (tag dest)
  (when (html-decode-tag-entering-p tag)
    (html-decode-add-env 'html-decode-fill))
  (html-decode-tag-with tag dest
      (progn
	(html-decode-add-pending 'paragraph)
	(html-decode-tag:align tag)
	(setq html-decode-fill nil))
    #'(lambda (name params dest start)
	(html-decode-add-pending 'paragraph))))

(put 'blockquote 'html-decode-body-fun 'html-decode:blockquote)
(defun html-decode:blockquote (tag dest)
  (html-decode-pre-tag:align tag)
  (html-decode-tag-with tag dest
      (progn
	(html-decode-add-pending 'paragraph)
	(html-decode-tag:align tag)
	(setq html-decode-fill 'left
	      html-decode-indent (+ html-decode-blockquote-indent
				    html-decode-indent)))
    #'(lambda (name params dest start)
	(html-decode-add-pending 'paragraph))))

(put 'q 'html-decode-body-fun 'html-decode:q)
(defun html-decode:q (tag dest)
  (if (html-decode-tag-entering-p tag)
      (html-decode-insert "``" dest)
    (html-decode-insert "''" dest)))

(put 'table 'html-decode-body-fun 'html-decode:table)
(defun html-decode:table (tag dest)
  (when (html-decode-tag-entering-p tag)
    (html-decode-add-env 'html-decode-fill))
  (html-decode-tag-with tag dest
      (progn
	(html-decode-add-pending 'paragraph)
	(setq html-decode-fill 'left))
    #'(lambda (name params dest start)
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
  (when (html-decode-tag-entering-p tag)
    (html-decode-add-env 'html-decode-fill))
  (html-decode-tag-with tag dest
      (progn
	(html-decode-add-pending 'paragraph)
	(setq html-decode-fill 'center))
    #'(lambda (name params dest start)
	(html-decode-add-pending 'paragraph))))

(put 'ul 'html-decode-body-fun 'html-decode-list)
(put 'ol 'html-decode-body-fun 'html-decode-list)
(put 'dir 'html-decode-body-fun 'html-decode-list)
(put 'menu 'html-decode-body-fun 'html-decode-list)

(defun html-decode-list (tag dest)
  (html-decode-tag-with tag dest
      (progn
	(if (zerop html-decode-list-depth)
	    (html-decode-add-pending 'paragraph)
	  (html-decode-add-pending 'line))
	(setq html-decode-list-type (car tag))
	(setq html-decode-list-id 1)
	(setq html-decode-list-depth (1+ html-decode-list-depth))
	(setq html-decode-indent (+ html-decode-indent
				    html-decode-list-indent)))
    #'(lambda (name params dest start)
	(if (zerop html-decode-list-depth)
	    (html-decode-add-pending 'paragraph)
	  (html-decode-add-pending 'line)))))

(put 'li 'html-decode-body-fun 'html-decode:li)
(defun html-decode:li (tag dest)
  (when (html-decode-tag-entering-p tag)
    (html-decode-pop-optional tag dest))
  (html-decode-tag-with tag dest
      (progn
	(html-decode-add-pending 'line)
	(setq html-decode-list-pending (car tag)))
    nil))

(put 'script 'html-decode-body-fun 'html-decode:script)
(defun html-decode:script (tag dest)
  (if (search-forward "</script>" html-decode-point html-decode-source t)
      (setq html-decode-point (match-end))
    (error "<SCRIPT> doesn't end: %s, %s"
	   html-decode-source html-decode-point)))


;; Tags in frameset section

(put 'frame 'html-decode-frameset-fun 'html-decode:frame)
(defun html-decode:frame (tag dest)
  (when (html-decode-tag-entering-p tag)
    (let*
	((name (cdr (assq 'name (nthcdr 2 tag))))
	 (url (cdr (assq 'src (nthcdr 2 tag)))))
      (html-decode-add-pending 'line)
      (html-decode-insert "  " dest)
      (with-buffer dest
	(make-extent (prog1 (cursor-pos)
		       (html-decode-insert
			(format nil "Frame: %s" (or name url)) dest))
		     (cursor-pos)
		     (list 'face (html-decode-elt-face 'link)
			   'html-anchor-params (list (cons 'href url)))))
      (html-decode-add-pending 'line))))

(put 'noframes 'html-decode-frameset-fun 'html-decode:body)
(put 'noframes 'html-decode-body-fun 'html-decode:body)
