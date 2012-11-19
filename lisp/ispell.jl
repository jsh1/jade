;;;; ispell.jl -- Interface with ispell(1)
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
;;  * replace the use of recursive-edit and catch/throw by a keymap
;;    in the minibuffer view for interactive checking. This will allow
;;    multiple buffers to be checked concurrently

(provide 'ispell)


;; Configuration and variables

(defvar ispell-program "ispell"
  "Filename of program used to start ispell(1).")

(defvar ispell-options nil
  "List of options to pass to Ispell")

(defvar ispell-dictionary nil
  "Name of dictionary to pass to Ispell, or nil for the default.")

(defvar ispell-timeout 5
  "Seconds to wait for ispell output before giving up.")

(defvar ispell-word-re "[a-zA-Z]([a-zA-Z']*[a-zA-Z])?"
  "Regexp matching a region of text that can be spell-checked using Ispell,
i.e. a single word.")

(defface ispell-misspelt-face "Face used to highlight misspelt words."
  (set-face-attribute ispell-misspelt-face 'underline t)
  (set-face-attribute ispell-misspelt-face 'foreground "#700000"))

(defvar ispell-ignore-word-hook nil
  "Hook called as (FUNCTION WORD START END) before WORD is to be spell-checked,
if the hook (`or' style) returns t the word is assumed to be correct.")
(make-variable-buffer-local 'ispell-ignore-word-hook)

(defvar ispell-echo-output nil
  "Use for debugging only.")

(defvar ispell-process nil
  "Subprocess that ispell is running in, or nil if ispell isn't running.")

(defvar ispell-process-busy nil
  "When t, the ispell-process is being used to check a word, but not all
results have been received.")

(defvar ispell-id-string nil
  "String sent by ispell identifying itself when it started executing.")

(defvar ispell-pending-output nil
  "String of output received from ispell but not processed.")

(defvar ispell-line-callback nil
  "Function to call asynchronously with a single line of output from ispell.")

(defvar ispell-prompt-buffer nil)
(defvar ispell-options-buffer nil)

;; [CHANGES PAGE-START PAGE-END]
(defvar ispell-minor-mode-last-scan nil)
(make-variable-buffer-local 'ispell-minor-mode-last-scan)

(defvar ispell-keymap (bind-keys (make-sparse-keymap)
			"SPC" 'ispell-accept
			"0" '(ispell-replace 0)
			"1" '(ispell-replace 1)
			"2" '(ispell-replace 2)
			"3" '(ispell-replace 3)
			"4" '(ispell-replace 4)
			"5" '(ispell-replace 5)
			"6" '(ispell-replace 6)
			"7" '(ispell-replace 7)
			"8" '(ispell-replace 8)
			"9" '(ispell-replace 9)
			"r" 'ispell-replace
			"a" 'ispell-accept-and-remember
			"i" 'ispell-accept-and-always-remember
			"l" 'ispell-lookup
			"u" 'ispell-accept-and-always-remember-uncap
			"q" 'ispell-quit
			"x" 'ispell-quit
			"ESC" 'ispell-quit
			"Ctrl-g" 'ispell-quit
			"?" 'ispell-help
			"Ctrl-h" 'ispell-help))


;; Process management

;; Function to buffer output from Ispell
(defun ispell-output-filter (output)
  (when (integerp output)
    (setq output (make-string 1 output)))
  (and ispell-echo-output
       (stringp output)
       (let
	   ((print-escape t))
	 (format (stderr-file) "Ispell: %S\n" output)))
  (setq ispell-pending-output (concat ispell-pending-output output))
  (while (and ispell-line-callback
	      ispell-pending-output
	      (string-match "\n" ispell-pending-output))
    (let
	((line (substring ispell-pending-output 0 (match-end))))
      (setq ispell-pending-output (substring ispell-pending-output
					     (match-end)))
      (ispell-line-callback line))))

;; Start the ispell-process if it isn't already
(defun ispell-start-process ()
  (unless ispell-process
    (let
	((process (make-process ispell-output-filter))
	 (sentinel (lambda ()
		     (setq ispell-process nil)
		     (setq ispell-id-string nil))))
      (set-process-function process sentinel)
      ;; Use a pty if possible. This allow EOF to be sent via ^D
      (set-process-connection-type process 'pty)
      (apply start-process process ispell-program "-a"
	     (nconc (and ispell-dictionary
			 (list "-d" ispell-dictionary))
		    ispell-options))
      (setq ispell-process process)
      (setq ispell-pending-output nil)
      (setq ispell-line-callback nil)
      (setq ispell-id-string (ispell-read-line))
      (unless (string-match "ispell version" ispell-id-string 0 t)
	(ispell-kill-process)
	(error "Ispell: %s" ispell-id-string)))))

(defun ispell-kill-process ()
  "Kill any subprocesses being used internally to run Ispell."
  (interactive)
  (accept-process-output 0)		;in case the process already died
  (when ispell-process
    (ispell-save-dictionary)
    (if (eq (process-connection-type ispell-process) 'pty)
	(write ispell-process ?\^D)
      ;; Not so successful..
      (interrupt-process ispell-process))
    (let
	((counter 0))
      (while (and (accept-process-output ispell-timeout) ispell-process)
	(if (< counter 2)
	    (interrupt-process ispell-process)
	  (kill-process ispell-process))
	(setq counter (1+ counter))))))

;; Read one whole line from the ispell-process (including newline)
(defun ispell-read-line ()
  (let*
      ((ispell-read-line-out nil)
       (ispell-line-callback (lambda (l)
			       (setq ispell-read-line-out l)
			       ;; Only want the first line
			       (setq ispell-line-callback nil))))
    ;; Flush any pending output
    (ispell-output-filter nil)
    (while (and (not ispell-read-line-out)
		ispell-process
		(not (accept-process-output ispell-timeout))))
    (or ispell-read-line-out
	(error "Ispell timed out waiting for output"))))

;; put in the before-exit-hook
(defun ispell-cleanup-before-exit ()
  (when ispell-process
    (ispell-kill-process)))

(add-hook 'before-exit-hook ispell-cleanup-before-exit)

;; Arbitrate access to the Ispell process, the mutex must be obtained
;; before sending a command that generates output. An error is signalled
;; if the process is busy
(defun ispell-mutex (grab)
  (if grab
      (if ispell-process-busy
	  (error "Ispell process is busy!")
	(ispell-start-process)
	(setq ispell-process-busy t))
    (setq ispell-process-busy nil)))

;; Check a word with Ispell. Returns the raw (single-line) output
(defun ispell-check-word (word)
  (let
      (response tem)
    (ispell-mutex t)
    (unwind-protect
	(progn
	  (format ispell-process "%s\n" word)
	  (setq response (ispell-read-line))
	  (if (eq (aref response 0) ?\n)
	      ;; This can happen when multi-language text is checked
	      (setq response "*\n\n")
	    ;; Gobble following blank line
	    (setq tem (ispell-read-line))
	    (unless (eq (aref tem 0) ?\n)
	      (error "Non-null trailing line from Ispell"))))
      (ispell-mutex nil))
    response))


;; Commands to interactively spell-check parts of a buffer

(defun ispell-region-1 (function start end)
  (while (< start end)
    (let
	(w-start w-end word response)
      (setq w-start (re-search-forward ispell-word-re start))
      (if (and w-start (<= (setq w-end (match-end)) end))
	  (if (and ispell-ignore-word-hook
		   (call-hook ispell-ignore-word-hook
			      (list word w-start w-end) 'or))
	      (setq start w-end)
	    (setq word (copy-area w-start w-end))
	    (setq response (ispell-check-word word))
	    (if (string-looking-at "^[*+-]" response)
		;; Word spelt ok
		(setq start w-end)
	      ;; Not ok
	      (setq start (function word response w-start w-end))))
	;; Can't find word
	(setq start end)))))

;;;###autoload
(defun ispell-region (start end)
  "Run Ispell interactively over the region of the current buffer from START
to END. Any misspelt words will result in the correct spelling being prompted
for. When called interactively, spell-check the current block."
  (interactive "-m\nM")
  (let
      (ispell-prompt-buffer
       ispell-options-buffer)
    (ispell-region-1 ispell-handle-failure-interactively start end)
    (when ispell-options-buffer
      (kill-buffer ispell-options-buffer))))

;;;###autoload
(defun ispell-buffer ()
  "Run the ispell-region command over the entire buffer."
  (interactive)
  (ispell-region (start-of-buffer) (end-of-buffer)))

(defun ispell-handle-failure-interactively (word response start end)
  (let
      ((old-buffer (current-buffer))
       options word-extent)
    (when (string-looking-at "^[&?].*: " response)
      (let
	  ((point (match-end)))
	(while (string-looking-at " *([^,\n]+),?" response point)
	  (setq options (cons (substring response
					 (match-start 1) (match-end 1))
			      options))
	  (setq point (match-end)))
	(setq options (nreverse options))))
    (unless ispell-options-buffer
      (setq ispell-options-buffer (make-buffer "*Ispell-options*")))
    (with-buffer ispell-options-buffer
      (let
	  ((inhibit-read-only t))
	(clear-buffer)
	(insert ispell-id-string)
	(insert "\n")
	(insert (with-buffer old-buffer
		  (copy-area (start-of-line start) (end-of-line end))))
	(make-extent (pos (pos-col start) 2)
		     (pos (pos-col end) 2)
		     (list 'face highlight-face))
	(insert "\n\n")
	(if options
	    (let
		((i 0)
		 (tem options))
	      (while tem
		(format (current-buffer) "%2d: %s\n" i (car tem))
		(setq i (1+ i))
		(setq tem (cdr tem))))
	  (insert "[No options]\n"))
	(setq read-only t)))
    (unless ispell-prompt-buffer
      (setq ispell-prompt-buffer (make-buffer "*Ispell-prompt*"))
      (with-buffer ispell-prompt-buffer
	(insert "[SP] <number> R\)epl A\)ccept I\)nsert L\)ookup U\)ncap Q\)uit e\(X\)it or ? for help")
	(setq local-keymap 'ispell-keymap)
	(setq read-only t)))
    (goto start)
    (setq word-extent (make-extent start end (list 'face highlight-face)))
    (unwind-protect
	(progn
	  (with-view (other-view)
	    (goto-buffer ispell-options-buffer)
	    (shrink-view-if-larger-than-buffer))
	  (with-view (minibuffer-view)
	    (with-buffer ispell-prompt-buffer
	      (let
		  ((done nil)
		   command)
		(while (not done)
		  (setq command (catch 'ispell-exit
				  (recursive-edit)))
		  (cond
		   ((eq (car command) 'accept)
		    (when (cdr command)
		      (write ispell-process (cdr command))
		      (write ispell-process word)
		      (write ispell-process ?\n))
		    (setq done t))
		   ((eq (car command) 'replace)
		    (setq done t)
		    (if (integerp (cdr command))
			(with-buffer old-buffer
			  (setq end (replace-string word
						    (ispell-strip-word
						     (nth (cdr command)
							  options))
						    start)))
		      (let
			  ((string (prompt-for-string "Replace with:" word)))
			(if string
			    (setq end (replace-string word string start))
			  (setq done nil)))))
		   ((eq (car command) 'quit)
		    (setq done t)
		    (setq end (end-of-buffer old-buffer)))
		   (t
		    (error "Unknown ispell command, %S" command))))))))
      (when word-extent
	(delete-extent word-extent)))
    end))


;; Support functions for interactive spell checking

;; Given a derivation WORD, produce the resulting replacement
(defun ispell-strip-word (word)
  (let
      ((out "")
       (point 0))
    (when (string-looking-at "([^+-]+)\\+" word point)
      ;; [prefix+]
      (setq out (substring word (match-start 1) (match-end 1)))
      (setq point (match-end)))
    (when (string-looking-at "([^+-]+)([+-]|$)" word point)
      ;; root
      (setq out (concat out (substring word (match-start 1) (match-end 1))))
      (setq point (match-end 1)))
    (while (string-looking-at "-([^+-]+)" word point)
      ;; [-prefix] | [-suffix]
      (let*
	  ((phrase (substring word (match-start 1) (match-end 1)))
	   (quoted (quote-regexp phrase)))
	(setq point (match-end))
	(cond
	 ((string-match (concat quoted ?$) out)
	  (setq out (substring out 0 (match-start))))
	 ((string-match (concat ?^ quoted) out)
	  (setq out (substring out (match-end))))
	 (t
	  ;; not a prefix or a suffix, just copy to the output
	  (setq out (concat out ?- phrase))))))
    (when (string-looking-at "\\+([^+-]+)" word point)
      (setq out (concat out (substring word (match-start 1) (match-end 1)))))
    out))

(defun ispell-accept ()
  (interactive)
  (throw 'ispell-exit '(accept)))

(defun ispell-accept-and-remember ()
  (interactive)
  (throw 'ispell-exit '(accept . ?@)))

(defun ispell-accept-and-always-remember ()
  (interactive)
  (throw 'ispell-exit '(accept . ?*)))

(defun ispell-accept-and-always-remember-uncap ()
  (interactive)
  (throw 'ispell-exit '(accept . ?&)))

(defun ispell-replace (arg)
  (interactive "P")
  (throw 'ispell-exit (cons 'replace
			    (if (consp arg)
				(prefix-numeric-argument arg)
			      arg))))

(defun ispell-quit ()
  (interactive)
  (throw 'ispell-exit '(quit)))


;; Non-interactive spell-checking

(defvar ispell-misspelt nil
  "Set to t in extents marking misspelt words.")

;; Add the minor mode name..
(setq minor-mode-alist (cons (list 'ispell-minor-mode-last-scan " Ispell")
			     minor-mode-alist))

;; ..and keymap.
(setq minor-mode-keymap-alist (cons '(ispell-misspelt . ispell-minor-keymap)
				    minor-mode-keymap-alist))

(defvar ispell-minor-keymap (bind-keys (make-sparse-keymap)
			      "LMB-Click2" 'ispell-misspelt-word
			      "C-c" 'ispell-minor-c-c-keymap))
(defvar ispell-minor-c-c-keymap (bind-keys (make-sparse-keymap)
				  "RET" 'ispell-misspelt-word
				  "C-a" 'ispell-add-word-to-dictionary
				  "C-s" 'ispell-add-word-for-session))

(defvar ispell-minor-menus '(("Ispell word" ispell-misspelt-word)
			     ("Add to dictionary"
			     ispell-add-word-to-dictionary)
			     ("Add for session" ispell-add-word-for-session)))

(defun ispell-delete-highlights (start end)
  (interactive (if (blockp)
		   (list (block-start) (block-end))
		 (list (start-of-buffer) (end-of-buffer))))
  (let
      (extents)
    (map-extents (lambda (e)
		   (when (eq (extent-get e 'face) ispell-misspelt-face)
		     (setq extents (cons e extents)))) start end)
    (mapc delete-extent extents)))

;; Returns the end of the checked region
;;;###autoload
(defun ispell-highlight-misspellings (start end #!optional abort-on-input)
  "Highlight misspellings in the region between START and END. If
ABORT-ON-INPUT is non-nil, any input arriving will preempt the spell checking.
This function returns the position of the end of the actually checked region.

When called interactively, the region is either the marked block, or the
whole of the buffer (if no block)."
  (interactive (if (blockp)
		   (list (block-start) (block-end))
		 (list (start-of-buffer) (end-of-buffer))))
  (let
      ((failure-fun (lambda (word response wstart wend)
		      (declare (unused word response))
		      (let ((e (make-extent
				wstart wend
				(list 'face ispell-misspelt-face
				      'mouse-face active-face))))
			(extent-set e 'ispell-misspelt t)
			(extent-put e 'popup-menus ispell-minor-menus)
			wend))))
    (if (not abort-on-input)
	;; Just scan the whole thing in one chunk
	(progn
	  (ispell-delete-highlights start end)
	  (ispell-region-1 failure-fun start end)
	  end)
      ;; Scan a line at a time, checking if input is pending
      (catch 'abort
	(while (and start (< start end))
	  (let
	      ((this-end (end-of-line start)))
	    (ispell-delete-highlights start this-end)
	    (ispell-region-1 failure-fun start this-end)
	    (setq start (forward-char 1 this-end)))
	  (unless (sit-for 0)
	    ;; Returns nil when the timeout didn't complete, i.e. if
	    ;; any input arrived
	    (throw 'abort start)))
	end))))

;; Called from the idle-hook
(defun ispell-idle-function ()
  (when ispell-minor-mode-last-scan
    (let
	((start (display-to-char-pos '(0 . 0)))
	 (end (view-dimensions)))
      (setq end (display-to-char-pos (pos (1- (car end)) (1- (cdr end)))))
      (when (or (null end)
		(> end (end-of-buffer)))
	(setq end (end-of-buffer)))
      (if (> (buffer-changes) (aref ispell-minor-mode-last-scan 0))
	  ;; Rescan entirely
	  (progn
	    (ispell-delete-highlights (start-of-buffer) (end-of-buffer))
	    (setq end (ispell-highlight-misspellings start end t))
	    (aset ispell-minor-mode-last-scan 0 (buffer-changes))
	    (aset ispell-minor-mode-last-scan 1 start)
	    (aset ispell-minor-mode-last-scan 2 end))
	;; No changes, so just rescan the bits of the current page
	;; that aren't already scanned
	(let
	    ((old-start (aref ispell-minor-mode-last-scan 1))
	     (old-end (aref ispell-minor-mode-last-scan 2)))
	  (cond
	   ((< start old-start)
	    ;; Extend upwards to start
	    (setq end (min end old-start))
	    (setq end (ispell-highlight-misspellings start end t))
	    (aset ispell-minor-mode-last-scan 1 start)
	    (aset ispell-minor-mode-last-scan 2 end))
	   ((> end old-end)
	    ;; Extend downwards to end
	    (setq start (max start old-end))
	    (setq end (ispell-highlight-misspellings start end t))
	    (aset ispell-minor-mode-last-scan 1 start)
	    (aset ispell-minor-mode-last-scan 2 end))))))))

;;;###autoload
(defun ispell-minor-mode ()
  "Minor mode enabling misspellings in the current buffer to be highlighted
automatically. When the no input arrives for more than a second, the current
page is spell-checked, any unknown words are displayed in the
`ispell-misspelt-face'.

Each misspelling also has a set of extra commands, which are activated when
the cursor is placed in a misspelt word; they are,

\\{ispell-minor-keymap}"
  (interactive)
  (if ispell-minor-mode-last-scan
      (progn
	(setq ispell-minor-mode-last-scan nil)
	(ispell-delete-highlights (start-of-buffer) (end-of-buffer))
	(remove-hook 'idle-hook 'ispell-idle-function))
    (setq ispell-minor-mode-last-scan (vector 0 nil nil))
    (make-local-variable 'idle-hook)
    (add-hook 'idle-hook ispell-idle-function)))

(defun ispell-invalidate-past-scans ()
  (mapc (lambda (b)
	  (with-buffer b
	    (when ispell-minor-mode-last-scan
	      (aset ispell-minor-mode-last-scan 0 (1- (buffer-changes))))))
	(buffer-list)))

;; Return the string of the misspelt word under point, or nil
(defun ispell-get-misspelt-word ()
  (let
      ((e (get-extent)))
    (while (and e (not (eq (buffer-symbol-value 'ispell-misspelt e nil t) t)))
      (setq e (extent-parent e)))
    (and e (copy-area (extent-start e) (extent-end e)))))

(defun ispell-misspelt-word ()
  "Interactively run Ispell over the misspelt word that is highlighted under
the cursor."
  (interactive)
  (let
      ((e (get-extent)))
    (while (and e (not (eq (buffer-symbol-value 'ispell-misspelt e nil t) t)))
      (setq e (extent-parent e)))
    (or e (error "No misspelling here!"))
    (ispell-region (extent-start e) (extent-end e))))


;; Dictionary management

;;;###autoload
(defun ispell-set-dictionary (dict-name)
  "Set the name of the dictionary used by Ispell to DICT-NAME."
  (interactive "sName of dictionary:")
  (setq ispell-dictionary dict-name)
  (when ispell-process
    (ispell-kill-process)
    (ispell-start-process))
  (ispell-invalidate-past-scans))

;;;###autoload
(defun ispell-add-word-to-dictionary (word)
  "Add the string WORD to your personal Ispell dictionary."
  (interactive (list (prompt-for-string "Word to add to dictionary:"
					(or (ispell-get-misspelt-word)
					    (symbol-at-point)))))
  (ispell-start-process)
  (format ispell-process "*%s\n" word)
  (ispell-invalidate-past-scans))

;;;###autoload
(defun ispell-add-word-for-session (word)
  "Add the string WORD to Ispell's per-session dictionary."
  (interactive (list (prompt-for-string "Word to add for session:"
					(or (ispell-get-misspelt-word)
					    (symbol-at-point)))))
  (ispell-start-process)
  (format ispell-process "@%s\n" word)
  (ispell-invalidate-past-scans))

(defun ispell-save-dictionary ()
  "Make Ispell save the current personal dictionary to its file."
  (interactive)
  (when ispell-process
    (write ispell-process "#\n")))
