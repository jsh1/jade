;;; Cyrillic minor mode based on locale settings.
;; Requires a locale-enabled Jade.
;; Author: Andrew Rodionoff <arnost@mail.ru>
;; Jade is Copyright (C) John Harper
;;
(require 'edit)
(require 'ispell)
(provide 'cyrillic)

(setq-default word-not-regexp "[^a-zA-Z0-9\x80-\xff]")
(setq-default word-regexp "[a-zA-Z0-9\x80-\xff]")
(setq-default ispell-word-re "[a-zA-Z\x80-\xff]([a-zA-Z\x80-\xff']*[a-zA-Z\x80-\xff])?")

(setq upcase-table
      (let    
	  ((i 0)
	   (tab (make-string 256)))
	(while (< i (length tab))
	  (aset tab i (char-upcase i))
	  (setq i (1+ i)))
	tab))

(setq downcase-table
      (let    
	  ((i 0)
	   (tab (make-string 256)))
	(while (< i (length tab))
	  (aset tab i (char-downcase i))
	  (setq i (1+ i)))
	tab))
             
(let
    ((i 127))
  (while (< i 256)
    (set-glyph (default-glyph-table) i (make-string 1 i))
    (setq i (1+ i))))               

;(let
;    ((a nil)
;     (b nil))
;  (while (/= a "M-ESC")
;    (setq a (event-name (read-event)))
;    (setq b (event-name (read-event)))
;    (insert (format nil "(\"%s\" . \"%s\")\n" a b))))

(defvar jcuken-key-alist
      '(("q" . "Cyrillic_shorti")
	("w" . "Cyrillic_tse")
	("e" . "Cyrillic_u")
	("r" . "Cyrillic_ka")
	("t" . "Cyrillic_ie")
	("y" . "Cyrillic_en")
	("u" . "Cyrillic_ghe")
	("i" . "Cyrillic_sha")
	("o" . "Cyrillic_shcha")
	("p" . "Cyrillic_ze")
	("[" . "Cyrillic_ha")
	("]" . "Cyrillic_hardsign")
	("a" . "Cyrillic_ef")
	("s" . "Cyrillic_yeru")
	("d" . "Cyrillic_ve")
	("f" . "Cyrillic_a")
	("g" . "Cyrillic_pe")
	("h" . "Cyrillic_er")
	("j" . "Cyrillic_o")
	("k" . "Cyrillic_el")
	("l" . "Cyrillic_de")
	(";" . "Cyrillic_zhe")
	("'" . "Cyrillic_e")
	("z" . "Cyrillic_ya")
	("x" . "Cyrillic_che")
	("c" . "Cyrillic_es")
	("v" . "Cyrillic_em")
	("b" . "Cyrillic_i")
	("n" . "Cyrillic_te")
	("m" . "Cyrillic_softsign")
	("," . "Cyrillic_be")
	("." . "Cyrillic_yu")
	("Q" . "Cyrillic_SHORTI")
	("W" . "Cyrillic_TSE")
	("E" . "Cyrillic_U")
	("R" . "Cyrillic_KA")
	("T" . "Cyrillic_IE")
	("Y" . "Cyrillic_EN")
	("U" . "Cyrillic_GHE")
	("I" . "Cyrillic_SHA")
	("O" . "Cyrillic_SHCHA")
	("P" . "Cyrillic_ZE")
	("{" . "Cyrillic_HA")
	("}" . "Cyrillic_HARDSIGN")
	("A" . "Cyrillic_EF")
	("S" . "Cyrillic_YERU")
	("D" . "Cyrillic_VE")
	("F" . "Cyrillic_A")
	("G" . "Cyrillic_PE")
	("H" . "Cyrillic_ER")
	("J" . "Cyrillic_O")
	("K" . "Cyrillic_EL")
	("L" . "Cyrillic_DE")
	(":" . "Cyrillic_ZHE")
	("\"" . "Cyrillic_E")
	("Z" . "Cyrillic_YA")
	("X" . "Cyrillic_CHE")
	("C" . "Cyrillic_ES")
	("V" . "Cyrillic_EM")
	("B" . "Cyrillic_I")
	("N" . "Cyrillic_TE")
	("M" . "Cyrillic_SOFTSIGN")
	("<" . "Cyrillic_BE")
	(">" . "Cyrillic_YU")
	("/" . ".")
	("?" . ",")
	("@" . "\"")
	("~" . "?")))

(defvar cyr-mode-p nil
"Non-nil if cyr-mode is active")                
(make-variable-buffer-local 'cyr-mode-p)

(setq minor-mode-alist (cons '(cyr-mode-p " [RUS]") minor-mode-alist))
(setq minor-mode-keymap-alist (cons '(cyr-mode-p . cyr-mode-keymap)
				    minor-mode-keymap-alist))
(defvar cyr-mode-keymap
(let
    ((k (make-sparse-keymap)))
  (mapc 
   (lambda (x)
     (bind-keys k
      (car x)
      (eval `#'(lambda ()
	    (interactive)
	    (if (/= (buffer-name) "*isearch*")
		(insert ,(make-string 1 (car (lookup-event (cdr x)))))
					; Let's handle increment-search mode
	      (let
		  ((old-c-e-s current-event-string))
		(setq current-event-string (lambda ()
					     ,(make-string
					       1 (car (lookup-event (cdr x))))))
		(call-hook 'unbound-key-hook)
		(setq current-event-string old-c-e-s)))))))
   jcuken-key-alist)
  k)
"Cyrillic minor mode keymap")

(defun cyr-mode ()
"Toggles minor mode for entering cyrillic glyphs."
(interactive)
(if cyr-mode-p
    (setq cyr-mode-p nil)
  (setq cyr-mode-p t)))

(bind-keys global-keymap
  "C-\\" 'cyr-mode)

;;; FIXME:
;;  Redefinition of upcase and downcase tables doesn't seem to affect librep 
;;  string utility functions, so I have to define 'em once more. (A bug or
;;  feature??)

(defun string-downcase (x)
  "Return a new string, a lower case copy of string X."
  (translate-string (copy-sequence x) downcase-table))

(defun string-upcase (x)
  "Return a new string, an upper case copy of string X."
  (translate-string (copy-sequence x) upcase-table))
