;;;; latin-1.jl -- Make the default glyph-table show Latin 1 chars
;;;  Copyright (C) 1994 John Harper <john@dcs.warwick.ac.uk>
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

(provide 'latin-1)

(defvar use-latin-1 nil
  "Non-nil when the Latin-1 character set is being used.")

(defvar latin-1-char-alist
  ;; These are from the X11 keysymdef.h header file
  '((space . ?\x20) (exclam . ?\x21) (quotedbl . ?\x22) (numbersign . ?\x23)
    (dollar . ?\x24) (percent . ?\x25) (ampersand . ?\x26) (apostrophe . ?\x27)
    (quoteright . ?\x27) (parenleft . ?\x28) (parenright . ?\x29)
    (asterisk . ?\x2a) (plus . ?\x2b) (comma . ?\x2c) (minus . ?\x2d)
    (period . ?\x2e) (slash . ?\x2f) (colon . ?\x3a) (semicolon . ?\x3b)
    (less . ?\x3c) (equal . ?\x3d) (greater . ?\x3e) (question . ?\x3f)
    (at . ?\x40) (bracketleft . ?\x5b) (backslash . ?\x5c)
    (bracketright . ?\x5d) (asciicircum . ?\x5e) (underscore . ?\x5f)
    (grave . ?\x60) (quoteleft . ?\x60) (braceleft . ?\x7b) (bar . ?\x7c)
    (braceright . ?\x7d) (asciitilde . ?\x7e) (nobreakspace . ?\xa0)
    (exclamdown . ?\xa1) (cent . ?\xa2) (sterling . ?\xa3) (currency . ?\xa4)
    (yen . ?\xa5) (brokenbar . ?\xa6) (section . ?\xa7) (diaeresis . ?\xa8)
    (copyright . ?\xa9) (ordfeminine . ?\xaa) (guillemotleft . ?\xab)
    (notsign . ?\xac) (hyphen . ?\xad) (registered . ?\xae) (macron . ?\xaf)
    (degree . ?\xb0) (plusminus . ?\xb1) (twosuperior . ?\xb2)
    (threesuperior . ?\xb3) (acute . ?\xb4) (mu . ?\xb5) (paragraph . ?\xb6)
    (periodcentered . ?\xb7) (cedilla . ?\xb8) (onesuperior . ?\xb9)
    (masculine . ?\xba) (guillemotright . ?\xbb) (onequarter . ?\xbc)
    (onehalf . ?\xbd) (threequarters . ?\xbe) (questiondown . ?\xbf)
    (Agrave . ?\xc0) (Aacute . ?\xc1) (Acircumflex . ?\xc2)
    (Atilde . ?\xc3) (Adiaeresis . ?\xc4) (Aring . ?\xc5) (AE . ?\xc6)
    (Ccedilla . ?\xc7) (Egrave . ?\xc8) (Eacute . ?\xc9) (Ecircumflex . ?\xca)
    (Ediaeresis . ?\xcb) (Igrave . ?\xcc) (Iacute . ?\xcd)
    (Icircumflex . ?\xce) (Idiaeresis . ?\xcf) (ETH . ?\xd0) (Eth . ?\xd0)
    (Ntilde . ?\xd1) (Ograve . ?\xd2) (Oacute . ?\xd3) (Ocircumflex . ?\xd4)
    (Otilde . ?\xd5) (Odiaeresis . ?\xd6) (multiply . ?\xd7) (Ooblique . ?\xd8)
    (Ugrave . ?\xd9) (Uacute . ?\xda) (Ucircumflex . ?\xdb)
    (Udiaeresis . ?\xdc) (Yacute . ?\xdd) (THORN . ?\xde) (Thorn . ?\xde)
    (ssharp . ?\xdf) (agrave . ?\xe0) (aacute . ?\xe1) (acircumflex . ?\xe2)
    (atilde . ?\xe3) (adiaeresis . ?\xe4) (aring . ?\xe5) (ae . ?\xe6)
    (ccedilla . ?\xe7) (egrave . ?\xe8) (eacute . ?\xe9) (ecircumflex . ?\xea)
    (ediaeresis . ?\xeb) (igrave . ?\xec) (iacute . ?\xed)
    (icircumflex . ?\xee) (idiaeresis . ?\xef) (eth . ?\xf0) (ntilde . ?\xf1)
    (ograve . ?\xf2) (oacute . ?\xf3) (ocircumflex . ?\xf4) (otilde . ?\xf5)
    (odiaeresis . ?\xf6) (division . ?\xf7) (oslash . ?\xf8) (ugrave . ?\xf9)
    (uacute . ?\xfa) (ucircumflex . ?\xfb) (udiaeresis . ?\xfc)
    (yacute . ?\xfd) (thorn . ?\xfe) (ydiaeresis . ?\xff))
  "Alist of (SYMBOL . LATIN-1-CHAR) defining all non-alphanumeric characters
in the Latin 1 character set.")

;;;###autoload
(defun latin-1-mode ()
  "Toggles whether or not the characters with numeric values from 160 to 256
are displayed as octal escape sequences or in the Latin-1 character set."
  (interactive)
  (let
      ((i 160))
    (setq use-latin-1 (not use-latin-1))
    (while (< i 256)
      (set-glyph (default-glyph-table) i (if use-latin-1
					     (make-string 1 i)
					   (format nil "\\%o" i)))
      (setq i (1+ i))))
  use-latin-1)

;;;###autoload
(defun insert-latin-1-character (symbol count)
  "Insert COUNT copies of the Latin 1 character named by the symbol SYMBOL.
When called interactively, SYMBOL is prompted for, COUNT is taken from the
prefix argument."
  (interactive
   (let
       ((arg current-prefix-arg))
     (list (intern (prompt-from-list (mapcar #'(lambda (x)
						 (symbol-name (car x)))
					     latin-1-char-alist)
				     "Latin 1 character:"))
	   (prefix-numeric-argument arg))))
  (let
      ((char (cdr (or (assoc symbol latin-1-char-alist)
		      (error "Unknown Latin 1 character: %s" symbol)))))
    (insert (make-string count char))))
