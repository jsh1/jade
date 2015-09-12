;;;; mime-quoted-printable.jl -- Encoder/decoder for quoted-printable encoding
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

(provide 'mime-quoted-printable)

;;;###autoload
(defun mime-encode-quoted-printable (input output)
  (let
      ((col 0)
       char)
    ;; XXX: trailing TAB and SPC chars should be encoded..
    (while (setq char (read-char input))
      (when (>= col 76)
	(write output "=\n")
	(setq col 0))
      (cond ((or (and (>= char 33) (<= char 60))
		 (and (>= char 62) (<= char 126))
		 (= char #\space)
		 (= char #\tab))
	     ;; null encoding
	     (setq col (1+ col))
	     (write output char))
	    ((= char #\newline)
	     (setq col 0)
	     (write output char))
	    (t
	     ;; Encode using =HH format
	     (format output "=%02X" char)
	     (setq col (+ col 3)))))))

;;;###autoload
(defun mime-decode-quoted-printable (input output)
  (let
      (char tem)
    (while (setq char (read-char input))
      (if (/= char #\=)
	  (write output char)
	(or (setq char (read-char input))
	    (error "Malformed quoted-printable data"))
	(unless (= char #\newline)
	  (or (setq tem (read-char input))
	      (error "Malformed quoted-printable data"))
	  (setq char (char-upcase char))
	  (setq tem (char-upcase tem))
	  (if (and (>= char #\0) (<= char #\9))
	      (setq char (- char #\0))
	    (setq char (+ (- char #\A) 10)))
	  (if (and (>= tem #\0) (<= tem #\9))
	      (setq tem (- tem #\0))
	    (setq tem (+ (- tem #\A) 10)))
	  (write output (logior (ash char 4) tem)))))))
