;;;; mime-base64.jl -- Encoder/decoder for base64 encoding
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

(provide 'mime-base64)

(defvar mime-base64-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun mime-encode-base64 (input output)
  (let
      ((col 0)
       reg reg1 reg2 reg3)
    (catch 'done
      (while t
	(setq reg1 (read-char input))
	(setq reg2 (read-char input))
	(setq reg3 (read-char input))
	(cond
	 ((and reg1 reg2 reg3)
	  ;; Got our 24 bits, split into four 6 bit quantities
	  (progn
	    (setq reg (logior (lsh reg1 16) (lsh reg2 8) reg3))
	    (write output (aref mime-base64-alphabet (lsh reg -18)))
	    (write output (aref mime-base64-alphabet
				(logand (lsh reg -12) 077)))
	    (write output (aref mime-base64-alphabet
				(logand (lsh reg -6) 077)))
	    (write output (aref mime-base64-alphabet (logand reg 077)))
	    (setq col (+ col 4))
	    (when (>= col 76)
	      (write output ?\n)
	      (setq col 0))))
	 (reg2
	  ;; 16 bits read, shift in 2 zeros
	  (setq reg (lsh (logior (lsh reg1 8) reg2) 2))
	  (write output (aref mime-base64-alphabet (lsh reg -12)))
	  (write output (aref mime-base64-alphabet
			      (logand (lsh reg -6) 077)))
	  (write output (aref mime-base64-alphabet (logand reg 077)))
	  (write output ?=)
	  (throw 'done t))
	 (reg1
	  ;; eight bits read, shift in 4 zeros
	  (setq reg (lsh reg1 4))
	  (write output (aref mime-base64-alphabet (lsh reg -6)))
	  (write output (aref mime-base64-alphabet (logand reg 077)))
	  (write output ?=)
	  (write output ?=)
	  (throw 'done t))
	 (t
	  ;; 0 bits read
	  (throw 'done t)))))
    (write output ?\n)))

(defun mime-decode-base64 (input output)
  (let
      ((reg 0)
       (bits 0)
       char)
    (while (setq char (read-char input))
      (cond
       ((and (>= char ?A) (<= char ?Z))
	(setq char (- char ?A)))
       ((and (>= char ?a) (<= char ?z))
	(setq char (+ 26 (- char ?a))))
       ((and (>= char ?0) (<= char ?9))
	(setq char (+ 52 (- char ?0))))
       ((= char ?+)
	(setq char 62))
       ((= char ?/)
	(setq char 63))
       (t
	(setq char nil)))
      (when char
	(setq reg (logior (lsh reg 6) char))
	(setq bits (+ bits 6)))
      (while (> bits 8)
	(setq char (lsh reg (- 8 bits)))
	(setq reg (logxor reg (lsh char (- bits 8))))
	(setq bits (- bits 8))
	(write output char)))))
