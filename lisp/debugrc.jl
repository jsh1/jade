;;;; debugrc.jl -- Type commands down stdin
;;;  Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
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

(while t
  (condition-case error-data
      (progn
	(write (stdout-file) "jade> ")
	(flush-file (stdout-file))
	(format (stdout-file) " => %S\n" (eval (read (stdin-file)))))
    (end-of-stream
     (throw 'quit 0))
    (error
     (format (stdout-file) "error--> %S\n" error-data))))
