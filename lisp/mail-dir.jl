;;;; mail-dir.jl -- Directory of known email addresses and aliases
;;;  Copyright (C) 1997 John Harper <john@dcs.warwick.ac.uk>
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

(require 'maildefs)
(provide 'mail-dir)


;; User configuration

(defvar mail-directory-file "~/.mail-directory"
  "The file storing the mail directory.")

(defvar mail-directory-modified nil
  "t when the mail directory has been modified since it was loaded.")

(defvar mail-dir-scan-messages t
  "When t addresses will be taken from all read mail messages.")

(defvar mail-dir-prompt-when-scanning t
  "When t any unknown addresses seen in messages will not be added to the
directory without confirmation from the user.")

(defvar mail-address-list nil
  "List of records. Each record is a list (KEY VALUES...) where KEY is a
symbol (currently used keys are :net, :net-alias and :name for email
addresses, email aliases, and real names respectively). All VALUES should
be strings.")

(defvar mail-dir-load-on-init t
  "When t the mail directory stored in mail-directory-file is read when
mail-dir.jl is loaded.")


;; Storing directories in files

;;;###autoload
(defun load-user-mail-directory ()
  "Load the user's default mail-directory"
  (interactive)
  (load-mail-directory mail-directory-file))

;;;###autoload
(defun load-mail-directory (file &optional dont-merge)
  "Load email directory from FILE. Unless DONT-MERGE is t, the new
definitions will be added to any existing definitions."
  (interactive (list (prompt-for-file "Mail directory file" t
				      mail-directory-file)
		     current-prefix-arg))
  (let
      ((file-handle (open-file file 'read))
       form tem)
    (if dont-merge
	(setq mail-address-list nil
	      mail-directory-modified nil)
      (when mail-address-list
	(setq mail-directory-modified t)))
    (when file-handle
      (unwind-protect
	  (condition-case nil
	      (while t
		(setq form (read file-handle))
		(cond
		 ((null form))
		 ((eq (car form) 'mail-dir)
		  ;; List of mail addresses
		  (if dont-merge
		      (setq mail-address-list (cdr form))
		    ;; FIXME: remove duplicates
		    (setq mail-address-list (nconc (cdr form)
						   mail-address-list))))
		 (t
		  (message (format nil "Ignoring unknown item in mail dir: %s"
				   (car form))))))
	    (end-of-stream))
	(close-file file-handle)))))

;; Output a LIST of objects to STREAM. Each object printed will be indented
;; by two spaces.
(defun mail-dir-output-list (stream lst)
  (while (consp lst)
    (format stream "\n  %S" (car lst))
    (setq lst (cdr lst))))

(defun save-mail-directory (file)
  "Save the current contents of the mail directory to the file FILE."
  (interactive (list (prompt-for-file "File to save mail directory to"
				      nil mail-directory-file)))
  (let
      ((file-handle (open-file file 'write)))
    (when file-handle
      (unwind-protect
	  (progn
	    (format file-handle ";; Mail directory\n;; Created by %s on %s for %s\n\n"
		    jade-version (current-time-string) (user-login-name))
	    (write file-handle "\(mail-dir")
	    (mail-dir-output-list file-handle mail-address-list)
	    (write file-handle "\)\n"))
	(close-file file-handle))
      (setq mail-directory-modified nil))))


;; Low-level mail-address functions

(defmacro md-get-field (record field)
  `(cdr (assq ,field ,record)))

(defun md-add-to-field (record field value)
  (let
      ((cell (assq field record)))
    (if cell
	(unless (member value (cdr cell))
	  (rplacd cell (nconc (cdr cell) (list value)))
	  (setq mail-directory-modified t))
      (setq record (nconc record (list (list field value))))
      (setq mail-directory-modified t))))

(defun md-delete-from-field (record field value)
  (let
      ((cell (assq field record)))
    (when cell
      (rplacd cell (delete value (cdr cell))))))

(defun md-delete-field (record field)
  (mapc #'(lambda (cell)
	    (when (eq (car cell) field)
	      ;; Not possible to delete the whole thing. So just
	      ;; clear out the field's contents
	      (rplacd cell nil))) record))

(defun md-get-record (field key)
  (catch 'out
    (mapc #'(lambda (r)
	      (when (member key (md-get-field r field))
		(throw 'out r))) mail-address-list)))

(defun md-add-record (field value)
  (let
      ((record (list (list field value))))
    (setq mail-address-list (nconc mail-address-list (list record)))
    (setq mail-directory-modified t)
    record))

(defun md-delete-record (record)
  (setq mail-address-list (delq record mail-address-list))
  (setq mail-directory-modified t))

;; Return a flattened list of all fields matching FIELD
(defun md-get-all-fields (field &optional required)
  (apply append (mapcar #'(lambda (r)
			    (when (or (not required)
				      (md-get-field r required))
			      (md-get-field r field)))
			mail-address-list)))

(defun md-field-exists-p (key-field key field)
  (let
      ((record (md-get-record key-field key)))
    (and record (md-get-field record field))))


;; Prompt variants

(defun prompt-for-mail-address (title &optional dont-validate initial)
  (prompt-from-list (md-get-all-fields ':net) title initial dont-validate))

(defun prompt-for-mail-item (title &optional dont-validate initial)
  (prompt-from-list (md-get-all-fields ':name) title initial dont-validate))

(defun prompt-for-mail-full-name (title &optional dont-validate initial)
  (prompt-from-list (md-get-all-fields ':name ':net)
		    title initial dont-validate))

(defun prompt-for-mail-alias (title &optional dont-validate initial)
  (prompt-from-list (md-get-all-fields ':name ':net-alias)
		    title initial dont-validate))

(defun prompt-for-address-list (title &optional dont-validate)
  (let
      ((lst '())
       tem)
    (while (setq tem (prompt-for-mail-address (concat title
						      " (Ctrl-g to finish)")
					      dont-validate))
      (setq lst (cons tem lst)))
    (nreverse lst)))


;; Adding and removing entries

;;;###autoload
(defun add-mail-address (address full-name)
  "Add the address ADDRESS to the mail directory, as the address of the
entity called FULL-NAME."
  (interactive (let
		   ((addr (prompt-for-mail-address "Address to add" t)))
		 (list addr (prompt-for-mail-full-name (concat "Set address "
							       addr " to")
						       t))))
  (unless (and address full-name)
    (error "Null argument"))
  (let
      ((record (or (md-get-record ':net address)
		   (md-get-record ':name full-name))))
    (if record
	(md-add-to-field record ':net address)
      (setq record (md-add-record ':net address)))
    (md-add-to-field record ':name full-name)
    (format t "Added address of %s as <%s>" full-name address)))

;;;###autoload
(defun add-mail-alias (alias address-list)
  (interactive (let
		   ((alias (prompt-for-mail-alias "Alias to add" t)))
		 (list alias (prompt-for-address-list (concat "Set alias "
							alias " to:") t))))
  (unless (and alias address-list)
    (error "Null argument"))
  (let
      ((record (md-get-record ':name alias)))
    (if record
	(unless (y-or-n-p (concat "Alias " alias
				  " already exists; add to it?"))
	  (md-delete-field record ':net-alias))
      (setq record (md-add-record ':name alias)))
    (md-add-to-field record ':net-alias address-list)
    (setq mail-directory-modified t)))

;;;###autoload
(defun remove-mail-item (name)
  "Remove mail address NAME from the mail directory."
  (interactive (list (prompt-for-mail-address "Name to delete" t)))
  (let
      ((record (md-get-record ':name name)))
    (when record
      (md-delete-record record)
      (format t "Removed mail item <%s>" name))))


;; Accessor functions

;;;###autoload
(defun get-mail-address (full-name &optional do-print)
  "Return the mail address of the entity called FULL-NAME (a string). When
PRINT is t the address is also displayed in the message area."
  (interactive (list (prompt-for-mail-full-name "Name") t))
  (let
      ((record (md-get-record ':name full-name))
       field)
    (unless record
      (error "Address for %s doesn't exist" full-name))
    (setq field (md-get-field record ':net))
    (when do-print
      (format t "Address of %s is %S" full-name field))
    (car field)))

;;;###autoload
(defun get-mail-name-from-address (address &optional do-print)
  "Return the name of the entity whose email address is ADDRESS. When PRINT
is t the address is also displayed in the message area."
  (interactive (list (prompt-for-mail-address "Address") t))
  (let
      ((record (md-get-record ':net address))
       field)
    (unless record
      (error "Address doesn't exist: %s" address))
    (setq field (md-get-field record ':name))
    (when do-print
      (format t "Name of <%s> is %s" address (car field)))
    (car field)))

;;;###autoload
(defun get-mail-alias (alias &optional do-print)
  "Return the expansion of mail alias ALIAS (a list of addresses). When
PRINT is t the expansion is also displayed in the message area."
  (interactive (list (prompt-for-mail-alias "Alias") t))
  (let
      ((record (md-get-record ':name alias))
       field)
    (unless record
      (error "Alias doesn't exist: %s" alias))
    (setq field (md-get-field record ':net-alias))
    (when do-print
      (format t "Alias %s expands to %s" alias field))
    field))

;;;###autoload
(defun insert-mail-item (name)
  "Insert a mail address/name pair into the buffer for the address of the
entity NAME."
  (interactive (list (prompt-for-mail-item "Name:")))
  (let
      ((record (or (md-get-record ':name name)
		   (md-get-record ':net name)))
       field)
    (unless record
      (error "Record doesn't exist: %s" name))
    (require 'mail-headers)
    (cond
     ((setq field (md-get-field record ':net))
      (goto (insert (mail-format-address (car field)
					 (or (car (md-get-field record ':name))
					     name)))))
     ((setq field (md-get-field record ':net-alias))
      (mail-insert-address-list
       (mapcar #'(lambda (a)
		   (condition-case nil
		       (cons a (get-mail-name-from-address a))
		     (error
		      (cons a nil)))) field)))
     (t
      (error "Nothing to insert in record: %s" record)))))


;; Snarfing address/name combinations from messages

(defun mail-dir-scan-function (address full-name &aux item)
  "Assuming that a mail/news message has been received from email address
ADDRESS, from an entity called FULL-NAME, take appropriate action for the
mail directory. Also see the variables `mail-dir-scan-messages' and
`mail-dir-prompt-when-scanning'."
  (when mail-dir-scan-messages
    (let
	(record)
      (cond
       ((setq record (md-get-record ':net address))
	(md-add-to-field record ':name full-name))
       ((setq record (md-get-record ':name full-name))
	(md-add-to-field record ':net address))
       ((or (not mail-dir-prompt-when-scanning)
	    (y-or-n-p (concat "Add " full-name " <"
			      address "> to directory?")))
	(add-mail-address address full-name))))))


;; Initialisation

(when (and mail-dir-load-on-init
	   (file-readable-p mail-directory-file))
  (load-mail-directory mail-directory-file))

(add-hook 'before-exit-hook
	  #'(lambda ()
	      (when (and mail-directory-modified
			 (y-or-n-p "Mail directory modified; save it?"))
		(save-mail-directory mail-directory-file))))
