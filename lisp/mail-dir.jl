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

(defvar mail-dir-auto-expand-aliases t
  "When t any mail aliases in mail messages being sent will be expanded.
Note that this variable only has an effect when sm-mail-dir.jl is first
loaded.")

(defvar mail-address-alist nil
  "Alist of (EMAIL-ADDRESS . REAL-NAME).")

(defvar mail-alias-alist nil
  "Alist of (EMAIL-ALIAS EMAIL-ADDRESSES...).")

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
       form list tem)
    (if dont-merge
	(setq mail-address-alist nil
	      mail-alias-alist nil
	      mail-directory-modified nil)
      (when (or mail-address-alist mail-alias-alist)
	(setq mail-directory-modified t)))
    (when file-handle
      (unwind-protect
	  (condition-case nil
	      (while t
		(setq form (read file-handle))
		(cond
		 ((null form))
		 ((eq (car form) 'address-alist)
		  ;; List of mail addresses
		  (setq list 'mail-address-alist))
		 ((eq (car form) 'alias-alist)
		  ;; List of mail aliases
		  (setq list 'mail-alias-alist))
		 (t
		  (error "Unknown item in mail directory: %s" (car form))))
		(when list
		  (setq form (nreverse (cdr form)))
		  (while (consp form)
		    (when (or dont-merge
			      (null (assoc (car (car form))
					   (symbol-value list))))
		      (set list (cons (car form) (symbol-value list))))
		    (setq form (cdr form)))))
	    (end-of-stream))
	(close-file file-handle)))))

;; Output a LIST of objects to STREAM. Each object printed will be indented
;; by two spaces.
(defun mail-dir-output-list (stream list)
  (while (consp list)
    (format stream "\n  %S" (car list))
    (setq list (cdr list))))

(defun save-mail-directory (file)
  "Save the current contents of the mail directory to the file FILE."
  (interactive (list (prompt-for-file "File to save mail directory to"
				      nil mail-directory-file)))
  (let
      ((file-handle (open-file file 'write))
       list)
    (when file-handle
      (unwind-protect
	  (progn
	    (format file-handle ";; Mail directory\n;; Created by %s on %s for %s\n\n"
		    (version-string) (current-time-string) (user-login-name))
	    (write file-handle ";; Address list\n\(address-alist")
	    (mail-dir-output-list file-handle mail-address-alist)
	    (write file-handle "\)\n\n;; Alias list\n\(alias-alist")
	    (mail-dir-output-list file-handle mail-alias-alist)
	    (write file-handle "\)\n"))
	(close-file file-handle))
      (setq mail-directory-modified nil))))


;; Prompt variants

;;;###autoload
(defun prompt-for-mail-alias (prompt &optional dont-validate initial)
  (prompt-from-list (mapcar 'car mail-alias-alist) prompt
		    initial dont-validate))

;;;###autoload
(defun prompt-for-mail-address (prompt &optional dont-validate initial)
  (prompt-from-list (mapcar 'car mail-address-alist) prompt
		    initial dont-validate))

;;;###autoload
(defun prompt-for-mail-full-name (prompt &optional dont-validate initial)
  (prompt-from-list (mapcar 'cdr mail-address-alist) prompt
		    initial dont-validate))

;;;###autoload
(defun prompt-for-address-list (prompt &optional dont-validate)
  (let
      ((list '())
       tem)
    (while (setq tem (prompt-for-mail-address (concat prompt
						      " (Ctrl-g to finish)")
					      dont-validate))
      (setq list (cons tem list)))
    (nreverse list)))


;; Adding and removing entries

;;;###autoload
(defun add-mail-alias (alias address-list)
  (interactive (let
		   ((alias (prompt-for-mail-alias "Alias to add" t)))
		 (list alias (prompt-for-address-list (concat "Set alias "
							alias " to:") t))))
  (let
      ((current (assoc alias mail-alias-alist)))
    (if current
	(if (y-or-n-p (concat "Alias " alias " already exists; add to it?"))
	    (rplacd current (append address-list (cdr current)))
	  (rplacd current address-list))
      (setq mail-alias-alist (cons (cons alias address-list) mail-alias-alist)))
    (setq mail-directory-modified t)))

;;;###autoload
(defun remove-mail-alias (alias)
  "Remove the mail alias ALIAS from the mail directory."
  (interactive (list (prompt-for-mail-alias mail-alias-alist
					    "Alias to delete")))
  (setq mail-alias-alist (delete-if #'(lambda (x)
					(string= (car x) alias))
				    mail-alias-alist)
	mail-directory-modified t))

;;;###autoload
(defun add-mail-address (address full-name)
  "Add the address ADDRESS to the mail directory, as the address of the
entity called FULL-NAME."
  (interactive (let
		   ((addr (prompt-for-mail-address "Address to add" t)))
		 (list addr (prompt-for-mail-full-name (concat "Set address "
							       addr " to")
						       t))))
  (let
      ((current (assoc address mail-address-alist)))
    (if current
	(rplacd current full-name)
      (setq mail-address-alist (cons (cons address full-name)
				     mail-address-alist)))
    (setq mail-directory-modified t)
    (format t "Added address of %s as <%s>" full-name address)))

;;;###autoload
(defun remove-mail-address (address)
  "Remove mail address ADDRESS from the mail directory."
  (interactive (list (prompt-for-mail-address "Address to delete" t)))
  (setq mail-address-alist (delete-if #'(lambda (x)
					  (string= (car x) address))
				      mail-address-alist)
	mail-directory-modified t)
  (format t "Removed mail address <%s>" address))


;; Accessor functions

;;;###autoload
(defun get-mail-alias (alias &optional print)
  "Return the expansion of mail alias ALIAS (a list of addresses). When
PRINT is t the expansion is also displayed in the message area."
  (interactive (list (prompt-for-mail-alias "Alias") t))
  (let
      ((body (assoc alias mail-alias-alist)))
    (unless body
      (error "Alias doesn't exist: %s" alias))
    (when print
      (format t "Alias %s expands to %s" alias (cdr body)))
    (cdr body)))

;;;###autoload
(defun get-mail-address (full-name &optional print)
  "Return the mail address of the entity called FULL-NAME (a string). When
PRINT is t the address is also displayed in the message area."
  (interactive (list (prompt-for-mail-full-name "Name") t))
  (let
      ((body (rassoc full-name mail-address-alist)))
    (unless body
      (error "Address for %s doesn't exist" full-name))
    (when print
      (format t "Address of %s is <%s>" full-name (car body)))
    (car body)))

;;;###autoload
(defun get-mail-name-from-address (address &optional print)
  "Return the name of the entity whose email address is ADDRESS. When PRINT
is t the address is also displayed in the message area."
  (interactive (list (prompt-for-mail-address "Address") t))
  (let
      ((body (assoc address mail-address-alist)))
    (unless body
      (error "Address doesn't exist: %s" address))
    (when print
      (format t "Name of <%s> is %s" address (cdr body)))
    (car body)))

(defun insert-mail-address-and-name (name)
  "Insert a mail address/name pair into the buffer for the address of the
entity NAME."
  (interactive (list (prompt-for-mail-full-name "Name:")))
  (let
      ((addr (get-mail-address name)))
    (goto (insert (mail-format-address addr name)))))

(defun insert-mail-alias (name)
  "Insert the expansion of alias NAME into the buffer."
  (interactive (list (prompt-for-mail-alias "Alias:")))
  (let
      ((alias (get-mail-alias name)))
    (mail-insert-list (mapcar #'(lambda (item &aux pair)
				  (setq pair (assoc item mail-address-alist))
				  (if pair
				      (mail-format-address (car pair)
							   (cdr pair))
				    item))
			      alias))))


;; Snarfing address/name combinations from messages

(defun mail-dir-scan-function (address full-name &aux item)
  "Assuming that a mail/news message has been received from email address
ADDRESS, from an entity called FULL-NAME, take appropriate action for the
mail directory. Also see the variables `mail-dir-scan-messages' and
`mail-dir-prompt-when-scanning'."
  (when mail-dir-scan-messages
    (cond
     ((assoc address mail-address-alist))	;address already known
     ((setq item (rassoc full-name mail-address-alist))
      ;; Change of address for FULL-NAME?
      (when (y-or-n-p (concat "Address of " full-name " changed to "
			      address "; update directory?"))
	(rplaca item address)
	(setq mail-directory-modified t)))
     (t
      ;; An unknown address
      (when (or (not mail-dir-prompt-when-scanning)
		(y-or-n-p (concat "Add " full-name " <" address
				  "> to directory?")))
	(add-mail-address address full-name))))))


;; Initialisation

(when (and mail-dir-load-on-init
	   (file-readable-p mail-directory-file))
  (load-mail-directory mail-directory-file))

(add-hook 'before-exit-hook #'(lambda ()
				(when (and mail-directory-modified
					   (y-or-n-p "Mail directory modified; save it?"))
				  (save-mail-directory mail-directory-file))))
