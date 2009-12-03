;;; new-subdirs-el.el --- Special subdirs.el to go along with write-subdirs-el

;; Copyright (C) 2000 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Keywords: lisp, internal

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;tehom-normal-top-level-add-subdirs-to-load-path's behavior is
;;different than normal-top-level-add-subdirs-to-load-path.  It
;;assumes directories that have their own subdirs.el are handled by
;;that subdirs.el and therefore doesn't explore them.  It also assumes
;;that the absence or presence of .nosearch indicates whether such a
;;directory-tree's root should be part of load-path or not.

;;; Code:

(defun tehom-normal-top-level-add-subdirs-to-load-path ()
  "Add subdirectories of current directory to `load-path', recursively.

More precisely, this uses only the subdirectories whose names
start with letters or digits; it excludes any subdirectory named `RCS'
or `CVS', and any subdirectory that contains a file named `.nosearch'.
Any subdirectory that contains `subdirs.el' will run that instead,
regardless whether `.nosearch' is present.

tehom-normal-top-level-add-subdirs-to-load-path is a variant of
normal-top-level-add-subdirs-to-load-path."
  (let (dirs 
	 attrs
	 (pending (list default-directory)))
    ;; This loop does a breadth-first tree walk on DIR's subtree,
    ;; putting each subdir into DIRS as its contents are examined.
    (while pending
      (setq dirs (cons (car pending) dirs))
      (setq pending (cdr pending))
      (setq attrs (nthcdr 10 (file-attributes (car dirs))))
      (let ((contents (directory-files (car dirs)))
	     (default-directory (car dirs)))
	(unless (member attrs normal-top-level-add-subdirs-inode-list)
	  (setq normal-top-level-add-subdirs-inode-list
	    (cons attrs normal-top-level-add-subdirs-inode-list))
	  (while contents
	    (unless (member (car contents) '("." ".." "RCS" "CVS"))
	      (when 
		(and 
		  (string-match "\\`[a-zA-Z0-9]" (car contents))
		  ;; Avoid doing a `stat' when it isn't necessary
		  ;; because that can cause trouble when an NFS server
		  ;; is down.
		  (not (string-match "\\.elc?\\'" (car contents)))
		  (file-directory-p (car contents)))
		(let*
		  (
		    (expanded (expand-file-name (car contents)))
		    (subdirs-filename 
		      (expand-file-name "subdirs.el" expanded)))
		  (cond
		    ;;If we see ".nosearch", don't explore.  But if
		    ;;we also see subdirs.el, execute it now.
		    ((file-exists-p 
		       (expand-file-name ".nosearch" expanded))
		      (if (file-exists-p subdirs-filename)
			(load subdirs-filename t t t)))
		      
		    ;;If we see subdirs.el, it will do the work, so
		    ;;don't explore.  Just put the directory in
		    ;;load-path, which will run subdirs.el later.
		    ((file-exists-p subdirs-filename)
		      (setq dirs 
			(cons expanded dirs)))
		      
		    ;;Otherwise we will explore the directory later.
		    (t
		      (setq pending 
			(nconc pending (list expanded))))))))
	    
	    (setq contents (cdr contents))))))
    (normal-top-level-add-to-load-path (cdr (nreverse dirs)))))

;;A little safer than the old normal-top-level-add-to-load-path, for
;;when the current directory isn't in load-path.
(defun normal-top-level-add-to-load-path (dirs)
  (let ((tail load-path)
	(thisdir (directory-file-name default-directory)))
    (while (and 
	     tail
	     (cdr tail)  ;;Don't go all the way to the nil terminator.
	     (not (equal thisdir (car tail)))
	     (not (and (memq system-type '(ms-dos windows-nt))
		    (equal (downcase thisdir) (downcase (car tail))))))
      (setq tail (cdr tail)))
    ;;Splice the new section in.
    (if tail
      (setcdr tail 
	(append (mapcar 'expand-file-name dirs) (cdr tail))))))

;;Execute it here.
(tehom-normal-top-level-add-subdirs-to-load-path)

;;; new-subdirs-el.el ends here

