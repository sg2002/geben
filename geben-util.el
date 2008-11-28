;;--------------------------------------------------------------
;;  utilities
;;--------------------------------------------------------------

(require 'cl)

(defsubst geben-flatten (x)
  "Make cons X to a flat list."
  (flet ((rec (x acc)
	      (cond ((null x) acc)
		    ((atom x) (cons x acc))
		    (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defsubst geben-what-line (&optional pos)
  "Get the number of the line in which POS is located.
If POS is omitted, then the current position is used."
  (save-restriction
    (widen)
    (save-excursion
      (if pos (goto-char pos))
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defsubst geben-plist-push (plist prop value)
  (let ((l (plist-get plist prop)))
    (cond
     ((consp l)
      (plist-put plist prop
		 (cons value (plist-get plist prop))))
     ((null l)
      (setf plist (plist-put plist prop (list value))))
     (t
      (error "geben-plist-push: cannot add value; type of prop `%s' is not `list' but `%s'."
	     prop (type-of value))))))

(defsubst geben-plist-append (plist prop value)
  (let ((l (plist-get plist prop)))
    (cond
     ((consp l)
      (nconc l (list value)))
     ((null l)
      (setf plist (plist-put plist prop (list value))))
     (t
      (error "geben-plist-add: cannot add value; type of prop `%s' is not `list' but `%s'."
	     prop (type-of value))))))

(defmacro geben-lexical-bind (bindings &rest body)
  (declare (indent 1))
  (cl-macroexpand-all
   (nconc
    (list 'lexical-let (mapcar (lambda (arg)
				 (list arg arg))
			       bindings))
    body)))
			      
(defun geben-remove-directory-tree (basedir)
  (ignore-errors
    (mapc (lambda (path)
	    (cond
	     ((or (file-symlink-p path)
		  (file-regular-p path))
	      (delete-file path))
	     ((file-directory-p path)
	      (let ((name (file-name-nondirectory path)))
		(or (equal "." name)
		    (equal ".." name)
		    (geben-remove-directory-tree path))))))
	  (directory-files basedir t nil t))
    (delete-directory basedir)))
  
;;--------------------------------------------------------------
;;  cross emacs overlay definitions
;;--------------------------------------------------------------

(eval-and-compile
  (and (featurep 'xemacs)
       (require 'overlay))
  (or (fboundp 'overlay-livep)
      (defalias 'overlay-livep 'overlay-buffer)))

(defun geben-overlay-make-line (lineno &optional buf)
  "Create a whole line overlay."
  (with-current-buffer (or buf (current-buffer))
    (save-excursion
      (widen)
      (goto-line lineno)
      (beginning-of-line)
      (make-overlay (point)
		    (save-excursion
		      (forward-line) (point))
		    nil t nil))))

(provide 'geben-util)
