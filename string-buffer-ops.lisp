;;;; -*- Mode: Lisp; Package: RACEHORSE -*-

(in-package racehorse)

(defun list-contains-only-strings-p (list)
  (loop for o in list
	with found-non-string = nil
	when (not (typep o 'string))
	  do (setf found-non-string t)
	finally (return-from list-contains-only-strings-p (and list (not found-non-string)))))

(defun printed-length (object)
  (etypecase object
    (integer (let ((baselength (case (abs object)
				 (0 1)
				 (1 1)
				 (t (multiple-value-bind (len)
					(ceiling (log (abs object) 10))
				      len)))))
	       (if (minusp object)
		   (1+ baselength)
		   baselength)))
    ;; what about flonums?!
    (character 1)
    (null 4)
    (list (+ 2 (loop named count ; doesnt quite work right yet
		     for elt in object
		     with spacelength = (1- (length object))
		     sum (printed-length elt) into eltlength
		     finally (return-from count (+ eltlength spacelength)))))
    (symbol (length (symbol-name object)))
    (string (length object))))

(defun print-to-string-buffer (thing &key (function #'princ))
  (let ((buf (claim-string-buffer (printed-length thing))))
    (with-output-to-string (s buf)
      (funcall function thing s))
    buf))

(defun process-into-strings (things)
  (loop named process-strings
	for thing in things
	as string = (etypecase thing
		      (string thing)
		      (null "NULL")
		      (number (print-to-string-buffer thing))
		      (character (print-to-string-buffer thing))
		      (symbol (print-to-string-buffer thing)))
	sum (length string) into maxlen
	collect string into strings
	finally (return-from process-strings (values strings maxlen))))

(defun release-string-buffers (strings)
  (map nil #'release-string-buffer strings))

(defun string-append (&rest things)
  "Append THINGS into a single resourced string.  If any string in THINGS is resourced, release it.
If any of the THINGS are actually symbols or numbers, process them as if using PRINC-TO-STRING
Make sure caller releases the string returned by STRING-APPEND when it is done with the string."
  (multiple-value-bind (strings maxlen)
      (if (list-contains-only-strings-p things)
	  (loop named count
		for s in things
		sum (length s) into maxlen
		finally (return-from count (values things maxlen)))
	  (process-into-strings things))
    (let ((bigstring (claim-string-buffer maxlen)))
      (loop for s in strings
	    with bigi = 0
	    do (loop for stringi from 0 below (length s)
		     do (setf (aref bigstring bigi) (aref s stringi))
		        (incf bigi))
	    finally (setf (fill-pointer bigstring) bigi))
      (release-string-buffers strings)
      bigstring)))
