;;;; -*- Mode: Lisp; Package: RACEHORSE -*-

(in-package racehorse)

(defparameter *string-buffer-size-table* (make-array '(0 3) :adjustable t :element-type t))

(defun register-string-buffer-resource (size fixed adjustable)
  (let ((new (array-dimension *string-buffer-size-table* 0)))
    (adjust-array *string-buffer-size-table* (list (1+ new) 3))
    (setf (aref *string-buffer-size-table* new 0) size)
    (setf (aref *string-buffer-size-table* new 1) fixed)
    (setf (aref *string-buffer-size-table* new 2) adjustable)))

(defun find-string-buffer-resource-for-size (size adjustable)
  (loop for i from 0
	with max = (array-dimension *string-buffer-size-table* 0)
	when (= i max)
	  do (error "Requested string buffer resource size of ~D is greater than the largest defined string buffer resource of ~D."
		    size (aref *string-buffer-size-table* (1- i) 0))
	when (<= size (aref *string-buffer-size-table* i 0))
	  return (aref *string-buffer-size-table* i (if adjustable 2 1))))

(defun claim-optimal-string-buffer (maxlen adjustable)
  (let ((resource (find-string-buffer-resource-for-size maxlen adjustable)))
    (claim-resourced-object resource)))

(defun resource-for-string-buffer (string)
  (if (adjustable-array-p string)
      (lookup-adjustable-string-buffer string)
      (let ((res (find-string-buffer-resource-for-size (array-dimension string 0) nil)))
	(if (eq (resourced-object-state res string)
		:not-resourced)
	    nil
	    res))))

(defun release-string-buffer (string)
  (when (typep string 'string)
    (let ((res (resource-for-string-buffer string)))
      (when res
	(release-resourced-object res string)))))

(defun make-string-buffer (length &key (adjustable nil)) ; name deprecated in favor of CLAIM-STRING-BUFFER
  (claim-optimal-string-buffer length adjustable))

(defun claim-string-buffer (length &key (adjustable nil))
  (claim-optimal-string-buffer length adjustable))

;; record the adjustable strings in a special table so that we know for sure which resource they belong to.
;; can't just use the size lookup like with fixed strings because adjustable strings can grow beyond the next-size-up and fool the lookup.

(defparameter *adjustable-string-buffer-lookup-table* (make-hash-table :test 'eq))

(defun add-to-adjustable-string-buffer-table (buffer resource)
  (setf (gethash buffer *adjustable-string-buffer-lookup-table*) resource)
  t)

(defun remove-from-adjustable-string-buffer-table (buffer)
  (remhash buffer *adjustable-string-buffer-lookup-table*)
  t)

(defun lookup-adjustable-string-buffer (buffer)
  (gethash buffer *adjustable-string-buffer-lookup-table*))

(defun define-string-buffer-1 (size fixed adjustable)
  (let ((fixed (find-resource fixed))
	(adjustable (find-resource adjustable)))
    (register-string-buffer-resource size fixed adjustable)
    ;; WTF is the purpose of this setf?  doesn't the equivalent form in DEFINE-STRING-BUFFER do enough??
    (setf (resource-constructor adjustable)
	  (compile nil `(lambda () (let ((b (make-array ,size :element-type 'character :fill-pointer 0 :adjustable t)))
				     (add-to-adjustable-string-buffer-table b ,adjustable)
				     b))))))

(defmacro define-string-buffer (size &key (fixed nil) (adjustable nil) (initial-copies 0) (soft-limit nil) (hard-limit nil))
  `(eval-when (:load-toplevel :execute)
     (defresource (,fixed
		   :initial-copies ,initial-copies
		   :soft-limit ,soft-limit
		   :hard-limit ,hard-limit)
	 :constructor #'(lambda () (make-array ,size :element-type 'character :fill-pointer 0 :adjustable nil))
	 :reinitializer #'(lambda (b) (setf (fill-pointer b) 0)))
     (defresource (,adjustable
		   :initial-copies ,initial-copies
		   :soft-limit ,soft-limit
		   :hard-limit ,hard-limit)
	 :constructor #'(lambda () (let ((b (make-array ,size :element-type 'character :fill-pointer 0 :adjustable t)))
				     (add-to-adjustable-string-buffer-table b ',adjustable)
				     b))
	 :deallocation-hook #'(lambda (b) (remove-from-adjustable-string-buffer-table b))
	 :reinitializer #'(lambda (b) (setf (fill-pointer b) 0)))
    (define-string-buffer-1 ,size ',fixed ',adjustable)
    ,size))

;;; String buffer definitions -- these must be in monotonically increasing order.

(define-string-buffer (* 1 1024)
    :fixed :1k-string-buffer
    :adjustable :1k-adjustable-string-buffer
    :initial-copies 10
    :soft-limit nil
    :hard-limit nil)

(define-string-buffer (* 25 1024)
    :fixed :25k-string-buffer
    :adjustable :25k-adjustable-string-buffer
    :initial-copies 5
    :soft-limit nil
    :hard-limit nil)

(define-string-buffer (* 50 1024)
    :fixed :50k-string-buffer
    :adjustable :50k-adjustable-string-buffer
    :initial-copies 0
    :soft-limit nil
    :hard-limit nil)

(define-string-buffer (* 64 1024)
    :fixed :64k-string-buffer
    :adjustable :64k-adjustable-string-buffer
    :initial-copies 0
    :soft-limit nil
    :hard-limit nil)

(define-string-buffer (* 100 1024)
    :fixed :100k-string-buffer
    :adjustable :100k-adjustable-string-buffer
    :initial-copies 0
    :soft-limit nil
    :hard-limit nil)

(define-string-buffer (* 250 1024)
    :fixed :250k-string-buffer
    :adjustable :250k-adjustable-string-buffer
    :initial-copies 0
    :soft-limit nil
    :hard-limit nil)

(define-string-buffer (* 500 1024)
    :fixed :500k-string-buffer
    :adjustable :500k-adjustable-string-buffer
    :initial-copies 0
    :soft-limit nil
    :hard-limit nil)

(define-string-buffer (* 1024 1024)
    :fixed :1024k-string-buffer 
    :adjustable :1024k-adjustable-string-buffer
    :initial-copies 0
    :soft-limit nil
    :hard-limit nil)

