;;;; -*- Mode: Lisp; Package: RACEHORSE -*-

(in-package racehorse)

(defparameter *octet-buffer-size-table* (make-array '(0 3) :adjustable t :element-type t))

(defun register-octet-buffer-resource (size fixed adjustable)
  (let ((new (array-dimension *octet-buffer-size-table* 0)))
    (adjust-array *octet-buffer-size-table* (list (1+ new) 3))
    (setf (aref *octet-buffer-size-table* new 0) size)
    (setf (aref *octet-buffer-size-table* new 1) fixed)
    (setf (aref *octet-buffer-size-table* new 2) adjustable)))

(defun find-octet-buffer-resource-for-size (size adjustable)
  (loop for i from 0
	with max = (array-dimension *octet-buffer-size-table* 0)
	when (= i max)
	  do (error "Requested octet buffer resource size of ~D is greater than the largest defined octet buffer resource of ~D."
		    size (aref *octet-buffer-size-table* (1- i) 0))
	when (<= size (aref *octet-buffer-size-table* i 0))
	  return (aref *octet-buffer-size-table* i (if adjustable 2 1))))

(defun claim-optimal-octet-buffer (maxlen adjustable)
  (let ((resource (find-octet-buffer-resource-for-size maxlen adjustable)))
    (claim-resourced-object resource)))

(defun resource-for-octet-buffer (octet-buffer)
  (if (adjustable-array-p octet-buffer)
      (lookup-adjustable-octet-buffer octet-buffer)
      (let ((res (find-octet-buffer-resource-for-size (array-dimension octet-buffer 0) nil)))
	(if (eq (resourced-object-state res octet-buffer)
		:not-resourced)
	    nil
	    res))))

(defun release-octet-buffer (octet-buffer)
  (when (typep octet-buffer 'array)
    (let ((res (resource-for-octet-buffer octet-buffer)))
      (when res
	(release-resourced-object res octet-buffer)))))

(defun make-octet-buffer (length &key (adjustable nil)) ; name deprecated in favor of CLAIM-OCTET-BUFFER
  (claim-optimal-octet-buffer length adjustable))

(defun claim-octet-buffer (length &key (adjustable nil))
  (claim-optimal-octet-buffer length adjustable))

;; record the adjustable strings in a special table so that we know for sure which resource they belong to.
;; can't just use the size lookup like with fixed strings because adjustable strings can grow beyond the next-size-up and fool the lookup.

(defparameter *adjustable-octet-buffer-lookup-table* (make-hash-table :test 'eq))

(defun add-to-adjustable-octet-buffer-table (buffer resource)
  (setf (gethash buffer *adjustable-octet-buffer-lookup-table*) resource)
  t)

(defun remove-from-adjustable-octet-buffer-table (buffer)
  (remhash buffer *adjustable-octet-buffer-lookup-table*)
  t)

(defun lookup-adjustable-octet-buffer (buffer)
  (gethash buffer *adjustable-octet-buffer-lookup-table*))

(defun define-octet-buffer-1 (size fixed adjustable)
  (let ((fixed (find-resource fixed))
	(adjustable (find-resource adjustable)))
    (register-octet-buffer-resource size fixed adjustable)
    ;; WTF is the purpose of this setf?  doesn't the equivalent form in DEFINE-OCTET-BUFFER do enough??
    (setf (resource-constructor adjustable)
	  (compile nil `(lambda () (let ((b (make-array ,size :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
				     (add-to-adjustable-octet-buffer-table b ,adjustable)
				     b))))))

(defmacro define-octet-buffer (size &key (fixed nil) (adjustable nil) (initial-copies 0) (soft-limit nil) (hard-limit nil))
  `(eval-when (:load-toplevel :execute)
     (defresource (,fixed
		   :initial-copies ,initial-copies
		   :soft-limit ,soft-limit
		   :hard-limit ,hard-limit)
	 :constructor #'(lambda () (make-array ,size :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable nil))
	 :reinitializer #'(lambda (b) (setf (fill-pointer b) 0)))
     (defresource (,adjustable
		   :initial-copies ,initial-copies
		   :soft-limit ,soft-limit
		   :hard-limit ,hard-limit)
	 :constructor #'(lambda () (let ((b (make-array ,size :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
				     (add-to-adjustable-octet-buffer-table b ',adjustable)
				     b))
	 :deallocation-hook #'(lambda (b) (remove-from-adjustable-octet-buffer-table b))
	 :reinitializer #'(lambda (b) (setf (fill-pointer b) 0)))
    (define-octet-buffer-1 ,size ',fixed ',adjustable)
    ,size))

;;; Octet buffer definitions -- these must be in monotonically increasing order.

(define-octet-buffer (* 1 1024)
    :fixed :1k-octet-buffer
    :adjustable :1k-adjustable-octet-buffer
    :initial-copies 10
    :soft-limit nil
    :hard-limit nil)

(define-octet-buffer (* 25 1024)
    :fixed :25k-octet-buffer
    :adjustable :25k-adjustable-octet-buffer
    :initial-copies 5
    :soft-limit nil
    :hard-limit nil)

(define-octet-buffer (* 50 1024)
    :fixed :50k-octet-buffer
    :adjustable :50k-adjustable-octet-buffer
    :initial-copies 0
    :soft-limit nil
    :hard-limit nil)

(define-octet-buffer (* 100 1024)
    :fixed :100k-octet-buffer
    :adjustable :100k-adjustable-octet-buffer
    :initial-copies 0
    :soft-limit nil
    :hard-limit nil)

(define-octet-buffer (* 250 1024)
    :fixed :250k-octet-buffer
    :adjustable :250k-adjustable-octet-buffer
    :initial-copies 0
    :soft-limit nil
    :hard-limit nil)

(define-octet-buffer (* 500 1024)
    :fixed :500k-octet-buffer
    :adjustable :500k-adjustable-octet-buffer
    :initial-copies 0
    :soft-limit nil
    :hard-limit nil)

(define-octet-buffer (* 1024 1024)
    :fixed :1024k-octet-buffer 
    :adjustable :1024k-adjustable-octet-buffer
    :initial-copies 0
    :soft-limit nil
    :hard-limit nil)

