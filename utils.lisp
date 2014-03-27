;;;; -*- Mode: Lisp; Package: KIWI -*-

(in-package racehorse)

(defun swap (array i j)
  (let ((oldi (aref array i)))
    (setf (aref array i) (aref array j)
	  (aref array j) oldi))
  array)

(defun apply-when (fcn &rest args)
  (when (and fcn (functionp fcn))
    (apply fcn args)))

(defun print-hash-table (hash-table)
  (maphash #'(lambda (key value) (terpri) (format t "~S: ~S~%" key value)) hash-table))



