;;;; -*- Mode: Lisp; Package: RACEHORSE -*-

(in-package racehorse)

;;; Exported interface

(defparameter *default-resource-hash-size* 100)
(defparameter *default-resource-rehash-size* 1.2)
(defparameter *default-resource-rehash-threshold* .64)
(defparameter *default-freelist-size* 4096)

(defmacro defresource ((name &key (initial-copies 0)
				  #+weak-data (weak-keys nil)
				  (size *default-resource-hash-size*)
				  (rehash-size *default-resource-rehash-size*)
				  (rehash-threshold *default-resource-rehash-threshold*)
				  (soft-limit nil)
				  (hard-limit nil)
				  (freelist-size *default-freelist-size*))
		       &rest resource-options)
  (let ((resource (gensym)))
    `(eval-when (:load-toplevel :execute)
       (let ((,resource (make-resource :name ',name
				       :pool (make-hash-table :test 'eq
							      #+weak-data :weak-keys #+weak-data ,weak-keys
							      :size ,size
							      :rehash-size ,rehash-size
							      :rehash-threshold ,rehash-threshold)
				       #+resource-annotations :annotations 
				       #+resource-annotations (make-hash-table :test 'eq
									       #+weak-data :weak-keys #+weak-data ,weak-keys
									       :size ,size
									       :rehash-size ,rehash-size
									       :rehash-threshold ,rehash-threshold)
				       :freelist (make-array ,freelist-size :element-type t :adjustable t :fill-pointer 0 
							     #+weak-data :weak #+weak-data ,weak-keys)
				       :soft-limit ,soft-limit
				       :hard-limit ,hard-limit
				       :lock (make-lock :name (format nil "Lock for ~A resource" ',name))
				       #+weak-data :weak-keys #+weak-data ,weak-keys
				       ,@resource-options)))
	 (setf (gethash ',name *resources*) ,resource)
	 (unless (zerop ,initial-copies)
	   (dotimes (i ,initial-copies)
	     (vector-push-extend (construct-resourced-object ,resource) (resource-freelist ,resource)))))
      ',name)))

(defmacro using-resource ((name resource-name) &body body)
  `(let ((,name))
     (unwind-protect
	 (progn (setf ,name (claim-resourced-object ',resource-name))
		,@body)
       (release-resourced-object ',resource-name ,name))))

(defun find-resource (name)
  (typecase name
    (symbol (or (gethash name *resources*)
		(error 'resource-does-not-exist :name name)))
    (resource name)))

(defmethod object-claimed-p ((resource symbol) (object t))
  (object-claimed-p (find-resource resource) object))

(defmethod object-claimed-p ((resource resource) (object t))
  (if (integerp (resourced-object-state resource object))
      t
      nil))

(defmethod claim-resourced-object ((resource symbol) &rest arguments)
  (apply #'claim-resourced-object (find-resource resource) arguments))

(defmethod claim-resourced-object ((resource resource) &rest arguments)
  (with-lock ((resource-lock resource))
    (incf (resource-claim-count resource))
    (activate-resourced-object resource
			       (aif (get-unclaimed-object resource)
				    (progn (apply #'reinitialize-resourced-object resource it arguments)
					   it)
				    (apply #'construct-resourced-object resource arguments)))))

(defmethod share-resourced-object ((resource symbol) (object t))
  (share-resourced-object (find-resource resource) object))

(defmethod share-resourced-object ((resource resource) (object t))
  (with-lock ((resource-lock resource))
    (incf (resourced-object-state resource object)))
  object)

(defmethod release-resourced-object ((resource symbol) (object t))
  (release-resourced-object (find-resource resource) object))

(defmethod release-resourced-object ((resource resource) (object t))
  (with-lock ((resource-lock resource))
    (let ((state (resourced-object-state resource object)))
      (etypecase state
	(symbol (ecase state
		  (:not-resourced (error 'object-not-owned-by-resource :resource resource :object object))
		  (:unclaimed     (error 'object-already-released :resource resource :object object))))
	(integer (if (> state 1)
		     (decf (resourced-object-state resource object))
		     (progn (deinitialize-resourced-object resource object)
			    (deactivate-resourced-object resource object)))))
      nil)))

(defmethod unresource-resourced-object ((resource symbol) (object t) &key (hold-lock t))
  (unresource-resourced-object (find-resource resource) object :hold-lock hold-lock))

(defmethod unresource-resourced-object ((resource resource) (object t) &key (hold-lock t))
  (flet ((unresource ()
	   (when (object-claimed-p resource object)
	     (release-resourced-object resource object))
	   (deallocate-resourced-object resource object)))
    (if hold-lock 
	(with-lock ((resource-lock resource))
	  (unresource))
	(unresource)))
  object)

(defmethod flush-resource ((resource symbol) &key (completely nil))
  (flush-resource (find-resource resource) :completely completely))

(defmethod flush-resource ((resource resource) &key (completely nil)) ; shouldn't this use unresource-resourced-object?
  (with-lock ((resource-lock resource))
    (maphash #'(lambda (key val)
		 (if (eq val :unclaimed)
		     (deallocate-resourced-object resource key)
		     (when completely
		       (when val
			 (dotimes (i val) ; take care of shared objects
			   (ignore-errors (release-resourced-object resource key))))
		       (deallocate-resourced-object resource key))))
	     (resource-pool resource))))

(defmethod deallocate-whole-resource ((resource symbol)) ; symbolics-compatible name
  (flush-resource resource :completely t))

(defmethod deallocate-whole-resource ((resource resource)) ; symbolics-compatible name
  (flush-resource resource :completely t))

(defun deallocate-all-resources ()
  (maphash #'(lambda (key val)
	       (declare (ignore key))
	       (deallocate-whole-resource val))
	   *resources*))

(defmethod deallocate-resourced-object ((resource resource) (object t))
  (with-accessors ((deallocation-hook resource-deallocation-hook) (pool resource-pool)
		   (allocated-count resource-allocated-count) (freelist resource-freelist)) resource
    (freelist-pull freelist object)
    (remhash object pool)
    #+resource-annotations (remhash object (resource-annotations resource))
    (decf allocated-count)
    (apply-when deallocation-hook object))
  t)

(defmethod map-resource ((resource symbol) (function function))
  (map-resource (find-resource resource) function))

(defmethod map-resource ((resource resource) (function function))
  (with-lock ((resource-lock resource))
    (maphash function (resource-pool resource))))

(defun all-resources ()
  (let ((resources nil))
    (maphash #'(lambda (key val) 
		 (declare (ignore key))
		 (push val resources))
	   *resources*)
    resources))

(defun find-resource-for-object (ob &optional (resource-list t))
  (loop named find 
	for res in (if (listp resource-list) 
		       resource-list
		       (all-resources))
	as resource = (find-resource res)
	as state = (resourced-object-state resource ob)
	until (not (eq state :not-resourced))
	finally (return-from find (if (eq state :not-resourced)
				      nil
				      resource))))
