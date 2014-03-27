(in-package racehorse)

;(eval-when (:compile-toplevel :load-toplevel :execute)
;  (push :resource-annotations *features*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 3) (safety 0) (compilation-speed 0) (debug 0) (space 0))))


;;; Nonportable code

#+nil (push :weak-data *features*) ;allegro

(defun lock-locker (lock)
  #+allegro (mp::process-lock-locker lock))

(defun make-lock (&rest options)
  #+allegro (apply #'mp:make-process-lock options))

(defmacro with-lock ((&rest options) &body body)
  #+allegro `(mp:with-process-lock ,options ,@body))

(defparameter *current-procesS* #+allegro mp:*current-process*)


;;; Resource

(defvar *resources* (make-hash-table :test 'eq))

(defstruct (resource) 
  name
  (allocated-count 0)
  (in-use-count 0)
  (claim-count 0)
  (soft-limit nil)
  (hard-limit nil)
  freelist
  pool
  #+weak-data (weak-keys nil)
  lock
  constructor
  initializer
  reinitializer
  deinitializer
  deallocation-hook
  checker
  matcher
  finder
  #+resource-annotations annotations)

(defmethod print-object ((object resource) (stream stream))
  (with-slots (#+weak-data weak-keys name allocated-count in-use-count soft-limit hard-limit lock claim-count) object
    (print-unreadable-object (object stream :type nil :identity t)
      (format stream "~A ~A (~A): ~D objects allocated, ~D in use, ~A, ~A, ~D objects claimed" 
	      #+weak-data (if weak-keys
			      "Weak resource"
			    "Resource")
	      #-weak-data "Resource"
	      name
	      (aif (lock-locker lock)
		 (format nil "locked by ~A" it)
		 "unlocked")
	    allocated-count
	    in-use-count
	    (if soft-limit
		(format nil "soft limit at ~D" soft-limit)
		"no soft limit")
	    (if hard-limit
		(format nil "hard limit at ~D" hard-limit)
		"no hard limit")
	    claim-count))))


;;; Internals

(defmethod resourced-object-state ((resource resource) (object t))
  (gethash object (resource-pool resource) :not-resourced))

(defmethod (setf resourced-object-state) ((new-state t) (resource resource) (object t))
  (setf (gethash object (resource-pool resource)) new-state))


#+resource-annotations
(defmethod resourced-object-annotations ((resource symbol) (object t))
  (resourced-object-annotations (find-resource resource) object))
  
#+resource-annotations
(defmethod (setf resourced-object-annotations) ((new-value t) (resource symbol) (object t))
  (setf (resourced-object-annotations (find-resource resource) object) new-value))

#+resource-annotations
(defmethod resourced-object-annotations ((resource resource) (object t))
  (gethash object (resource-annotations resource) :not-resourced))

#+resource-annotations
(defmethod (setf resourced-object-annotations) ((new-value t) (resource resource) (object t))
  (setf (gethash object (resource-annotations resource)) new-value))

(defmethod get-unclaimed-object ((resource resource))
  (with-accessors ((freelist resource-freelist) (checker resource-checker)) resource ; duh, its FINDER that cancels out the freelist, not MATCHER!
;    (if matcher
;	(loop named find-object 
;	      for key being each hash-key of (resource-pool resource) using (hash-value value)
;	      with match = nil
;	      when (eq value :unclaimed)
;	      do (setq match (apply matcher key match-arguments))
;	      until match
;	      finally (return-from find-object match))
    (if checker
	(get-checked-object resource)
	(if (zerop (fill-pointer freelist))
	    nil
	    (freelist-pop freelist)))))

(defmethod get-checked-object ((resource resource))
  (with-accessors ((freelist resource-freelist) (checker resource-checker)) resource
    (loop named checked 
	  with len = (length freelist) 
	  for i from (1- len) downto 0
	  as object = (aref freelist i)
	  as check = (funcall checker object)
	  until check
	  finally (return-from checked (if check
					   (freelist-pull freelist object)
					   nil)))))

(defun resource-over-soft-limit-p (resource)
  (and (resource-soft-limit resource)
       (> (resource-allocated-count resource) (resource-soft-limit resource))))

(defmethod construct-resourced-object ((resource resource) &rest constructor-arguments)
  (with-accessors ((allocated-count resource-allocated-count) (hard-limit resource-hard-limit)) resource
    (let ((constructor (aif (resource-constructor resource)
			    it
			    (error 'resource-constructor-function-missing :resource resource))))
      (if (and hard-limit (= allocated-count hard-limit))
	  (error 'resource-over-hard-limit :resource resource)
	  (progn (incf allocated-count)
		 (when (resource-over-soft-limit-p resource)
		   (signal 'resource-over-soft-limit :resource resource))
		 (if (eq constructor #'make-instance)
		     (initialize-resourced-object resource (apply constructor (resource-name resource) constructor-arguments))
		     (initialize-resourced-object resource (apply constructor constructor-arguments))))))))

(defmethod activate-resourced-object ((resource resource) (object t))
  (setf (resourced-object-state resource object) 1)
  (incf (resource-in-use-count resource))
  object)

(defmethod initialize-resourced-object ((resource resource) (object t))
  (setf (resourced-object-state resource object) :unclaimed)
  #+resource-annotations (setf (resourced-object-annotations resource object) nil)
  (apply-when (resource-initializer resource) object)
  object)

(defmethod reinitialize-resourced-object ((resource resource) (object t) &rest arguments)
  (apply-when (resource-reinitializer resource) object arguments)
  object)

(defmethod deinitialize-resourced-object ((resource resource) (object t)) 
  (apply-when (resource-deinitializer resource) object)
  nil)

(defmethod deactivate-resourced-object ((resource resource) (object t))
  (decf (resource-in-use-count resource))
  (if (resource-over-soft-limit-p resource)
      (deallocate-resourced-object resource object)
      (progn (setf (resourced-object-state resource object) :unclaimed)
	     #+resource-annotations (setf (resourced-object-annotations resource object) nil)
	     (unless (resource-matcher resource)
	       ;; when the matcher is being used the freelist isn't
	       (vector-push-extend object (resource-freelist resource))))))

(defun freelist-pop (freelist)
  (let* ((ptr (1- (fill-pointer freelist)))
	 (elt (aref freelist ptr)))
    (setf (aref freelist ptr) nil)
    (decf (fill-pointer freelist))
    elt))

(defun freelist-pull (freelist object)
  (let ((pos (position object freelist :test #'eq)))
    (when pos
      (freelist-pop (swap freelist pos (1- (fill-pointer freelist)))))))

