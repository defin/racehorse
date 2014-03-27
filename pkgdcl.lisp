;;;; -*- Mode: Lisp; Package: CL-USER -*-

(in-package cl-user)

(defpackage racehorse
  (:use cl excl)
  (:nicknames r)
  (:export *default-resource-hash-size* *default-resource-rehash-size* *default-resource-rehash-threshold* *default-freelist-size*
	   defresource using-resource find-resource object-claimed-p claim-resourced-object share-resourced-object unresource-resourced-object
	   release-resourced-object flush-resource map-resource all-resources find-resource-for-object
	   deallocate-whole-resource deallocate-resourced-object deallocate-all-resources
	   resourced-object-annotations
	   define-string-buffer resource-for-string-buffer release-string-buffer make-string-buffer claim-string-buffer
	   string-append))
