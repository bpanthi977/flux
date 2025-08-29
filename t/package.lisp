(defpackage #:flux/tests
  (:use #:cl #:flux #:flux/utils #:fiveam)
  (:local-nicknames (#:g #:flux)))

(in-package #:flux/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for s being the symbols of (find-package :flux)
	do (import s)))
