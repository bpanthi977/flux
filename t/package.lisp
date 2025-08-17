(defpackage #:gauthali/tests
  (:use #:cl #:gauthali #:gauthali/utils #:fiveam)
  (:local-nicknames (#:g #:gauthali)))

(in-package #:gauthali/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for s being the symbols of (find-package :gauthali)
	do (import s)))
