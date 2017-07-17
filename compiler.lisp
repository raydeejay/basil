;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:basil)

(defparameter *program* nil)

(defun compile* (tokens)
  (setf *program* (remove (car tokens) *program* :key 'car))
  (when (> (length tokens) 1)
    (push tokens *program*)
    (setf *program* (sort *program* '< :key 'car)))
  (format t "0 OK~%"))

(defun find-line (n &key jump-down)
  (position n *program*
            :test (if jump-down '<= '=)
            :key 'car))
