;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:basil)

(defmacro defcommand (basil-name fn-name args &body body)
  `(push (cons ,basil-name
               (defun ,fn-name ,args
                   ,@body))
         *commands*))
