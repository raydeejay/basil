;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:basic)

(defmacro defcommand (basic-name fn-name args &body body)
  `(push (cons ,basic-name
               (defun ,fn-name ,args
                   ,@body))
         *commands*))
