;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:basil)

(defparameter *variables* (make-hash-table :test 'equal))

(defun set-variable (var value)
  (setf (gethash var *variables*) value))

(defun get-variable (var)
  (gethash var *variables*))

(defparameter *math-tokens*
  '(|+| |-| |*| |/| |%| |(| |)| |<| |>| |<=| |>=|))

(defparameter *math-functions*
  '(sin asin cos acos tan atan sqrt length))

;; this is probably a terrible solution, but... it works for now
;; token translation is restricted to those whitelisted,
;; everything else is taken as a BASIC variable
(defun eval-math (tokens)
  (let ((substituted
         (loop :with i := 0
            :while (< i (length tokens))
            :for token := (elt tokens i)
            :collect (if (symbolp token)
                         (cond  ((find token *math-tokens*)
                                 token)
                                ((find token *math-functions*)
                                 (incf i)
                                 (funcall token (elt tokens i)))
                                (t (gethash token *variables*)))
                         token)
            :do (incf i))))
    (eval (infix:string->prefix (format nil "~{~A ~}" substituted)))))
