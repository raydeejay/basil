;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:basil)

(defparameter *stop* nil)
(defparameter *jump-to* nil)
(defparameter *current-line* nil)

(defun execute (tokens)
  (let ((subinstructions (split-sequence '|:| tokens
                                         :test 'equal
                                         :remove-empty-subseqs t)))
    (loop :for sub :in subinstructions
       :for instr := (find (car sub) *commands*
                           :test 'equal :key 'car)
       :while (not (equal 'jump
                          (if instr
                              (apply (cdr instr) (cdr sub))
                              (error "Instruction ~A not found."
                                     (car sub))))))))

(defun evaluate (line)
  (let ((tokens (tokenize line)))
    (when tokens
      (cond ((numberp (car tokens))
             (compile* tokens))
            (t (execute tokens))))))
