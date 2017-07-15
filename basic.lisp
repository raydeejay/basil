;;;; basic.lisp

(in-package #:basic)

;;; "basic" goes here. Hacks and glory await!

(defparameter *program* nil)
(defparameter *variables* (make-hash-table :test 'equal))

(defun compile* (tokens)
  (setf *program* (remove (car tokens) *program* :key 'car))
  (when (> (length tokens) 2)
    (push tokens *program*)
    (setf *program* (sort *program* '< :key 'car)))
  (format t "0 OK~%"))

(defun execute (tokens)
  (let ((instr (find (car tokens) *commands*
                     :test 'equal :key 'car)))
    (if instr
        (apply (cdr instr) (cdr tokens))
        (error "Instruction ~A not found." (car tokens)))))

(defun evaluate (line)
  (let ((tokens (tokenize line)))
    ;; (format t "TOKENS: ~A~%" tokens)
    (cond ((numberp (car tokens))
           ;; (format t "COMPILE: ~A~%" tokens)
           (compile* tokens))
          (t ;; (format t "EXECUTE: ~A~%" tokens)
             (execute tokens)))))

(defun repl ()
  (loop :for input := (read-line *standard-input* nil nil)
     :while input
     :do
     (cond ((equal input "exit")
            (loop-finish))
           (t (evaluate input)))
     :finally (format t "0 OK~%")))
