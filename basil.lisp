;;;; basil.lisp

(in-package #:basil)

;;; "basil" goes here. Hacks and glory await!

(defparameter *program* nil)
(defparameter *variables* (make-hash-table :test 'equal))

(defun compile* (tokens)
  (setf *program* (remove (car tokens) *program* :key 'car))
  (when (> (length tokens) 1)
    (push tokens *program*)
    (setf *program* (sort *program* '< :key 'car)))
  (format t "0 OK~%"))

(defun execute (tokens)
  (let ((subinstructions (split-sequence '|:| tokens
                                         :test 'equal
                                         :remove-empty-subseqs t)))
    (loop :for sub :in subinstructions
       :for instr := (find (car sub) *commands*
                           :test 'equal :key 'car)
       :do (if instr
               (apply (cdr instr) (cdr sub))
               (error "Instruction ~A not found." (car sub))))))

(defun evaluate (line)
  (let ((tokens (tokenize line)))
    (when tokens
      (cond ((numberp (car tokens))
             (compile* tokens))
            (t (execute tokens))))))

(defun repl ()
  (loop :for input := (read-line *standard-input* nil nil)
     :while input
     :do
     (cond ((equal input "exit")
            (loop-finish))
           (t (evaluate input)))
     :finally (format t "0 OK~%")))
