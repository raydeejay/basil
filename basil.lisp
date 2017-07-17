;;;; basil.lisp

(in-package #:basil)

;;; "basil" goes here. Hacks and glory await!

(defun repl ()
  (loop :for input := (read-line *standard-input* nil nil)
     :while input
     :do
     (cond ((equal input "exit")
            (loop-finish))
           (t (evaluate input)))
     :finally (format t "0 OK~%")))
