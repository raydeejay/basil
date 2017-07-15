;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:basic)

(defparameter *commands* nil)

(defcommand 'list list-program ()
  (loop :for line :in *program*
     :do (format t "~{~S ~}~%" line)
     :finally (format t "~%0 OK~%")))

(defcommand 'run run-program ()
  (loop :for line :in *program*
     :do ;; (format t "EXECUTING ~{~A ~}~%" line)
     (execute (cdr line))
     :finally (format t "0 OK~%")))

(defcommand 'clear clear ()
  (setf *variables* (make-hash-table :test 'equal)))

(defcommand 'new new ()
  (clear)
  (setf *program* nil))



(defcommand 'print print-values (&rest tokens)
  (loop :with last := nil
     :for tok :in tokens :do
     (when (and last
                (not (equal last '|,|))
                (not (equal last '|;|)))
       (format t " "))
     (cond ((numberp tok)
            (format t "~D" tok))
           ((stringp tok)
            (format t "~A" tok))
           ((equal tok '|,|)
            (format t "~A" #\Tab))
           ((equal tok '|;|)
            ;; do nothing
            )
           ((gethash tok *variables*)
            (format t "~A" (gethash tok *variables*)))
           (t (error "Unknown variable ~A" tok)))
     (setf last tok)
     :finally
     (when (and last
                (not (equal last '|,|))
                (not (equal last '|;|)))
       (format t "~%"))))

(defun read-string-for-input (stream)
  (loop :for c := (peek-char nil stream nil nil)
     :while c
     :if (equal c #\Newline)
     :do (progn (read-char stream)
                (return (coerce token 'string)))
     :else
     :collect (read-char stream) :into token
     :finally (error "Unterminated string~%")))

(defun read-number-for-input (stream)
  (loop :for c := (peek-char nil stream nil nil)
     :while c
     :if (equal c #\Newline)
     :do (progn (read-char stream)
                (return (parse-integer (coerce token 'string))))
     :else
     :collect (read-char stream) :into token
     :finally (error "Malformed number~%")))

(defcommand 'input input-string (prompt var)
  (format t "~A? " prompt)
  (setf (gethash var *variables*)
        (read-string-for-input *standard-input*)))

(defcommand 'input# input-number (prompt var)
  (format t "~A? " prompt)
  (setf (gethash var *variables*)
        (read-number-for-input *standard-input*)))
