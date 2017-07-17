;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:basil)

(defparameter *commands* nil)
(defparameter *stop* nil)
(defparameter *jump-to* nil)

(defun find-line (n &key jump-down)
  (position n *program*
            :test (if jump-down '<= '=)
            :key 'car))

(defcommand 'rem remark (&rest args)
  (declare (ignore args)))

(defcommand 'stop stop (&rest args)
  (declare (ignore args))
  (setf *stop* T)
  (format t "Stopping~%"))

(defcommand 'goto go-to (n)
  (let ((target (find-line n :jump-down t)))
    (if target
        (setf *jump-to* target)
        (error "GOTO to inexistent line ~D" n))))

(defcommand 'run run-program (&optional start)
  (setf *stop* nil)
  (loop :for i := (if start
                      (find-line start :jump-down t)
                      0)
     :then (if *jump-to*
               (let ((a (+ *jump-to*)))
                 (setf *jump-to* nil)
                 a)
               (1+ i))
     :while (and (< i (length *program*))
                 (not *stop*))
     :do (execute (cdr (elt *program* i)))
     :finally (format t "0 OK~%")))

(defcommand 'list list-program ()
  (loop :for line :in *program*
     :do (loop :for token :in line :do
            (cond ((equal token '|,|)
                   (format t ","))
                  ((equal token '|;|)
                   (format t ";"))
                  ((equal token '|:|)
                   (format t " :"))
                  ((equal token '|%|)
                   (format t " %"))
                  ((equal token '|(|)
                   (format t " ("))
                  ((equal token '|)|)
                   (format t " )"))
                  (t (format t " ~S" token)))
            :finally (format t "~%"))
     :finally (format t "~%0 OK~%")))

(defcommand 'clear clear ()
  (setf *variables* (make-hash-table :test 'equal)))

(defcommand 'new new ()
  (clear)
  (setf *program* nil))

(defcommand 'let let= (&rest args)
  (destructuring-bind (var equals-sign value) args
    (if (equal equals-sign '|=|)
        (setf (gethash var *variables*) value)
        (error "Bad LET Syntax!~%"))))

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
