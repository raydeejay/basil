;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:basil)

(defparameter *commands* nil)

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
  (destructuring-bind (var equals-sign . math-exp) args
    (if (equal equals-sign '|=|)
        (setf (gethash var *variables*)
              (if (= 1 (length math-exp))
                  (car math-exp)
                  (eval-math math-exp)))
        (error "Bad LET Syntax!~%"))))

(defun one-print-arg (tokens)
  (car (split-sequence-if (lambda (c)
                            (or (equal c '|,|)
                                (equal c '|;|)))
                          tokens
                          :count 1)))

(defcommand 'print print-values (&rest tokens)
  (loop :with i := 0
     :with last := nil
     :while (< i (length tokens))
     :for tok := (elt tokens i)
     :do
     (when (and last
                (not (equal last '|,|))
                (not (equal last '|;|)))
       (format t " "))
     (cond ((numberp tok)
            (let ((expr (one-print-arg (subseq tokens i))))
              (format t "~A" (eval-math expr))
              (setf last tok)
              (setf i (+ i (length expr)))))
           ((stringp tok)
            (format t "~A" tok)
            (setf last tok)
            (incf i))
           ((equal tok '|,|)
            (format t "~A" #\Tab)
            (setf last tok)
            (incf i))
           ((equal tok '|;|)
            ;; do nothing
            (setf last tok)
            (incf i))
           ((gethash tok *variables*)
            (format t "~A" (gethash tok *variables*))
            (setf last tok)
            (incf i))
           ((symbolp tok)
            (let ((expr (one-print-arg (subseq tokens i))))
              (format t "~A" (eval-math expr))
              (setf last tok)
              (setf i (+ i (length expr)))))
           (t (error "Unknown variable ~A" tok)))
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

(defcommand 'if if%% (&rest args)
  (let ((parts (split-sequence 'then args :count 2)))
    (let ((cond-exp (car parts))
          (then (cadr parts)))
      (if then
          (let ((result (eval-math cond-exp)))
            (if result
                (execute then)
                'jump))
          (error "Bad IF Syntax!~%")))))
