;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:basic)

(defparameter *symbols* '(#\: #\, #\; #\=))

(defun eat-spaces (stream)
  (loop :for next := (peek-char nil stream nil nil)
     :while (and next (equal next #\Space))
     :do (read-char stream nil nil)))

(defun read-number (stream)
  (loop :for next := (peek-char nil stream nil nil)
     :while (and next (digit-char-p next))
     :collect (read-char stream) :into n
     :finally (return (parse-integer (coerce n 'string)))))

(defun read-string (stream)
  (loop :for c := (peek-char nil stream nil nil)
     :while c
     :if (equal c #\")
     :do (return (coerce token 'string))
     :else
     :collect (read-char stream) :into token
     :finally (error "Unterminated string~%")))

(defun read-identifier (stream)
  (loop :for c := (peek-char nil stream nil nil)
     :while (and c
                 (not (equal c #\Space))
                 (not (find c *symbols*)))
     :collect (read-char stream) :into token
     :finally (return (read-from-string (coerce token 'string)))))

(defun read-symbol (stream)
  (loop :for c := (peek-char nil stream nil nil)
     :while (and c (find c *symbols*))
     :collect (read-char stream) :into token
     :finally (return (intern (coerce token 'string)))))

(defun read-token (stream)
  (loop :initially (eat-spaces stream)
     :for c := (peek-char nil stream nil nil)
     :do (cond ((null c)
                (return))
               ((digit-char-p c)
                (return (read-number stream)))
               ((equal c #\")
                (read-char stream)
                (let ((result (read-string stream)))
                  (read-char stream)
                  (return result)))
               ((alpha-char-p c)
                (return (read-identifier stream)))
               ((find c *symbols*)
                (return (read-symbol stream)))
               (t (return)))))

(defun tokenize (line)
  (with-input-from-string (stream line)
    (loop :for token := (read-token stream)
       :while token
       :collect token :into tokens
       :finally (return tokens))))
