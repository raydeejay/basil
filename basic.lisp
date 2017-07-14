;;;; basic.lisp

(in-package #:basic)

;;; "basic" goes here. Hacks and glory await!

(defparameter *symbols* '(#\, #\: #\;))
(defparameter *program* nil)
(defparameter *commands* nil)
(defparameter *variables* (make-hash-table :test 'equal))

(defcommand "list" list-program ()
  (loop :for line :in *program*
     :do (format t "~{~A ~}~%" line)
     :finally (format t "~%0 OK~%")))

(defcommand "run" run-program ()
  (loop :for line :in *program*
     :do ;; (format t "EXECUTING ~{~A ~}~%" line)
     (execute (cdr line))
     :finally (format t "0 OK~%")))

(defcommand "print" print-value (n)
  (cond ((numberp (read-from-string n))
         (format t "~A~%" n))
        ((stringp (read-from-string n))
         (format t "~A~%" n))
        ((gethash n *variables*)
         (format t "~A~%" (gethash n *variables*)))))

(defcommand "input" input-value (prompt var)
  (format t "~A? " prompt)
  (setf (gethash var *variables*) (read)))

(defun eat-spaces (stream)
  (loop :for next := (peek-char nil stream nil nil)
     :while (and next (equal next #\Space))
     :do (read-char stream nil nil)))

(defun read-number (stream))

(defun read-string (stream))

(defun read-identifier (stream))

;; called at the beginning of a line, once spaces have been eaten
(defun read-token (stream)
  (loop :for c := (peek-char nil stream nil nil)
     :with token := nil
     :initially (eat-spaces stream)
     :do (cond ((null c)
                (return (when token (coerce (reverse token)
                                            'string))))
               ((equal c #\Space)
                (eat-spaces stream)
                (return (if token
                            (coerce (reverse token) 'string)
                            (read-token stream))))
               ((find c *symbols*)
                (if token
                    (return (coerce (reverse token) 'string))
                    (progn (eat-spaces stream)
                           (return (string (read-char stream))))))
               (c (push (read-char stream) token))
               (t (return)))))

(defun tokenize (line)
  (with-input-from-string (stream line)
    (loop :for token := (read-token stream)
       :while token
       :collect token :into tokens
       :finally (return tokens))))


(defun compile* (tokens)
  (setf (car tokens) (parse-integer (car tokens)))
  (push tokens *program*)
  (setf *program* (sort *program* '< :key 'car))
  ;; (format t "PROGRAM: ~{~{~A ~}~%~}~%" *program*)
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
    (cond ((numberp (read-from-string (car tokens)))
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
           (t (evaluate input)))))
