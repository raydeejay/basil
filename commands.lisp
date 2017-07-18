;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:basil)

(defparameter *commands* nil)

(defcommand 'rem remark (&rest args)
  "REM [<arg> ...]

   Ignores its arguments."
  (declare (ignore args)))

(defcommand 'help help (fname)
  "HELP <instr>

   Displays help about a command."
  (let ((target (find fname *commands*
                      :test 'equal :key 'car)))
    (format t "~A~2%" (if target
                         (documentation (cdr target) 'function)
                         "No help on that."))))

(defcommand 'stop stop ()
  "STOP

   Unconditionally stops execution."
  (setf *stop* T)
  (format t "Stopping~%"))

(defcommand 'goto go-to (n)
  "GOTO <line number>

   Unconditionally jumps to the specified line."
  (let ((target (find-line n :jump-down t)))
    (if target
        (setf *jump-to* target)
        (error "GOTO to inexistent line ~D" n))))

(defcommand 'run run-program (&optional start)
  "RUN [line number]

   Clears variables, restores the data pointer, and begins execution
   either at the specified line if any, or the beginning of the
   program."
  (setf *stop* nil)
  (restore 0)
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
     :do (progn (setf *current-line* i)
                (execute (cdr (elt *program* i))))
     :finally (format t "0 OK~%")))

(defcommand 'list list-program ()
  "LIST

   Displays a listing of the program in memory."
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
  "CLEAR

   Clears the variables."
  (setf *variables* (make-hash-table :test 'equal)))

(defcommand 'new new ()
  "NEW

   Clears the variables and deletes the current program."
  (clear)
  (setf *program* nil))

(defcommand 'let let= (&rest args)
  "LET <var> = <expr>

   Performs assignment. The infix expression EXPR is evaluated and the
   result assigned to the variable VAR."
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

(defparameter *loop-stack* nil)

(defcommand 'for for%% (&rest args)
  ;; check syntax here? :D
  (let* ((var (caar (split-sequence '= args)))
         (start-expr (car (split-sequence 'TO (subseq args 2))))
         (end-expr (subseq args (+ 1 2 (length start-expr)))))
    (set-variable var (eval-math start-expr))
    (push (list var
                (eval-math end-expr)
                (1+ *current-line*))
          *loop-stack*)))

(defcommand 'next next%% (&rest args)
  (declare (ignore args))
  (destructuring-bind (var limit jump-point)
      (car *loop-stack*)
    (if (< (get-variable var) limit)
        (progn (set-variable var (1+ (get-variable var)))
               (setf *jump-to* jump-point)
               'jump)
        (pop *loop-stack*))))

(defparameter *gosub-stack* nil)

(defcommand 'gosub go-sub (n)
  (let ((target (find-line n :jump-down t)))
    (if target
        (progn (push (1+ *current-line*) *gosub-stack*)
               (setf *jump-to* target))
        (error "GOSUB to inexistent line ~D" n))))

(defcommand 'return return%% (&rest args)
  (declare (ignore args))
  (let ((jump-point (pop *gosub-stack*)))
    (if jump-point
        (progn (setf *jump-to* jump-point)
               'jump)
        (error "RETURN without gosub on line ~D" *current-line*))))

(defparameter *data-line-index* 0)
(defparameter *data-pointer* 2)

(defun data-line-p (line)
  (equal (cadr line) 'data))

;; 10 data 1 , 2 , 3 , 4 , 5
(defun read-next-data ()
  "So my first implementation will be to keep a pointer to the last
read data line, and scan the code forwards until a DATA command is
found. Adding lines past that point will have that DATA count for the
next READ, adding lines before the point will need a RESTORE to move
the pointer back (and probably mess things anyway), so those lines can be read by further READ commands, which I hope is compatible enough with Sinclair BASIC. This needs wooork."
  (loop :for line := (elt *program* *data-line-index*)
     :if (or (not (data-line-p line))
             (> *data-pointer* (1- (length line))))
     :do (progn (incf *data-line-index*)
                (setf *data-pointer* 2))
     :else
     :do (incf *data-pointer* 2)
     (return (elt (elt *program* *data-line-index*)
                  (- *data-pointer* 2)))))

(defcommand 'data data (&rest args)
  (declare (ignore args)))

(defcommand 'read read%% (&rest args)
  (loop :for var :in args :by #'cddr
     :do (set-variable var (read-next-data))))

(defcommand 'restore restore (&optional (n 0))
  (setf *data-line-index* (find-line n :jump-down t)
        *data-pointer* 2))
