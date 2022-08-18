;;================================================================
;; Primitive functions and macros
;;================================================================
;; - `defun` and `defmacro` are defined as macros.
;;   When evaluated,
;;      (defun f (a b &rest r) ...)
;;   expands to the following expression:
;;      (<- f (lambda (a b &rest r) ...))
;;   where `<-` is used to assign local variables.
;; - Note that using `<-` outside of any function is effectively the same as using `defvar`.
;; - `&rest` can be used to define macros and functions with variable-numbered arguments.
;; - The definition of `defun` can be written using backquotes in Common Lisp as:
;;      `(<- ,name (lambda ,args ,@body))
(defvar defun (macro (name args &rest body)
  (list '<- name (cons 'lambda (cons args body)))))

(defvar defmacro (macro (name args &rest body)
  (list '<- name (cons 'macro (cons args body)))))


;; - `0`, `1` and `2` are variable names, since integer types do not exist.
;; - Numbers are expressed in unary, as the number of items in a list.
;; - `defvar` is used to define global variables.
(defvar 0 ())
(defvar 1 (list t))
(defvar 2 (list t t))

;; Arithmetic and logic
(defun + (n m)
  (cond ((atom m) n)
        (else (+ (cons t n) (cdr m)))))
(defun - (n m)
  (cond ((atom n) nil)
        ((atom m) n)
        (else (- (cdr n) (cdr m)))))
(defun <= (n m) (atom (- n m)))
(defun not (p) (cond (p nil) (else t)))
(defun and (x y) (cond (x y) (else nil)))
(defun = (n m) (and (<= n m) (<= m n)))


;; Control flow
(defmacro when (condition &rest body)
  (list 'cond (cons condition body)))

(defmacro while (condition &rest body)
  (list 'cond (list condition (cons 'progn body) (cons 'while (cons condition body)))))


;; Printing
(defmacro print-quote (&rest l)
  ;; - Example usage:
  ;;     > (<- d '(x y z))
  ;;     > (print-quote a b c (unquote d) e \n f g)
  ;;     a b c (x y z) e
  ;;     f g
  ;; - (print \s) prints a whitespace.
  ;; - (print ... t) prints the expression without a newline.
  (cond
    ((atom l)
      nil)
    ((and (not (atom (car l))) (eq 'unquote (car (car l))))
      (list 'progn
        (list 'print (car (cdr (car l))) 't)
        (list 'print '\s 't)
        (cons 'print-quote (cdr l))))
    ((eq '\n (car l))
      (list 'progn
        (list 'print)
        (cons 'print-quote (cdr l))))
    (t
      (list 'progn
        (list 'print (list 'quote (car l)) 't)
        (list 'print '\s 't)
        (cons 'print-quote (cdr l))))))


;;================================================================
;; Example 1: Prime finding function
;;================================================================
;; Prime finding function.
;; Returns either `t` or `nil` given a unary number `n`.
;; The local function `mod` is invisible outside of the function scope of `isprime`.
(defun isprime (n)
    (defun mod (n m)
        (cond ((<= m n) (mod (- n m) m))
              (else n)))
    (<- result (<= 2 n))
    (<- i 2)
    (while (and (<= i (- n 1)) result)
        (when (= 0 (mod n i))
            (<- result nil))
        (<- i (+ 1 i)))
    result)

;; Prints prime numbers up to 10.
;; Numbers are expressed in unary as the number of elements in a list.
(print-quote \n \n *** Example 1: Finding primes *** \n \n)

(<- max (list t t t t t   t t t t t))   ;; Represents 10
(<- i ())                               ;; Represents 0
(while (<= i max)
    (<- p (isprime i))
    (when p
      (print i))
    (<- i (+ 1 i)))


;;================================================================
;; Example 2: Interactive prime finding function
;;================================================================
;; An interactive function that takes a number from a user input and shows whether if it is a prime.
;; The user input is taken through `(read)`, which is assigned to the variable `input`.
(defun checkprime ()
    (<- input nil)
    (while (atom input)
        (print-quote Please input a number in unary as a list, in the format (t t t t t) : \n)
        (print-quote number>)
        (<- input (read))
        (when (atom input)
            (print-quote Atom provided as input: (unquote input) \n)))
    (<- p (isprime input))
    (cond
        (p
            (print-quote (unquote input) is a prime. \n))
        (else
            (print-quote (unquote input) is not a prime. \n))))

(print-quote \n \n *** Example 2: Interactive prime finding function *** \n \n)
(checkprime)

(print-quote Examples finished. \n)
