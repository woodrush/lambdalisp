;;================================================================
;; Primitive functions and helper macros
;;================================================================
(defun - (n m)
    (cond ((atom n) nil)
          ((atom m) n)
          (else (- (cdr n) (cdr m)))))
(defun + (n m)
    (cond ((atom m) n)
          (else (+ (cons t n) (cdr m)))))
(defun <= (n m) (atom (- n m)))
(defun not (p) (cond (p nil) (else t)))
(defun and (x y) (cond (x y) (else nil)))
(defun = (n m) (and (<= n m) (<= m n)))
;; `0`, `1` and `2` are variable names, since integer types do not exist.
(defvar 0 ())
(defvar 1 (list t))
(defvar 2 (list t t))

;; `&rest` can be used to use variable-numbered arguments.
(defmacro when (condition &rest body)
  ;; `(cond (,condition ,@body))
  (list 'cond (cons condition body)))

(defmacro while2 (condition &rest body)
  ;; `(cond (,condition ,@body (while2 ,condition ,@body)))
  (list 'cond (list condition (cons 'progn body) (cons 'while2 (cons condition body)))))

(defmacro print-quote (&rest l)
  ;; Example usage:
  ;;   > (<- d '(x y z))
  ;;   > (print-quote a b c (unquote d) e)
  ;;   a b c (x y z) e
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
;; Prime finding function
;;================================================================
;; Prime finding function.
;; Returns either `t` or `nil` given a unary number `n`.
(defun isprime (n)
    ;; Local functions are invisible outside of the function scope.
    (defun mod (n m)
        (cond ((<= m n) (mod (- n m) m))
              (else n)))
    (<- result (<= 2 n))
    (<- i 2)
    (while2 (and (<= i (- n 1)) result)
        (when (= 0 (mod n i))
            (<- result nil))
        (<- i (+ 1 i)))
    result)


;; Prints prime numbers up to 8.
;; Numbers are expressed in unary as the number of elements in a list.
(<- max (list t t t t t t t t))   ;; Represents 8
(<- i ())                         ;; Represents 0
(while2 (<= i max)
    (<- p (isprime i))
    (when p
      (print i))
    (<- i (+ 1 i)))


;;================================================================
;; Interactive prime finding function
;;================================================================
;; An interactive function that takes a number from a user input and shows whether if it is a prime.
;; The user input is taken through `(read)`.
(defun checkprime ()
    (<- input nil)
    (while2 (atom input)
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

(checkprime)
