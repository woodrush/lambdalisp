(defmacro when (condition &rest body)
  (list 'cond (cons condition body)))

(defun isprime (n)
    (defun divides (n i)  ;; Invisible from the outer scope
        (= 0 (% n i)))
    (setq result (<= 2 n))
    (setq i 2)
    (while (< i n)
        (when (divides n i)
            (setq result nil)
            (setq i n))
        (setq i (+ 1 i)))
    result)

;; Interactive prime checker
(defun checkprime (n)
    (printq Please input a number: \n)
    (printq number>)
    (setq input (read))
    (cond
        ((not (isint input))
            (printq The input (unquote input) was not a number.))
        ((isprime input)
            (printq The input (unquote input) is a prime.))
        (else
            (printq The input (unquote input) is not a prime.)))
    nil)

(checkprime)

;; Print primes
(setq max 10)
(setq i 1)
(while (<= i max)
    (when (isprime i)
      (print i))
    (setq i (+ 1 i)))
