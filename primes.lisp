;; Define macros
(defmacro when (condition &rest body)
  (list 'cond (cons condition body)))

(defun isprime (n)
    (defun divides (n i)
        (= 0 (% n i)))
    (setq result (<= 2 n))
    (setq i 2)
    (while (< i n)
        (when (divides n i)
            (setq result nil)
            (setq i n))
        (setq i (+ 1 i)))
    result)

;; Print primes
(setq max 10)
(setq i 1)
(while (<= i max)
    (when (isprime i)
      (print i))
    (setq i (+ 1 i)))
