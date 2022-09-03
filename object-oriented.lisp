(defvar counter (lambda (i)
  (let ((i ()))
  (let (
    (inc (lambda ()
      (setq i (cons t i))))
    (dec (lambda ()
      (if i (setq i (cdr i)) i))))
  (lambda (a)
      (if (eq a 'inc) inc
      (if (eq a 'dec) dec
      nil)))))))

(defvar counter1 (counter (quote c)))
((counter1 'inc))
((counter1 'inc))
((counter1 'dec))
((counter1 'inc))
