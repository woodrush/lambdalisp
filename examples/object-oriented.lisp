;;============================================================================
;; The following program is LambdaLisp-exclusive.
;; A Common Lisp compatible version is available as `object-oriented.cl`.
;;============================================================================
(defclass Counter ()
  (i 0)

  (defmethod inc ()
    (setf (. self i) (+ 1 (. self i))))

  (defmethod dec ()
    (setf (. self i) (- (. self i) 1))))


(defclass Counter-add (Counter)
  (defmethod *init (i)
    (setf (. self i) i))

  (defmethod add (n)
    (setf (. self i) (+ (. self i) n))))


(defclass Counter-addsub (Counter-add)
  (defmethod *init (c)
    ((. (. self super) *init) c))

  (defmethod sub (n)
    (setf (. self i) (- (. self i) n))))


(defparameter counter1 (new Counter))
(defparameter counter2 (new Counter-add 100))
(defparameter counter3 (new Counter-addsub 10000))

((. counter1 inc))
((. counter1 inc))
((. counter2 inc))
((. counter3 inc))
((. counter1 inc))
((. counter2 inc))
((. counter3 inc))
((. counter1 inc))
((. counter3 inc))

((. counter1 dec))
((. counter3 dec))
((. counter1 dec))
((. counter2 dec))
((. counter3 dec))
((. counter1 dec))

((. counter2 add) 100)
((. counter3 add) 10000)

((. counter3 sub) 100)

(setf (. counter1 i) 5)
(setf (. counter2 i) 500)
(setf (. counter3 i) 50000)

((. counter1 inc))
((. counter2 inc))
((. counter3 inc))
((. counter2 inc))
((. counter1 inc))
((. counter3 inc))
((. counter2 inc))
((. counter2 inc))
((. counter3 inc))
