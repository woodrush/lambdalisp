(defclass counter ()
  (i 0)

  (defmethod *init (i)
    (setf (. self i) i))

  (defmethod inc ()
    (setf (. self i) (+ 1 (. self i))))

  (defmethod dec ()
    (cond
      ((. self i)
        (setf (. self i) (- (. self i) 1)))
      (t
        (. self i))))

  (defmethod add (n)
    (setf (. self i) (+ (. self i) n))))

(defclass counter-sub (counter)
  (defmethod *init (c)
    ((. (. self super) *init) c))

  (defmethod sub (n)
    (setf (. self i) (- (. self i) n))))


(setf counter1 (new counter 0))
(setf counter2 (new counter-sub 100))

(format t "inc~%")
((. counter1 inc))
((. counter1 inc))
((. counter2 inc))
((. counter1 inc))
((. counter2 inc))
((. counter1 inc))


(format t "dec~%")
((. counter1 dec))
((. counter1 dec))
((. counter2 dec))
((. counter1 dec))

(format t "add two~%")
((. counter1 add) 2)
((. counter2 add) 2)


(format t "subtract two~%")
((. counter2 sub) 2)

(format t "set values to 5 and 100~%")
(setf (. counter1 i) 5)
(setf (. counter2 i) 100)

(format t "inc~%")
((. counter1 inc))
((. counter2 inc))
((. counter2 inc))
((. counter1 inc))
((. counter2 inc))
((. counter2 inc))
