(defun format (option str &rest args)
  ;; Supports ~a and ~%
  (setq ret ())
  (setq newline "
")
  (loop
    (cond
      ((eq str "")
        (return-from))
      ((eq (carstr str) "~")
        (setq str (cdrstr str))
        (cond
          ((eq (carstr str) "%")
            (setq ret (cons newline ret)))
          ((eq (carstr str) "a")
            (if args
              (progn
                (setq item (car args))
                (setq args (cdr args)))
              (setq item nil))
            (setq ret (cons (str item) ret)))))
      (t
        (setq ret (cons (carstr str) ret))))
    (setq str (cdrstr str)))

  (setq retstr "")
  (loop
    (if (eq ret ())
      (return-from))
    (setq retstr (+ (car ret) retstr))
    (setq ret (cdr ret)))
  (if option
    (progn
      (print retstr t)
      nil)
    retstr))

(format t "Hello, world!~%")

(setq x "world")
(format t "Hello, ~a!~%" x)

(format t "Line 1~%Line 2~%Line 3~%")

(format t "Variable 1: ~a~%Variable 2: ~a~%Variable 3: ~a~%Variable 4: ~a~%" 1 (cons 1 2) '(a b c) (lambda () ()))

(format t "Created string: ~a~%" (format nil "Create a string: ~a ~a" 1 (cons 1 2)))
