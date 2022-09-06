(defun format (option str &rest args)
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
      (print retstr)
      nil)
    retstr))

(format t "abc")
(format t "abc~ad" "hello")
(format t "abc~ad~aefg" 1 (cons 1 2))
(format t "abc~a~%d~aefg~%" 1 (cons 1 2))
(format nil "abc~a~%d~aefg~%" 1 (cons 1 2))
