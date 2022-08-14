(load "./lazy.cl")


(def-lazy "(" (+ 32 8))
(def-lazy ")" (succ "("))
(def-lazy " " 32)
(def-lazy "\\n" (+ 8 2))

(def-lazy "A" (succ 64))


(defmacro-lazy typematch (tvar t0 t1)
  `(,tvar ,t0 ,t1))

(defun-lazy type-atom (t0 t1) t0)
(defun-lazy type-list (t0 t1) t1)

(defun-lazy char2stream (char)
  (cons char))

(defun-lazy catstream (stream1 stream2)
  (lambda (stream) (stream1 (stream2 stream))))

(defrec-lazy catstreamlist (streamlist)
  (if (isnil streamlist)
    (lambda (x) x)
    (catstream (car streamlist) (catstreamlist (cdr streamlist)))))

(defrec-lazy map (f list)
  (if (isnil list)
    nil
    (cons (f (car list)) (map f (cdr list)))))

(defrec-lazy printexpr (expr)
  (let ((exprtype (car expr)) (value (cdr expr)))
  ;; (list "A" 256 256)
    (typematch exprtype
      ;; atom
      (char2stream "A")
      ;; list
      ;; (char2stream "(")
      (catstream (catstream (char2stream "(") (catstreamlist (map printexpr value))) (char2stream ")"))
      ;; (((char2stream "(") (printexpr (cdr expr))) (char2stream ")"))
      ;; (cons "(" suffix)
      )
      )
      )


;; (defrec-lazy parseexpr (self stream)
;;   (let ((c (car stream)))
;;     (cond
;;       ((= c "(")
;;         (cons type-list (self self )))
;;       ((= c ")"))
;;       ((= c " "))
;;       ((= c "\n")))))

(def-lazy test
  (list type-atom))

(defrec-lazy f (x)
  (cons "A" (f nil))
  ;; (lambda (x) (cons "A" (x nil)))
  )

(defun-lazy main (stdin)
  ;; (f nil)
  ((printexpr (cons type-list (list (cons type-atom "A") (cons type-list (list (cons type-atom "A"))) (cons type-atom "A")))) (inflist 256))
  ;; (list "A" 256 256)
  )

;; (print (macroexpand-lazy main))
(format t (compile-to-ski-lazy main))

;; (defun-lazy main (stdin)
;;   (lazystr2blcstr (program (blcstr2lazystr stdin))))

;; (print (compile-to-blc-lazy main))
