(load "./lazy.cl")


(def-lazy "(" (+ 32 8))
(def-lazy ")" (succ "("))
(def-lazy " " 32)
(def-lazy "\\n" (+ 8 2))

(def-lazy "A" (succ 64))


(defmacro defrec-lazy (name args body)
  `(def-lazy ,name
     ((lambda (x) (x x))
      (lambda ,args ,body))))

(defmacro-lazy typematch (tvar t0 t1)
  `(,tvar ,t0 ,t1))

(defun-lazy type-atom (t0 t1) t0)
(defun-lazy type-list (t0 t1) t1)

(defrec-lazy printexpr (self expr)
  (let ((head (car expr))
  (headtype (car head)) (value (cdr head))
  )
  ;; (list "A" 256 256)
    (typematch headtype
      ;; atom
      (list "A" 256 256)
      ;; list
      (list "(" "A" ")" 256 256)    
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

(defun-lazy main (stdin)
  (printexpr (list (cons type-list "A")))
  ;; (list "A" 256 256)
  )

;; (print (macroexpand-lazy main))
(format t (compile-to-ski-lazy main))

;; (defun-lazy main (stdin)
;;   (lazystr2blcstr (program (blcstr2lazystr stdin))))

;; (print (compile-to-blc-lazy main))
