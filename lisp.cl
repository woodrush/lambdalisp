(load "./lazy.cl")


(def-lazy "(" (+ 32 8))
(def-lazy ")" (succ "("))
(def-lazy " " 32)
(def-lazy "\\n" (+ 8 2))

(def-lazy "A" (succ 64))
(def-lazy "B" (succ "A"))
(def-lazy "C" (succ "B"))
(def-lazy "D" (succ "C"))


(defrec-lazy map (f list)
  (if (isnil list)
    nil
    (cons (f (car list)) (map f (cdr list)))))

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

(defrec-lazy printexpr (atomenv expr)
  (let ((exprtype (car expr)) (value (cdr expr)))
    (typematch exprtype
      ;; atom
      ;; (char2stream "A")
      (char2stream (car (value cdr atomenv)))
      ;; list
      (catstreamlist
        (list (char2stream "(")
              (catstreamlist (map (printexpr atomenv) value))
              (char2stream ")"))))))


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
  ((printexpr
      (list "A" "B" "C")
      (cons type-list (list (cons type-atom 2) (cons type-list (list (cons type-atom 1))) (cons type-atom 2)))) (inflist 256))
  ;; (list "A" 256 256)
  )

;; (print (macroexpand-lazy main))
(format t (compile-to-ski-lazy main))

;; (defun-lazy main (stdin)
;;   (lazystr2blcstr (program (blcstr2lazystr stdin))))

;; (print (compile-to-blc-lazy main))
