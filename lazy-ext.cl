(load "./lazy.cl")

(defparameter simple-lambda-env-vars (concatenate 'list (coerce "xyzabcdefghijklmnopqrstuvw" `list) "α" "β" "γ" "δ" "ε" "ζ" "η" "θ"
;; "ι"
"κ"
"μ" "ν" "ξ" "ο" "π" "ρ"
;; "σ"
"τ" "υ" "φ" "χ" "ψ" "ω" (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `list)))

(defun to-simple-lambda (body &optional (env ()))
  (labels
    ((lookup (env var)
       (let ((i (position var (reverse env) :test #'equal)))
         (if i
          (nth i simple-lambda-env-vars)
          (decorate-varname var)))
         ))
    (if (atom body)
        (lookup env body)
        (if (not (islambda body))
          (format nil "(~a ~a)"
              (to-simple-lambda (car body) env)
              (to-simple-lambda (car (cdr body)) env))
          (format nil "λ~a.~a"
            (lookup (cons (lambdaarg-top body) env) (lambdaarg-top body))
            (to-simple-lambda (lambdabody body) (cons (lambdaarg-top body) env)))))))


(defun compile-to-simple-lambda (expr)
  (to-simple-lambda (curry expr)))

(defmacro compile-to-simple-lambda-lazy (expr-lazy)
  `(compile-to-simple-lambda (macroexpand-lazy ,expr-lazy)))

;; (def-lazy "A" (succ 64))
;; (defmacro def-alphabet-lazy ()
;;   (let* ((alphabet (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `list))
;;         (alphazip (mapcar #'list (cdr alphabet) alphabet))
;;         (expr (map 'list #'(lambda (z) `(def-lazy ,(string (car z)) (succ ,(string (car (cdr z)))))) alphazip)))
;;     `(progn ,@expr)))
;; (def-alphabet-lazy)
;; ;; (def-lazy "B" (succ "A"))

;; (def-lazy "a" (succ (+ 64 32)))
;; (defmacro def-alphabet-lazy ()
;;   (let* ((alphabet (coerce "abcdefghijklmnopqrstuvwxyz" `list))
;;         (alphazip (mapcar #'list (cdr alphabet) alphabet))
;;         (expr (map 'list #'(lambda (z) `(def-lazy ,(string (car z)) (succ ,(string (car (cdr z)))))) alphazip)))
;;     `(progn ,@expr)))
;; (def-alphabet-lazy)
