(load "./lazy.cl")


(def-lazy "(" (+ 32 8))
(def-lazy ")" (succ "("))
(def-lazy " " 32)
(def-lazy "." (+ (+ (+ 32 8) 4) 2))
(def-lazy ">" (+ (+ (+ (+ 32 16) 8) 4) 2))
(def-lazy "\\n" (+ 8 2))

(def-lazy 3 (succ 2))
(def-lazy 5 (succ 4))
(def-lazy 6 (+ 4 2))
(def-lazy 7 (succ 6))
(def-lazy 9 (succ 8))

(def-lazy "A" (succ 64))
(defmacro def-alphabet-lazy ()
  (let* ((alphabet (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `list))
        (alphazip (mapcar #'list (cdr alphabet) alphabet))
        (expr (map 'list #'(lambda (z) `(def-lazy ,(string (car z)) (succ ,(string (car (cdr z)))))) alphazip)))
    `(progn ,@expr)))
(def-alphabet-lazy)
;; (def-lazy "B" (succ "A"))

(def-lazy "a" (succ (+ 64 32)))
(defmacro def-alphabet-lazy ()
  (let* ((alphabet (coerce "abcdefghijklmnopqrstuvwxyz" `list))
        (alphazip (mapcar #'list (cdr alphabet) alphabet))
        (expr (map 'list #'(lambda (z) `(def-lazy ,(string (car z)) (succ ,(string (car (cdr z)))))) alphazip)))
    `(progn ,@expr)))
(def-alphabet-lazy)



(defrec-lazy map (f list)
  (if (isnil list)
    nil
    (cons (f (car list)) (map f (cdr list)))))

(defrec-lazy append-element (l item)
  (if (isnil l) (cons item nil) (cons (car l) (append-element (cdr l) item))))

(defrec-lazy append-list (l item)
  (if (isnil l) item (cons (car l) (append-list (cdr l) item))))


(defmacro-lazy typematch (expr atomcase listcase nilcase)
  `(if (isnil ,expr)
      ,nilcase
      ((typeof ,expr) ,atomcase ,listcase)))

(defun-lazy isatom (expr)
  (typematch expr
    ;; atom
    t
    ;; list
    nil
    ;; nil
    t))

(defun-lazy truth-data (expr)
  (if expr t-data nil))

(defun-lazy type-atom (t0 t1) t0)
(defun-lazy type-list (t0 t1) t1)

(def-lazy char2stream
  cons)

; (defun-lazy catstream (stream1 stream2)
;   (lambda (stream) (stream1 (stream2 stream))))

(defmacro-lazy catstream (&rest args)
  `(lambda (stream) (-> stream ,@(reverse args))))

(def-lazy nullstream (lambda (x) x))

; (defrec-lazy catstreamlist (streamlist)
;   (if (isnil streamlist)
;     nullstream
;     (catstream (car streamlist) (catstreamlist (cdr streamlist)))))

(defun-lazy str2stream (s)
  ; (catstreamlist (map char2stream s))
  (append-list s)
  )

(def-lazy typeof car)
(def-lazy valueof cdr)

(defmacro-lazy car-data (data)
  `(car (valueof ,data)))

(defmacro-lazy cdr-data (data)
  `(cdr (valueof ,data)))

(defmacro-lazy cons-data (x y)
  `(cons type-list (cons ,x ,y)))

(defmacro-lazy atom* (value)
  `(cons type-atom ,value))

(defmacro-lazy list* (arg &rest args)
  (if (not args)
    `(cons-data ,arg nil)
    `(cons-data ,arg (list* ,@args))))

(defrec-lazy append-element-data (l item)
  (if (isnil l) (cons-data item nil) (cons-data (car-data l) (append-element-data (cdr-data l) item))))

(defrec-lazy map-data-as-baselist (f data)
  (cond ((isnil data) nil)
        (t (cons (f (car-data data))
                 (map-data-as-baselist f (cdr-data data))))))

(defun-lazy printatom (atomenv expr cont)
  (append-list (car ((valueof expr) cdr atomenv)) cont)
  ;; (cons "A" cont)
  ;; (cons (car (car ((valueof expr) cdr atomenv))) cont)
  )

(defrec-lazy printexpr-helper (atomenv expr mode cont)
  (if mode
    (typematch expr
      ;; atom
      (printatom atomenv expr cont)
      ;; list
      (cons "(" (printexpr-helper atomenv expr nil (cons ")" cont)))
      ;; nil
      (cons "(" (cons ")" cont)))
    (printexpr-helper atomenv (car-data expr) t
      (typematch (cdr-data expr)
        ;; atom
        (cons " " (cons "." (cons " " (printatom atomenv (cdr-data expr) cont))))
        ;; list
        (cons " " (printexpr-helper atomenv (cdr-data expr) nil cont))
        ;; nil
        cont))
    )
  )

(defun-lazy printexpr (atomenv expr cont)
  (printexpr-helper atomenv expr t cont))

(defrec-lazy read-string (curstream stdin)
  (let ((c (car stdin)))
    (cond
          ;; ((or (= " " c) (= "\\n" c))
          ;;   (cons curstream (cdr stdin)))
          ((or (= "(" c) (= ")" c) (= 256 c) (= " " c) (= "\\n" c))
            (cons curstream stdin))
          (t
            (read-string (catstream curstream (char2stream (car stdin))) (cdr stdin))))))

(defun-lazy get-atomindex-env (atomenv str)
  ((letrec-lazy get-atomindex-env-helper (cur-atomenv n)
    (cond ((isnil cur-atomenv)
            (cons n (append-element atomenv str)))
          ((stringeq (car cur-atomenv) str)
            (cons n atomenv))
          (t
            (get-atomindex-env-helper (cdr cur-atomenv) (succ n))))
            )
   atomenv 0))

(defun-lazy read-atom (stdin atomenv)
  (let ((ret (read-string nullstream stdin))
        (ret-stream (car ret))
        (stdin (cdr ret))
        (ret-atomlookup (get-atomindex-env atomenv (ret-stream nil)))
        (ret-atom (atom* (car ret-atomlookup)))
        (atomenv (cdr ret-atomlookup)))
    (cons ret-atom (cons stdin atomenv))))

(defrec-lazy stringeq (s1 s2)
  (cond ((and (isnil s1) (isnil s2))
          t)
        ((or  (and (not (isnil s1)) (isnil s2))
              (and (isnil s1) (not (isnil s2))))
          nil)
        ((= (car s1) (car s2))
          (stringeq (cdr s1) (cdr s2)))
        (t
          nil)))


;; untested
(defrec-lazy read-skip-until-newline (stdin)
  (let ((c (car stdin)))
    (if (= "\\n" c)
      (cdr stdin)
      (read-skip-until-newline (cdr stdin)))))

(defrec-lazy read-skip-whitespace (stdin)
  (let ((c (car stdin)))
    (cond ((or (= " " c) (= "\\n" c))
            (read-skip-whitespace (cdr stdin)))
          (t
            stdin))))

(defrec-lazy reverse-base2data (list curlist)
  (if (isnil list) curlist (reverse-base2data (cdr list) (cons-data (car list) curlist))))

(defrec-lazy read-expr (stdin atomenv)
  (let ((read-list
          (letrec-lazy read-list (stdin atomenv curexpr)
            (let ((stdin (read-skip-whitespace stdin))
                  (c (car stdin)))
              (cond ((= ")" c)
                      (cons (reverse-base2data curexpr nil) (cons (cdr stdin) atomenv)))
                    (t
                      (let ((ret (read-expr stdin atomenv))
                            (ret-expr (car ret))
                            (stdin (car (cdr ret)))
                            (atomenv (cdr (cdr ret))))
                        (read-list stdin atomenv (cons ret-expr curexpr))
                        )))))))
    (let ((stdin (read-skip-whitespace stdin)) (c (car stdin)))
            (cond ((= "(" c)
                    (read-list (cdr stdin) atomenv nil))
                  (t
                    (read-atom stdin atomenv)
                    )))))


; (def-lazy initial-atomenv
;   (list
;     (list "q" "u" "o" "t" "e")
;     (list "c" "a" "r")
;     (list "c" "d" "r")
;     (list "c" "o" "n" "s")
;     (list "a" "t" "o" "m")
;     (list "e" "q")
;     (list "c" "o" "n" "d")
;     (list "p" "r" "i" "n" "t")
;     (list "r" "e" "a" "d")
;     (list "t")))

(def-lazy initial-atomenv
  (list
    (list "Q" "U" "O" "T" "E")
    (list "C" "A" "R")
    (list "C" "D" "R")
    (list "C" "O" "N" "S")
    (list "A" "T" "O" "M")
    (list "E" "Q")
    (list "C" "O" "N" "D")
    (list "P" "R" "I" "N" "T")
    (list "R" "E" "A" "D")
    (list "T")))

(def-lazy initial-varenv
  (list (cons 9 (atom* 9))))

(def-lazy t-data
  (atom* 9))

(defmacro-lazy new-evalret (expr varenv atomenv stdin stdout)
  `(list ,expr ,varenv ,atomenv ,stdin ,stdout))



(defmacro-lazy let-parse-evalret-raw (expr ret varenv atomenv stdin stdout body)
  `(let ((evalret ,expr)
         (,ret (-> evalret car))
         (,varenv (-> evalret cdr car))
         (,atomenv (-> evalret cdr cdr car))
         (,stdin (-> evalret cdr cdr cdr car))
         (,stdout (-> evalret cdr cdr cdr cdr car)))
     ,body))

(defmacro-lazy let-parse-evalret (expr ret varenv atomenv stdin stdout body)
  `(let-parse-evalret-raw (eval ,expr ,varenv ,atomenv ,stdin ,stdout)
      ,ret ,varenv ,atomenv ,stdin ,stdout ,body))


(defrec-lazy eval-cond (clauselist varenv atomenv stdin stdout)
  (let ((carclause (-> clauselist car-data))
        (carcond (-> carclause car-data))
        (carbody (-> carclause cdr-data car-data)))
    (let-parse-evalret carcond carcond-eval varenv atomenv stdin stdout
      (if (isnil carcond-eval)
          (eval-cond (cdr-data clauselist) varenv atomenv stdin stdout)
          (eval carbody varenv atomenv stdin stdout)))))

(defrec-lazy varenv-lookup (varenv varval)
  (let ((pair (car varenv))
        (evarval (car pair))
        (ebody (cdr pair)))
    (cond ((isnil varenv)
            nil)
          ((= varval evarval)
            ebody)
          (t
            (varenv-lookup (cdr varenv) varval)))))

(defrec-lazy eval-map-base (lexpr curexpr varenv atomenv stdin stdout)
  (cond ((isnil lexpr)
          (new-evalret curexpr varenv atomenv stdin stdout))
        (t
          (let-parse-evalret (car-data lexpr) ret varenv atomenv stdin stdout
            (eval-map-base (cdr-data lexpr)
            ;; This can be changed to (cons ret curexpr) if the arguments are independent.
            (append-element curexpr ret)
            varenv atomenv stdin stdout))
          ;; (cons (eval (car-data lexpr) varenv atomenv stdin stdout)
          ;;       (eval-map-base (cdr-data lexpr) varenv atomenv stdin stdout))
                )))

(defrec-lazy prepend-envzip (argnames evargs env)
  (cond ((or (isnil argnames) (isnil evargs))
          env)
        (t
          (cons (cons (valueof (car-data argnames)) (car evargs))
                (prepend-envzip (cdr-data argnames) (cdr evargs) env)))))

(defun-lazy eval-lambda (lambdaexpr callargs varenv atomenv stdin stdout)
  (let ((argnames (-> lambdaexpr cdr-data car-data))
        (lambdabody (-> lambdaexpr cdr-data cdr-data car-data)))
        ;; (new-evalret (atom* 2) varenv atomenv stdin stdout)
    (let-parse-evalret-raw (eval-map-base callargs nil varenv atomenv stdin stdout)
      evargs varenv atomenv stdin stdout
      ;; (new-evalret (atom* 2) varenv atomenv stdin stdout)
      (eval lambdabody (prepend-envzip argnames evargs varenv) atomenv stdin stdout)
      )
      ))


(defrec-lazy eval (expr varenv atomenv stdin stdout)
  (typematch expr
    ;; atom
    (new-evalret (varenv-lookup varenv (valueof expr))
                 varenv atomenv stdin stdout)
    ;; list
    (let ((head (car-data expr))
          (tail (cdr-data expr))
          (head-index (valueof head)))
      (typematch head
        ;; atom
        (cond
          ((<= 9 head-index)
            (let-parse-evalret head lambdaexpr varenv atomenv stdin stdout
              (eval-lambda lambdaexpr tail varenv atomenv stdin stdout))
            ;; (let ((lambdaexpr (eval head varenv atomenv stdin stdout))
            ;;       (callargs tail))
            ;;   (eval-lambda lambdaexpr callargs varenv atomenv stdin stdout))
              )
          (t
            (->
              (list
                ;; quote
                (new-evalret (car-data tail)
                             varenv atomenv stdin stdout)
                ;; car
                (let-parse-evalret (car-data tail) ret varenv atomenv stdin stdout
                  (new-evalret (car-data ret)
                               varenv atomenv stdin stdout))
                ;; cdr
                (let-parse-evalret (car-data tail) ret varenv atomenv stdin stdout
                  (new-evalret (cdr-data ret)
                               varenv atomenv stdin stdout))
                ;; cons
                (let-parse-evalret (car-data tail) c1 varenv atomenv stdin stdout
                  (let-parse-evalret (-> tail cdr-data car-data) c2 varenv atomenv stdin stdout
                    (new-evalret (cons-data c1 c2)
                                 varenv atomenv stdin stdout)))
                ;; atom
                (let-parse-evalret (car-data tail) ret varenv atomenv stdin stdout
                  (new-evalret (truth-data (isatom ret))
                               varenv atomenv stdin stdout))
                ;; eq
                (let-parse-evalret (car-data tail) x varenv atomenv stdin stdout
                  (let-parse-evalret (-> tail cdr-data car-data) y varenv atomenv stdin stdout
                    (new-evalret
                      (cond ((or (not (isatom x)) (not (isatom y)))
                              nil)
                            (t
                              (truth-data (= (valueof x) (valueof y)))))
                      varenv atomenv stdin stdout)))
                ;; cond
                (eval-cond tail varenv atomenv stdin stdout)
                ;; print
                (let-parse-evalret (car-data tail) ret varenv atomenv stdin stdout
                  (new-evalret ret
                               varenv atomenv stdin
                                (append-list stdout (printexpr atomenv ret (list "\\n")))))
                ;; read
                (let ((ret-parse (read-expr stdin atomenv))
                      (expr (-> ret-parse car))
                      (stdin (-> ret-parse cdr car))
                      (atomenv (-> ret-parse cdr cdr)))
                  (new-evalret expr
                               varenv atomenv stdin stdout))
                )
              (head-index cdr)
              car))
          )
        ;; list: parse as lambda
        (let ((lambdaexpr head)
              (callargs tail))
          (eval-lambda lambdaexpr callargs varenv atomenv stdin stdout)
          ;; (new-evalret (atom* 1) varenv atomenv stdin stdout)
          )
        ;; nil
        (new-evalret nil varenv atomenv stdin stdout)
        ))
    ;; nil
    (new-evalret nil varenv atomenv stdin stdout)))

(defrec-lazy repl (varenv atomenv stdin stdout)
  (cons ">" (cons " " (if (= 256 (car stdin))
    (inflist 256)
    (let (
        ;; (atomenv initial-atomenv)
        (ret-parse (read-expr stdin atomenv))
        (stdin (car (cdr ret-parse)))
        (atomenv (cdr (cdr ret-parse)))
        (expr (car ret-parse))
        ;; (ret-eval (eval expr varenv atomenv stdin stdout))
        )
      (let-parse-evalret expr ret-eval varenv atomenv stdin stdout
        (append-list stdout (printexpr
              atomenv
              ;; (atom* 0)
              ;; (cons-data (cons-data (atom* 1) (atom* 1)) expr)
              ret-eval
              (cons "\\n" (repl varenv atomenv stdin stdout))
              ))
        
      )
    )))))

(defun-lazy main (stdin)
  (repl initial-varenv initial-atomenv stdin nil)

  ;; (if (stringeq (list "A" "A") (list "A" "A"))
  ;;   (list "A" "A" 256 256)
  ;;   (list "A" "B" 256 256))

;; (list (list "A") (list "B") (list "A" "B" "C") (list "C"))


  ;; (cdr (read-atom nullstream stdin))

  ;; ((printexpr
  ;;     (list (list "A") (list "A" "B" "C") (list "C"))
  ;;     ;; (atom* 1)
  ;;     (list*  (atom* 2)
  ;;             (list* (atom* 1) (atom* 2) (atom* 0) (atom* 1))
  ;;             (atom* 0)
  ;;             (atom* 1))
  ;;       )
  ;;  (inflist 256))

  ;; ((printexpr
  ;;     (list (char2stream "A") (str2stream (list "A" "B" "C")) (char2stream "C"))
  ;;     (list*  (atom* 2)
  ;;             (list* (atom* 1) (atom* 2) (atom* 0))
  ;;             (atom* 0))
  ;;       )
  ;;  (inflist 256))
  )

;; (print (macroexpand-lazy main))
(format t (compile-to-ski-lazy main))
;; (format t (compile-to-blc-lazy main))

;; (defun-lazy main (stdin)
;;   (lazystr2blcstr (program (blcstr2lazystr stdin))))

;; (print (compile-to-blc-lazy main))
