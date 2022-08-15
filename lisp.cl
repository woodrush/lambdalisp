(load "./lazy.cl")


(def-lazy "(" (+ 32 8))
(def-lazy ")" (succ "("))
(def-lazy " " 32)
(def-lazy "\\n" (+ 8 2))

(def-lazy "A" (succ 64))
(defmacro def-alphabet-lazy ()
  (let* ((alphabet (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `list))
        (alphazip (mapcar #'list (cdr alphabet) alphabet))
        (expr (map 'list #'(lambda (z) `(def-lazy ,(string (car z)) (succ ,(string (car (cdr z)))))) alphazip)))
    `(progn ,@expr)))
(def-alphabet-lazy)

(def-lazy "a" (succ (+ 64 32)))
(defmacro def-alphabet-lazy ()
  (let* ((alphabet (coerce "abcdefghijklmnopqrstuvwxyz" `list))
        (alphazip (mapcar #'list (cdr alphabet) alphabet))
        (expr (map 'list #'(lambda (z) `(def-lazy ,(string (car z)) (succ ,(string (car (cdr z)))))) alphazip)))
    `(progn ,@expr)))
(def-alphabet-lazy)


;; (def-lazy "B" (succ "A"))
;; (def-lazy "C" (succ "B"))
;; (def-lazy "D" (succ "C"))


(defrec-lazy map (f list)
  (if (isnil list)
    nil
    (cons (f (car list)) (map f (cdr list)))))

(defrec-lazy append-element (l item)
  (if (isnil l) (cons item nil) (cons (car l) (append-element (cdr l) item))))

(defrec-lazy append-list (l item)
  (if (isnil l) item (cons (car l) (append-list (cdr l) item))))


(defmacro-lazy typematch (tvar t0 t1)
  `(,tvar ,t0 ,t1))

(defun-lazy type-atom (t0 t1) t0)
(defun-lazy type-list (t0 t1) t1)

(def-lazy char2stream
  cons)

(defun-lazy catstream (stream1 stream2)
  (lambda (stream) (stream1 (stream2 stream))))

(def-lazy nullstream (lambda (x) x))

(defrec-lazy catstreamlist (streamlist)
  (if (isnil streamlist)
    nullstream
    (catstream (car streamlist) (catstreamlist (cdr streamlist)))))

(defrec-lazy str2stream (s)
  (catstreamlist (map char2stream s)))

(def-lazy typeof car)
(def-lazy valueof cdr)

(defun-lazy car-data (data)
  (car (valueof data)))

(defun-lazy cdr-data (data)
  (cdr (valueof data)))

(defun-lazy cons-data (data)
  (lambda (x) (cons type-list (cons data x))))

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

(defrec-lazy printexpr (atomenv expr)
  (typematch (typeof expr)
    ;; atom
    (str2stream (car ((valueof expr) cdr atomenv)))
    ;; list
    (catstreamlist
      (list (char2stream "(")
            ((letrec-lazy interleave-space (slist)
                (if (isnil (cdr slist))
                  (car slist)
                  (catstreamlist (list (car slist) (char2stream " ") (interleave-space (cdr slist))))))
              (map-data-as-baselist (printexpr atomenv) expr))
            (char2stream ")")))))

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

(defrec-lazy read-expr (stdin atomenv)
  ;; (cons (atom* 0) (cons stdin atomenv))
  (let ((read-list
          (letrec-lazy read-list (stdin atomenv curexpr)
            (let ((stdin (read-skip-whitespace stdin))
                  (c (car stdin)))
              (cond ((= ")" c)
                      (cons curexpr (cons (cdr stdin) atomenv)))
                    (t
                      (let ((ret (read-expr stdin atomenv))
                            (ret-expr (car ret))
                            (stdin (car (cdr ret)))
                            (atomenv (cdr (cdr ret))))
                        (read-list stdin atomenv (append-element-data curexpr ret-expr))
                        )))))))
    (let ((stdin (read-skip-whitespace stdin)) (c (car stdin)))
            (cond ((= "(" c)
                    (read-list (cdr stdin) atomenv nil))
                  (t
                    (read-atom stdin atomenv)
                    ))))
                    )

;; (defrec-lazy read-expr (stdin atomenv)
;;   (let ((stdin (read-skip-whitespace stdin)) (c (car stdin)))
;;     (cond ((= "(" c)
;;             (read-list stdin atomenv))
;;           (t
;;             (read-atom stdin atomenv)))))

;; (defrec-lazy read-list (curlist stdin atomenv)
;;   (let ((stdin (read-skip-whitespace stdin)) (c (car stdin)))
;;     (cond ((or (= ")" c) (= 256 c))
;;             (cons (cons type-list curlist) (cons atomenv (cdr stdin))))
;;           ((= "(" c)
;;             (let ((readoutstate (read-list nil (cdr stdin) atomenv))
;;                   (readoutlist (car readoutstate))
;;                   (atomenv (car (cdr readoutstate)))
;;                   (stdin (cdr (cdr readoutstate))))
;;               (read-list (append-element curlist readoutlist) stdin atomenv)))
;;           (t
;;             (let ((readoutstate (read-atom nullstream stdin))
;;                   (readoutstream (car readoutstate))
;;                   (stdin (cdr readoutstate))
;;                   (ret-atomlookup (get-atomindex-env atomenv (readoutstream nil)))
;;                   (ret-atom (atom* (car ret-atomlookup)))
;;                   (ret-atomenv (cdr ret-atomlookup)))
;;               ;; (cons (atom* 1) nil)
;;               (read-list (append-element curlist
;;               ;; (atom* 0)
;;               ret-atom
;;               )
;;               stdin
;;               ret-atomenv)
;;               )
;;               )
;;               ))
;;     )

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



(def-lazy initialenv
  (list
    (list "q" "u" "o" "t" "e")
    (list "c" "a" "r")
    (list "c" "d" "r")
    (list "c" "o" "n" "s")
    (list "a" "t" "o" "m")
    (list "e" "q")
    (list "c" "o" "n" "d")
    (list "p" "r" "i" "n" "t")
    (list "r" "e" "a" "d")
    (list "t")))

(defrec-lazy eval (expr varenv atomenv stdin stdoutstream)
  (typematch (typeof expr)
    ;; atom
    nil
    ;; list
    (let ((head (car-data expr))
          (tail (cdr-data expr))
          (head-index (valueof head)))
      (typematch (typeof head)
        ;; atom
        (cond
          ;; quote
          ((= head-index 0)
            (car-data tail))
          ;; car
          ((= head-index 1)
            (car-data (eval tail varenv atomenv stdin stdoutstream)))
          ;; cdr
          ((= head-index 2)
            (cdr-data (eval tail varenv atomenv stdin stdoutstream)))
          ;; ;; cons
          ;; ((= head-index 3)
          ;;   )
          (t
            nil)
          )
        ;; list
        nil
        ))
      ))

(defun-lazy main (stdin)
  (let ((env initialenv)
        (ret-parse (read-expr stdin env))
        (stdin (car (cdr ret-parse)))
        (env (cdr (cdr ret-parse)))
        (expr (car ret-parse))
        )
    ((printexpr
        env
        ;; (atom* 0)
        ;; expr
        (eval expr nil env stdin nil)
        )
     (inflist 256))
    )

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

;; (defun-lazy main (stdin)
;;   (lazystr2blcstr (program (blcstr2lazystr stdin))))

;; (print (compile-to-blc-lazy main))
