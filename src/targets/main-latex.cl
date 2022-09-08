;; Execute at the same level as ./lambdalisp.cl for resolving nested `load`s
(load "./lambdalisp.cl")

(format t (compile-to-simple-lambda-lazy main))
