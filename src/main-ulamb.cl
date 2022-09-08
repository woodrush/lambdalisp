;; Execute at the same level as ./lambdalisp.cl for resolving nested `load`s
(load "./lambdalisp.cl")
(load "./lazyk-ulamb-blc-wrapper.cl")

(format t (compile-to-blc-lazy (blc-to-ulamb-wrapper main)))
