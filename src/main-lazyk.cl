;; Execute at the same level as ./lambdalisp.cl for resolving nested `load`s
(load "./lambdalisp.cl")
(load "./chars-lazyk.cl")
(load "./blc-lazyk-ulamb-wrapper.cl")

;; Override the prelude definition with the Lazy K optimized version
(load "./build/def-prelude-lazyk.cl")


(format t (compile-to-ski-lazy (blc-to-lazyk-wrapper main)))
