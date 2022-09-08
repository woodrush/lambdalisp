;; Execute at the same level as ./lambdalisp.cl for resolving nested `load`s
(load "./lambdalisp.cl")

;; Override the prelude definition with the Lazy K optimized version
(load "./targets/def-prelude-chars-lazyk.cl")
(load "./targets/def-prelude-lazyk.cl")
(load "./targets/blc-lazyk-ulamb-wrapper.cl")

(format t (compile-to-ski-lazy (blc-to-lazyk-wrapper main)))
