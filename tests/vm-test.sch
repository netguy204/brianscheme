(load "compiler.sch")

(set! map2 ((compiler (compile-together 'map 'mapr 'reverse))))
(set! fn ((compiler '(lambda (x) (+ x 1)))))

(compiled-environment map2)
(compiled-environment fn)

'invocation
(map2 fn '(1 2 3))

