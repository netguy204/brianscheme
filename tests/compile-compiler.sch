(require 'compiler)

(replace-with-compiled comp)
(replace-with-compiled arg-count)
(replace-with-compiled comp-begin)
(replace-with-compiled comp-list)
(replace-with-compiled bytecode-literal?)
(replace-with-compiled comp-const)
(replace-with-compiled comp-var)
(replace-with-compiled false?)
(replace-with-compiled true?)
(replace-with-compiled comp-if)
(replace-with-compiled comp-funcall)
(replace-with-compiled primitive-procedure?)
; compound->lambda
(replace-with-compiled sym-is-syntax?)
; comp-macroexpand0

(replace-with-compiled make-fn)
(replace-with-compiled fn?)
(replace-with-compiled comp-lambda)
(replace-with-compiled gen-args)
(replace-with-compiled num-args)

; unsorted
(replace-with-compiled make-true-list)
(replace-with-compiled assemble)
(replace-with-compiled optimize)
(replace-with-compiled gen)
(replace-with-compiled gen-label)
(replace-with-compiled gen-set)
(replace-with-compiled in-env?)
(replace-with-compiled label?)
(replace-with-compiled arg1)
(replace-with-compiled set-arg1!)
(replace-with-compiled asm-first-pass)
(replace-with-compiled asm-second-pass)


(load "tests/vm-test.sch")
