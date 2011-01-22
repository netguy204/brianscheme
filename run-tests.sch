(require "tests/math-test.sch")
;;(require "tests/vm-test.sch")
(require "tests/clos-test.sch")
(require "tests/random-test.sch")

(if (combine-results
     (basic-arith)
     (clos-test)
     (mersenne-test))
;;     (vm-test))
    (display "all tests pass!\n")
    (display "there were failures\n"))

(exit 0)
