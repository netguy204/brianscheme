(require "tests/math-test.sch")
(require "tests/clos-test.sch")
(require "tests/random-test.sch")
(require "tests/lang-test.sch")

(if (combine-results
     (basic-arith)
     (clos-test)
     (lang-test)
     (mersenne-test))

    (display "all tests pass!\n")
    (display "there were failures\n"))

(exit 0)
