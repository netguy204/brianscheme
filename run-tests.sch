(require "tests/math-test.sch")
(require "tests/clos-test.sch")
(require "tests/random-test.sch")
(require "tests/lang-test.sch")
(require "tests/hash-test.sch")
(require "tests/list-test.sch")

(if (combine-results
     (basic-arith)
     (clos-test)
     (lang-test)
     (mersenne-test)
     (hash-table-test)
     (list-test))

    (display "all tests pass!\n")
    (display "there were failures\n"))

(exit 0)
