(ns halfer-test
  (:require [clojure.test :refer [deftest is]]
            halfer))

(deftest nice-halving
  (is (= 2 (halfer/half 4))))

(deftest throws-error-on-odd-number
  (is (thrown? AssertionError (halfer/half 5))))
