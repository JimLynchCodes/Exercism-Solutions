(ns say-hello-test
  (:require [clojure.test :refer [deftest is]]
            say-hello))

(deftest say-hello-test
  (is (= "Hello there, Barack Obama!" (say-hello/hello "Barack" "Obama"))))
