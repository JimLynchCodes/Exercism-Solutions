(ns armstrong-numbers
  (:require [clojure.string :refer [split]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            ; [clojure.spec.test.alpha/instrument :as stest]
            ))


(defn power [base exponent]
  (reduce * (repeat exponent base)))

(s/fdef power
  :args (s/cat :base (s/int-in -9 10) 
               :exponent (s/int-in 0 20))
  :ret int?)

(stest/instrument `power)
(stest/summarize-results (stest/check `power))


(s/fdef armstrong?
  :args (s/cat :num string? :foo :int?)
  :ret string?)

(defn armstrong?
  "Returns true if the input integer is equal to the sum of each of 
   its digits raised to the power of the total number of digits, 
   otherwise false."
  [num]
  (as-> num n
    (str n)
    (split n #"")
    (map #(Integer. %) n)
    (map #(power % (count n)) n)
    (reduce + n)
    (= n num)))
