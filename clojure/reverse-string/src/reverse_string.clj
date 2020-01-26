(ns reverse-string
  (:require [clojure.string :as str]))

(defn reverse-string [s]
  {:pre  [(string? s)]
   :post [(string? %)]}
  "Reverse a string by looping over it and pushing each character into a list."
  (->> s
       (reduce conj ())
       (str/join)))

