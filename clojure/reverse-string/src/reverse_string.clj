(ns reverse-string)

(defn reverse-string
  "Reverse a string by looping over it and pushing each character into a list."
  [s]
  {:pre  [(string? s)]
   :post [(string? %)]}
  (->> s
       (reduce conj ())
       (apply str)))

