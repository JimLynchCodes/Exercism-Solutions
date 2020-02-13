(ns halfer)

(defn half
  "splits an even integer in half"
  [x]
  {:pre  [(and (int? x)
               (even? x))]
   :post [int? %]}
  (/ x 2))
