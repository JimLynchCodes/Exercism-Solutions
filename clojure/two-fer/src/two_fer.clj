(ns two-fer)

(defn two-fer
  "Returns the \"One For\" string with the name passed in, otherwise \"you\"."
  ([]
   {:pre []
    :post [(string? %)]}
   (two-fer "you"))
  ([name]
   {:pre [(string? name)]
    :post [(string? %)]}
   (str "One for " name ", one for me.")))