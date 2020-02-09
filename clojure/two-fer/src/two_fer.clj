(ns two-fer)

(defn two-fer
  "Returns the \"One For\" string with the name passed in, otherwise \"you\"."
  ([] (two-fer "you"))
  ([name] (str "One for " name ", one for me.")))