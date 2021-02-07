(ns two-fer)

(def line_beginning "One for ")
(def line_ending ", one for me.")
(def default_name "you")

(defn two-fer
  ([name]
   (str line_beginning name line_ending))
  ([]
   (str line_beginning default_name line_ending)))
