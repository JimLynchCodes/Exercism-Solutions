(ns two-fer)
(ns two-fer
  (:require [clojure.spec.alpha :as s]))

(s/fdef two-fer
  :args nil
  :ret string?)

(defn two-fer
  "Returns the \"One For\" string with the name passed in, otherwise \"me\"."
  ([] "One for you, one for me.")
  ([name] (str "One for " name ", one for me.")))