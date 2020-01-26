(ns bob
  (:require [clojure.string :as str]))

(defn- yelled? [s]
  (and
   (re-find #"[A-Z]" s)
   (= s (str/upper-case s))))

(defn- question? [s]
  (->> s
       (last)
       (= \?)))

(defn- yelled-question? [s]
  (and (yelled? s) (question? s)))

(defn- nothing? [s]
  (= "" s))

(defn response-for
  "Takes some text and returns the response of Bob."
  [s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (let [trimmed (str/trim s)]
    (cond
      (yelled-question? trimmed) "Calm down, I know what I'm doing!"
      (yelled? trimmed) "Whoa, chill out!"
      (question? trimmed) "Sure."
      (nothing? trimmed) "Fine. Be that way!"
      :else "Whatever.")))
