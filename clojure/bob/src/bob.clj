(ns bob
  (:require [clojure.string :as str]))

(defn- nothing? [s]
  (= "" (str/trim s)))

(defn- yelled? [s]
  (let [trimmed (str/trim s)]
    (and
     (re-find #"[A-Z]" trimmed)
     (= trimmed (str/upper-case trimmed)))))

(defn- question? [s]
  (->> s
       (str/trim)
       (last)
       (= \?)))

(defn- yelled-question? [s]
  (and (yelled? s) (question? s)))

(defn response-for
  "Takes some text and returns the response of Bob."
  [s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (cond
    (yelled-question? s) "Calm down, I know what I'm doing!"
    (yelled? s) "Whoa, chill out!"
    (question? s) "Sure."
    (nothing? s) "Fine. Be that way!"
    :else "Whatever."))
