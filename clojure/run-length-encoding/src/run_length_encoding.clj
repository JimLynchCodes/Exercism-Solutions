(ns run-length-encoding
  (:require [clojure.string :as str]))

(defn run-length-encode
  "takes a string of repeated elements and returns a loss-less REL compressed string of the elements and elments counts."
  [s]
  {:pre  [(string? s)]
  ;  :post [(string? %)]}
   :post [(any? %)]}
  (let [result (->> s
                    (partition-by identity)
      ;  (map #(str (when-not (= 1 (count %)) (count %)) (first %)))
      ;  (apply str)))
                    )]
    
    (prn "result " result)
    result))

(defn- decode-chunk
  "takes vector from re-seq for a single number/letter pair (or a single char with no num) and returns decoded chunk.
  eg. the chunk: [3A' '3' 'A'] decodes to: 'AAA'"
  [[_ repeated-times element]]
  {:pre  [[(any? _) (string? repeated-times) (string? element)]]
   :post [(string? %)]}
  (if (str/blank? repeated-times)
    element
    (str/join (repeat (Integer/parseInt repeated-times) element))))

(defn run-length-decode
  "replaces RLE string of elements and consecutive element counts with the elements repeated for the specified number of times."
  [s]
  {:pre  [(string? s)]
   :post [(string? %)]}
  (->> s
       (re-seq #"(\d*)([a-zA-Z\s])")
       (mapcat decode-chunk)
       (str/join)))


