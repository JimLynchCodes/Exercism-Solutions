(ns raindrops)

(def ^{:private true} sound-map {3 "Pling"
                                 5 "Plang"
                                 7 "Plong"})

(defn- raindrop-sound
  "if x is divisible by n, returns the sound of n from sound-map, otherwise returns empty string"
  [x n]
  {:pre [(int? x) (pos-int? n)]
   :post [(string? %)]}
  (if (= 0 (mod x n)) (sound-map n) ""))

(defn convert
  "converts input int to either a string of raindrop sounds or a stringified number."
  [x]
  {:pre [(pos-int? x)]
   :post [(string? %)]}
  (let [raindrop-sounds (str (raindrop-sound x 3)
                             (raindrop-sound x 5)
                             (raindrop-sound x 7))]
    (if (= raindrop-sounds "")
      (str x)
      raindrop-sounds)))


