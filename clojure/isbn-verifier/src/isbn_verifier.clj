(ns isbn-verifier
  (:require [clojure.string :refer [replace]]))

(defn- verify-mod-zero
  "takes 10 numbers"
  [seq]
  true
  )

(defn isbn? [isbn]

  (let [dash-stripped (remove #{\-} isbn)
        char-count (count dash-stripped)
        ;; x-replaced (replace dash-stripped \X 1)

        ;; no-x (replace dash-stripped \X \1)
        ; is-valid (valid-)
        ]

    dash-stripped

    (and (= 10 char-count)
         (verify-mod-zero dash-stripped)))
    ;; no-x))
  )