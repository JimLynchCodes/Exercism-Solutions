(ns say-hello
  (:require [clojure.spec.alpha :as s]))

(s/fdef hello
  :args (s/cat :first nil? :last int?)
  :ret string?)

; (s/fdef power
;   :args (s/cat :base int? :exponent int?)
;   :ret int?)

(defn hello [first last]
  (format "Hello there, %s %s!" first last))
