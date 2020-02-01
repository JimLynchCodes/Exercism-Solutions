(ns rna-transcription)

(def ^{:private true} DNA-to-RNA
  "Store DNA to RNA mappings in a hash map."
  {\G \C
   \C \G
   \A \U
   \T \A})

(defn to-rna
  "Converts a string of DNA nucleotides to a string of RNA nucleotides."
  [dna]
  {:pre [(string? dna) (every? DNA-to-RNA dna)]
   :post [(string? %)]}
  (->> dna
       (map #(DNA-to-RNA %))
       (apply str)))