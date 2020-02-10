(ns grade-school)

(defn school?
  "predicate fn for :pre validation that an object is a valid school data structure, ie. a map with an integer key (grade value) and array of strings (student names). Note that a grade is a valid \"school\" object with just a single key and value."
  [school]
  (or (= {})
      (and (vector? school)
           (every? int? (keys school))
           (every? #(every? string? %) (first (vals school))))))

(defn grade
  "returns a vector of student names for a single grade."
  [school grade]
  {:pre [(school? school) (int? grade)]
   :post [(every? string? %)]}
  (get school grade []))

(defn add
  "adds a student to the end of the student names vector for the given grade."
  [school name n]
  {:pre [(school? school) (string? name) (int? n)]
   :post [(school? %)]}
  (let [students (grade school n)]
    (assoc school n (conj students name))))

(defn sorted
  "takes a school map and returns a sorted map by grade with student names sorted alphabetically."
  [school]
  {:pre [(school? school)]
   :post [(school? %)]}
  (->> school
       (map (fn [[key value]]
              [key (sort value)]))
       (into (sorted-map))))
