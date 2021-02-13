(ns cljs-test-runner.gen
       (:require [doo.runner :refer-macros [doo-tests]] [hello-world-test]))
  (defn var->sym [var]
    (symbol (:ns (meta var)) (:name (meta var))))

  (defn var-filter
    [{:keys [var include exclude]}]
    (let [test-specific (if var
                          (comp var var->sym)
                          (constantly true))
          test-inclusion (if include
                           #((apply some-fn include) (meta %))
                           (constantly true))
          test-exclusion (if exclude
                           #((complement (apply some-fn exclude)) (meta %))
                           (constantly true))]
      #(and (test-specific %)
            (test-inclusion %)
            (test-exclusion %))))

  (defn filter-vars! [ns-syms filter-fn]
    (doseq [ns-sym ns-syms]
      (doseq [[_ var] ns-sym]
        (when (:test (meta var))
          (when (not (filter-fn var))
            (set! (.-cljs$lang$test @var) nil))))))
  (filter-vars! [(ns-publics 'hello-world-test)]
        (var-filter {:var nil
                     :include nil
                     :exclude nil}))
(doo-tests 'hello-world-test)