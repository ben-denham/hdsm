(ns hdsm.dataset.probabilistic-test
  (:use midje.sweet)
  (:require
   [hdsm.dataset.probabilistic :as probabilistic]
   [hdsm.dataset.base :refer [get-dataset-id-map]]
   [hdsm.utils :refer [percent map-vals]]))

(facts "about `possible-inputs`"
       (fact "it should return an empty list for 0 features"
             (probabilistic/possible-inputs 0) => '(()))
       (fact "it should return a list of 2 options for 1 feature"
             (set (probabilistic/possible-inputs 1)) => #{[true] [false]})
       (fact "it should return expected combinations for 2 features"
             (set (probabilistic/possible-inputs 2))
             => #{[true true] [true false] [false true] [false false]})
       (fact "it should return 8 combinations for 3 features"
             (count (probabilistic/possible-inputs 3)) => 8)
       (fact "it should return 16 combinations for 4 features"
             (count (probabilistic/possible-inputs 4)) => 16))

(defn- record-frequencies
  [dataset]
  (let [record-values (map :values (vals (get-dataset-id-map dataset)))
        record-count (count record-values)]
    (->> record-values
         (map vec)
         (frequencies)
         (map-vals #(->> (/ % record-count)
                         (float)
                         (format "%.2f"))))))

(facts "about `make-balanced-probabilistic-dataset`"
       (fact "it should return a balanced feature-set for a balanced class-fn"
             (record-frequencies
              (probabilistic/make-balanced-probabilistic-dataset
               2 (fn [f1 f2]
                   (or (and f1 f2)
                       (and (not f1) (not f2))))
               0 100000))
             => {[0 0 1] "0.25"
                 [0 1 0] "0.25"
                 [1 0 0] "0.25"
                 [1 1 1] "0.25"})
       (fact "it should return a balanced feature-set for an unbalanced class-fn"
             (record-frequencies
              (probabilistic/make-balanced-probabilistic-dataset
               2 (fn [f1 f2] (or f1 f2))
               0 100000))
             => {[0 0 0] "0.50"
                 [0 1 1] "0.17"
                 [1 0 1] "0.17"
                 [1 1 1] "0.17"})
       (fact "it should return the opposite classes than expected for 100% noise"
             (record-frequencies
              (probabilistic/make-balanced-probabilistic-dataset
               2 (fn [f1 f2] (or f1 f2))
               1 100000))
             => {[0 0 1] "0.50"
                 [0 1 0] "0.17"
                 [1 0 0] "0.17"
                 [1 1 0] "0.17"}))
