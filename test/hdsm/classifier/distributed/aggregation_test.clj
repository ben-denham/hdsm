(ns hdsm.classifier.distributed.aggregation-test
  (:use midje.sweet)
  (:require
   [hdsm.classifier.distributed.aggregation
    :as aggregation]))

(facts "about `classification-without-sites`"
       (let [result {:candidate-results {0 {:class 0 :confidence 0.3}
                                         1 {:class 2 :confidence 0.5}
                                         2 {:class 1 :confidence 0.8}
                                         3 {:class 2 :confidence 0.6}
                                         4 {:class 0 :confidence 0.0}}}]
         (fact "It should return the class with the highest confidence in candidate-results."
               (aggregation/classification-without-sites result []) => {:class 1 :confidence 0.8})
         (fact "It should exclude sites with the given indexes."
               (aggregation/classification-without-sites result [2 3]) => {:class 2 :confidence 0.5})
         (fact "It should raise an exception when excluded site-ids not in map of site-results."
               (try
                 (aggregation/classification-without-sites result [5])
                 (catch Exception e (.getMessage e))) => "Some excluded site-ids not in map of site-results")))
