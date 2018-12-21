(ns hdsm.evaluation.timing-test
  (:use midje.sweet)
  (:require
   [hdsm.evaluation.timing :as timing]
   [hdsm.utils :refer [map-vals]]))

(defn- dummy-time
  [time]
  {:cpu-nano time
   :wall-nano time})

(defn- dummy-site-result
  [type time & {:keys [source-sites]}]
  {:site-type type
   :process-time (dummy-time time)
   :source-sites source-sites})

(defn- dummy-sites
  [& {:keys [p-sites t-sites]}]
  (merge (into {} (map vector p-sites (repeat {})))
         (map-vals #(hash-map :source-sites %)
                   (or t-sites {}))))

(defn- dummy-result
  [site-results-map]
  {:breakdown (->> site-results-map
                   (filter #(contains? #{:p-site :t-site}
                                       (:site-type (second %))))
                   (into {}))
   :candidate-results (->> site-results-map
                           (remove #(= :p-site (:site-type (second %))))
                           (into {}))})

(facts "about `result-trouble-critical-path-time`"
       (fact "it should return the slowest primary-site if there are only primary-sites"
             (timing/result-trouble-critical-path-time
              (dummy-result
               {0 (dummy-site-result :p-site 1)
                1 (dummy-site-result :p-site 3)
                2 (dummy-site-result :p-site 5)})
              (dummy-sites :p-sites [0 1 2]))
             => 5)
       (fact "it should ignore stacked-aggregator time"
             (timing/result-trouble-critical-path-time
              (dummy-result
               {0 (dummy-site-result :p-site 1)
                1 (dummy-site-result :p-site 3)
                2 (dummy-site-result :p-site 5)
                -1 (dummy-site-result :STACKED-CLASSIFIER 2)})
              (dummy-sites :p-sites [0 1 2]))
             => 5)
       (fact "it should add a series of trouble-sites and primary-sites"
             (timing/result-trouble-critical-path-time
              (dummy-result
               {0 (dummy-site-result :p-site 1)
                1 (dummy-site-result :p-site 3)
                2 (dummy-site-result :p-site 5)
                3 (dummy-site-result :t-site 7)
                4 (dummy-site-result :t-site 9)})
              (dummy-sites :p-sites [0 1 2]
                           :t-sites {3 [0 1]
                                     4 [2 3]}))
             => 19)
       (fact "it should account for multiple terminal sites"
             (timing/result-trouble-critical-path-time
              (dummy-result
                     {0 (dummy-site-result :p-site 1)
                      1 (dummy-site-result :p-site 3)
                      2 (dummy-site-result :p-site 11)
                      3 (dummy-site-result :t-site 7)})
              (dummy-sites :p-sites [0 1 2]
                           :t-sites {3 [0 1]}))
             => 11
             (timing/result-trouble-critical-path-time
              (dummy-result
               {0 (dummy-site-result :p-site 1)
                1 (dummy-site-result :p-site 3)
                2 (dummy-site-result :p-site 9)
                3 (dummy-site-result :t-site 7)})
              (dummy-sites :p-sites [0 1 2]
                           :t-sites {3 [0 1]}))
             => 10))

(facts "about `result-p-site-aggregation-critical-path-time`"
       (fact "it should add stacked-aggregator time and primary-site time"
             (timing/result-p-site-aggregation-critical-path-time
              (dummy-result
               {0 (dummy-site-result :p-site 3)
                -1 (dummy-site-result :STACKED-CLASSIFIER 2)})
              (dummy-sites :p-sites [0]))
             => 5)
       (fact "it should ignore aggregator time if it is not for a STACKED-CLASSIFIER"
             (timing/result-p-site-aggregation-critical-path-time
              (dummy-result
               {0 (dummy-site-result :p-site 3)
                -1 (dummy-site-result :NOT-STACKED-CLASSIFIER 2)})
              (dummy-sites :p-sites [0]))
             => 3)
       (fact "it should take the slowest primary-site time"
             (timing/result-p-site-aggregation-critical-path-time
              (dummy-result
               {0 (dummy-site-result :p-site 1)
                1 (dummy-site-result :p-site 3)
                2 (dummy-site-result :p-site 5)
                -1 (dummy-site-result :STACKED-CLASSIFIER 2)})
              (dummy-sites :p-sites [0 1 2]))
             => 7)
       (fact "it should ignore trouble-sites time"
             (timing/result-p-site-aggregation-critical-path-time
              (dummy-result
               {0 (dummy-site-result :p-site 1)
                1 (dummy-site-result :p-site 3)
                2 (dummy-site-result :p-site 5)
                3 (dummy-site-result :t-site 5)
                -1 (dummy-site-result :STACKED-CLASSIFIER 2)})
              (dummy-sites :p-sites [0 1 2]
                           :t-sites {4 [0 1]}))
             => 7))

(facts "about `results-sequence-finish-times`"
       (fact "it should return the slowest primary-site for a single result with only primary-sites"
             (timing/results-sequence-finish-times
              [(dummy-result
                {0 (dummy-site-result :p-site 1)
                 1 (dummy-site-result :p-site 3)
                 2 (dummy-site-result :p-site 5)})]
              (dummy-sites :p-sites [0 1 2]))
             => [5])
       (fact "it should offset finish times according to the time primary-sites become free"
             (timing/results-sequence-finish-times
              [(dummy-result
                {0 (dummy-site-result :p-site 1)
                 1 (dummy-site-result :p-site 3)
                 2 (dummy-site-result :p-site 5)}) ;; 2 finishes last, and starts at 0
               (dummy-result
                {0 (dummy-site-result :p-site 10) ;; 0 finishes last, but starts at 1
                 1 (dummy-site-result :p-site 2)
                 2 (dummy-site-result :p-site 6)})
               (dummy-result
                {0 (dummy-site-result :p-site 12)
                 1 (dummy-site-result :p-site 20) ;; 1 finishes last, but starts at 3 + 2 = 5
                 2 (dummy-site-result :p-site 4)})]
              (dummy-sites :p-sites [0 1 2]))
             => [5 11 25])
       (fact "it should take the time spent at trouble-sites into account"
             (timing/results-sequence-finish-times
              [(dummy-result
                {0 (dummy-site-result :p-site 10)
                 1 (dummy-site-result :p-site 5)
                 2 (dummy-site-result :p-site 15)
                 3 (dummy-site-result :p-site 10)
                 4 (dummy-site-result :t-site 20)
                 5 (dummy-site-result :t-site 10)})
               (dummy-result
                {0 (dummy-site-result :p-site 10)
                 1 (dummy-site-result :p-site 5)
                 2 (dummy-site-result :p-site 15)
                 3 (dummy-site-result :p-site 10)
                 5 (dummy-site-result :t-site 10)})
               (dummy-result
                {0 (dummy-site-result :p-site 10)
                 1 (dummy-site-result :p-site 5)
                 2 (dummy-site-result :p-site 15)
                 3 (dummy-site-result :p-site 10)
                 4 (dummy-site-result :t-site 20)})]
              (dummy-sites :p-sites [0 1 2 3]
                           :t-sites {4 [0 1]
                                     5 [2 3]}))
             => [30 40 50])
       (fact "it should take stacked-aggregation time into account"
             (timing/results-sequence-finish-times
              [(dummy-result
                {0 (dummy-site-result :p-site 10)
                 1 (dummy-site-result :p-site 5)
                 2 (dummy-site-result :p-site 15)
                 3 (dummy-site-result :p-site 10)
                 4 (dummy-site-result :t-site 20)
                 5 (dummy-site-result :t-site 10)
                 -1 (dummy-site-result :STACKED-CLASSIFIER 20)})
               (dummy-result
                {0 (dummy-site-result :p-site 10)
                 1 (dummy-site-result :p-site 5)
                 2 (dummy-site-result :p-site 15)
                 3 (dummy-site-result :p-site 10)
                 5 (dummy-site-result :t-site 10)
                 -1 (dummy-site-result :STACKED-CLASSIFIER 1)})
               (dummy-result
                {0 (dummy-site-result :p-site 10)
                 1 (dummy-site-result :p-site 5)
                 2 (dummy-site-result :p-site 15)
                 3 (dummy-site-result :p-site 10)
                 4 (dummy-site-result :t-site 20)
                 -1 (dummy-site-result :STACKED-CLASSIFIER 20)})]
              (dummy-sites :p-sites [0 1 2 3]
                           :t-sites {4 [0 1]
                                     5 [2 3]}))
             => [35 40 65])
       (fact "it should ignore aggregation time that is not from a STACKED-CLASSIFIER"
             (timing/results-sequence-finish-times
              [(dummy-result
                {0 (dummy-site-result :p-site 10)
                 1 (dummy-site-result :p-site 5)
                 2 (dummy-site-result :p-site 15)
                 3 (dummy-site-result :p-site 10)
                 4 (dummy-site-result :t-site 20)
                 5 (dummy-site-result :t-site 10)
                 -1 (dummy-site-result :NOT-STACKED-CLASSIFIER 30)})
               (dummy-result
                {0 (dummy-site-result :p-site 10)
                 1 (dummy-site-result :p-site 5)
                 2 (dummy-site-result :p-site 15)
                 3 (dummy-site-result :p-site 10)
                 5 (dummy-site-result :t-site 10)
                 -1 (dummy-site-result :NOT-STACKED-CLASSIFIER 30)})
               (dummy-result
                {0 (dummy-site-result :p-site 10)
                 1 (dummy-site-result :p-site 5)
                 2 (dummy-site-result :p-site 15)
                 3 (dummy-site-result :p-site 10)
                 4 (dummy-site-result :t-site 20)
                 -1 (dummy-site-result :NOT-STACKED-CLASSIFIER 30)})]
              (dummy-sites :p-sites [0 1 2 3]
                           :t-sites {4 [0 1]
                                     5 [2 3]}))
             => [30 40 50]))
