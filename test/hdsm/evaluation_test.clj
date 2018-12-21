(ns hdsm.evaluation-test
  (:use midje.sweet)
  (:require
   [hdsm.evaluation :as evaluation]
   [hdsm.classifier.distributed.sites
    :refer [make-site-structure p-site t-site init-sites-atom]]))

;; Example from: http://scikit-learn.org/stable/modules/generated/sklearn.metrics.f1_score.html
(let [results [{:class "0" :truth "0"}
               {:class "2" :truth "1"}
               {:class "1" :truth "1"}
               {:class "0" :truth "0"}
               {:class "0" :truth "1"}
               {:class "1" :truth "2"}]
      confusion-frequencies (evaluation/get-confusion-frequencies results)
      class-performance-measures (evaluation/get-class-performance-measures
                                  confusion-frequencies)
      measures-by-class (zipmap (map :class class-performance-measures)
                                class-performance-measures)
      average-f-scores (evaluation/get-average-f-scores
                        class-performance-measures)]
  (fact "Precision calculation"
        (get-in measures-by-class ["0" :precision]) => (float 2/3)
        (get-in measures-by-class ["1" :precision]) => 0.5
        (get-in measures-by-class ["2" :precision]) => 0.0)
  (fact "Recall calculation"
        (get-in measures-by-class ["0" :recall]) => 1.0
        (get-in measures-by-class ["1" :recall]) => (float 1/3)
        (get-in measures-by-class ["2" :recall]) => 0.0)
  (fact "F-Score calculation"
        (get-in measures-by-class ["0" :f-score]) => (float 0.8)
        (get-in measures-by-class ["1" :f-score]) => (float 0.4)
        (get-in measures-by-class ["2" :f-score]) => 0.0)
  (fact "Unweighted Mean F-Score calculation"
        (format "%.3f" (:unweighted-mean average-f-scores)) => "0.400")
  (fact "Weighted Mean F-Score calculation"
        (format "%.3f" (:weighted-mean average-f-scores)) => "0.467")
  (fact "Micro F-Score calculation"
        (format "%.3f" (:micro average-f-scores)) => "0.500"))
