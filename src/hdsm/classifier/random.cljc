(ns hdsm.classifier.random
  (:require [hdsm.classifier.base
             :refer [process-record describe-model]])
  (:import [hdsm.classifier.base Classifier]))

(defn random-classifier [schema]
  (reify Classifier
    (process-record [this record]
      (let [class-options (:options (last schema))]
        {:class (rand-int (count class-options))
         :confidence (rand 1)
         :id (:id record)}))
    (describe-model [this]
      "Randomness")))
