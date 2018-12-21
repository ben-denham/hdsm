(ns hdsm.dataset.base
  "IMPORTANT NOTE: There is a hard assumption throughout the codebase
  that the last value in each record is the class value.")

(defprotocol Dataset
  (get-schema [dataset])
  (pop-record-and-rest [dataset]))

(defrecord StaticDataset [schema records]
  Dataset
  (get-schema [this]
    schema)
  (pop-record-and-rest [this]
    [(first records) (StaticDataset. schema (rest records))]))

(defn concat-datasets
  "Returns a new dataset made by concatenating multiple datasets"
  ([datasets]
   (concat-datasets datasets 0))
  ([[head-dataset & rest-datasets] next-id]
   ;; Build a re-ified Dataset for the collection of datasets.
   (let [schemas (map get-schema (conj rest-datasets head-dataset))]
     (when (not (apply = schemas))
       (throw (Exception. "Cannot concat datasets with different schemas.")))
     (reify Dataset
       (get-schema [this]
         (first schemas))
       (pop-record-and-rest [this]
         ;; Pop a record from the head dataset.
         (let [[record rest-head-dataset] (pop-record-and-rest head-dataset)]
           (if record
             ;; If a non-nill record was returned, return it (though
             ;; with an id in the sequence of this concatenation) with
             ;; a new concated-dataset that only has the "rest" of the
             ;; head dataset.
             [(assoc record :id next-id)
              (concat-datasets (concat [rest-head-dataset] rest-datasets)
                               (inc next-id))]
             (if (empty? rest-datasets)
               ;; If there are no records left in the head dataset and
               ;; no other datasets, then return nil and this
               ;; unchanged dataset.
               [nil this]
               ;; If there are no records left in the head dataset,
               ;; but their are other datasets, then recur with a
               ;; concated-dataset of the remaining datasets.
               (pop-record-and-rest (concat-datasets rest-datasets next-id))))))))))

(defn get-dataset-id-map
  "Returns a map of all records in the dataset keyed by :id"
  [dataset]
  (loop [dataset dataset
         record-map {}]
    (let [[record rest-dataset] (pop-record-and-rest dataset)]
      (if record
        (recur rest-dataset (assoc record-map (:id record) record))
        record-map))))

(defn get-dataset-cardinalities
  "Return a sequence of the cardinalities of the attributes in a
  dataset (containing only nominal attributes)."
  [dataset]
  (let [feature-options (->> dataset
                             (get-schema)
                             (map :options))]
    (when (some #(= :numeric %) feature-options)
      (throw (Exception. "Cannot get cardinality of numeric feature.")))
    (map count feature-options)))
