(ns hdsm.classifier.distributed.aggregation
  (:require
   [hdsm.classifier.base
    :refer [process-record describe-model]]
   [hdsm.utils
    :refer [max-fn mean]]
   [hdsm.utils.random
    :refer [seeded-shuffler shuffle!]])
  (:import [hdsm.classifier.base Classifier]))

(defn- aggregate-max-conf
  "Returns the result with the highest confidence."
  [site-results]
  (max-fn :confidence (vals site-results)))

(defprotocol Aggregator
  (aggregate-results [aggregator record site-results]
    "Return the final classification result based on the individual
  results from each site.")
  (describe-aggregator [aggregator]
    "Return a description of any classification model (e.g. stacked
    classifier) in this aggregator."))

(defprotocol FinalAggregator
  (aggregate-results-with-candidates [aggregator record site-results]
    "Return a map containing the :final-result and
    the :candidate-results that were used to determine it through a
    form of aggregation that can be repeated (e.g. max-conf) to
    compare accuracy without certain site(s)."))

;; Simple Aggregators

(defn- build-max-conf-aggregator
  "Builds an aggregator that chooses the result with the highest
  confidence."
  []
  (reify
    Aggregator
    (aggregate-results [this record site-results]
      (aggregate-max-conf site-results))
    (describe-aggregator [this]
      nil)))

(defn- build-simple-voting-aggregator
  "Builds an aggregator that produces a new result for the class with
  the greatest number of votes in the input results (choosing randomly
  between ties)."
  []
  (let [shuffler (seeded-shuffler 1)]
    (reify
      Aggregator
      (aggregate-results [this record site-results]
        (let [class-votes (->> (vals site-results)
                               (map :class)
                               (frequencies))
              ;; Find the maximum votes received by a class
              max-votes (apply max (vals class-votes))
              ;; Find all classes that reached max-votes
              best-classes (->> class-votes
                                (filter #(= (second %) max-votes))
                                (map first))
              ;; In the case of any ties, randomly pick a
              ;; winner using the record ID as a
              ;; repeatable seed.
              winning-class (->> best-classes
                                 (shuffle! shuffler)
                                 (first))
              mean-confidence (->> (vals site-results)
                                   (map :confidence)
                                   (mean))]
          {:class winning-class
           :confidence mean-confidence
           :site-label :SIMPLE-VOTING
           :site-type :SIMPLE-VOTING
           :id (:id record)}))
      (describe-aggregator [this]
        nil))))

(defn- build-simple-aggregator
  "Simple aggregators do not require building classifiers or have
  knowledge of schema and site-structure."
  [aggregation-rule]
  (case (:type aggregation-rule)
    :max-conf (build-max-conf-aggregator)
    :simple-voting (build-simple-voting-aggregator)
    (throw (IllegalArgumentException. (str "Unknown simple aggregation-rule type: "
                                           (:type aggregation-rule))))))

;; Stacking Aggregators

(defn- build-stacked-classifier
  "Build a classifier that can be used for stacked site classification
  aggregation."
  [classifier-generator schema site-count]
  ;; Assumes last attribute in the class.
  (let [class-attribute (last schema)
        feature-attributes (map #(hash-map :name (str "site-" (inc %))
                                           :options (:options class-attribute))
                                (range site-count))
        stacked-schema (concat feature-attributes [class-attribute])]
    (classifier-generator stacked-schema)))

(defn- build-stacked-aggregator
  "Builds an aggregator that uses p-site classifications as input to a
  stacked-classifier."
  [classifier-generator schema initial-sites]
  (let [stacked-classifier (build-stacked-classifier classifier-generator
                                                     schema
                                                     (->> (vals initial-sites)
                                                          (filter #(= :p-site (:type %)))
                                                          (count)))]
    (reify
      Aggregator
      (aggregate-results [this record site-results]
        (when (some #(not= :p-site (:site-type %))
                    (vals site-results))
          (throw (Exception. "Attempted running stacked-aggregator
          with non-p-site result.")))
        (let [;; Assuming last value is the class.
              true-class (last (:values record))
              input-classes (->> (vals site-results)
                                 (map :class))
              stacked-record {:id (:id record)
                              :values (vec (concat input-classes
                                                   [true-class]))}]
          (-> (process-record stacked-classifier stacked-record)
              (assoc :site-label :STACKED-CLASSIFIER)
              (assoc :site-type :STACKED-CLASSIFIER))))
      (describe-aggregator [this]
        (describe-model stacked-classifier)))))

;; The way one-level-stacking has many missing values is problematic
;; for some classifiers (e.g. adaptive random forest), so is not
;; appropriate for further use.
#_(defn- build-one-level-stacked-aggregator
  "Builds an aggregator that uses all p-site and t-site classifications
  as input to a stacked-classifier."
  [classifier-generator schema max-site-count]
  (let [stacked-classifier (build-stacked-classifier classifier-generator
                                                     schema max-site-count)]
    (reify
      Aggregator
      (aggregate-results [this record site-results]
        (let [;; Assuming last value is the class.
              true-class (last (:values record))
              site-classes (for [i (range max-site-count)]
                             (if-let [site-result (get site-results i)]
                               (:class site-result)
                               Double/NaN))
              stacked-record {:id (:id record)
                              :values (vec (concat site-classes
                                                   [true-class]))}]
          (-> (process-record stacked-classifier stacked-record)
              (assoc :site-label :STACKED-CLASSIFIER)
              (assoc :site-type :STACKED-CLASSIFIER))))
      (describe-aggregator [this]
        (describe-model stacked-classifier)))))

(defn- build-p-site-aggregator
  "Builds an aggregator for p-site results with the given
  aggregation-rule and other details of the distributed classifier."
  [aggregation-rule classifier-generator schema initial-sites]
  (try
    (build-simple-aggregator aggregation-rule)
    (catch IllegalArgumentException ex
      (case (:type aggregation-rule)
        :two-level-stacked
        (build-stacked-aggregator (or (:classifier-generator aggregation-rule)
                                      classifier-generator)
                                  schema
                                  initial-sites)
        ;; :one-level-stacked
        ;; (build-one-level-stacked-aggregator classifier-generator
        ;;                                     schema
        ;;                                     (:max-site-count aggregation-rule))
        (throw (IllegalArgumentException. (str "Unknown aggregation-rule type: "
                                               (:type aggregation-rule))))))))

(defn build-two-stage-aggregator
  "Build an aggregator that works in two stages:
  1. Aggregate p-site results according to the p-site-aggregation-rule
  2. Aggregate the initial result with t-site results according to max-confidence."
  [p-site-aggregation-rule classifier-generator schema initial-sites]
  (let [p-site-aggregator (build-p-site-aggregator p-site-aggregation-rule
                                                   classifier-generator schema
                                                   initial-sites)]
    (reify
      FinalAggregator
      (aggregate-results-with-candidates [this record site-results]
        (let [p-site-results (->> site-results
                                  (filter #(= :p-site (:site-type (second %))))
                                  (into (sorted-map)))
              p-site-aggregated-result (-> (aggregate-results p-site-aggregator
                                                              record p-site-results)
                                           ;; Because this result is
                                           ;; based on p-sites, it is
                                           ;; order zero.
                                           (assoc :site-order 0))
              ;; The new set of results is the
              ;; p-site-aggregated-result along with all t-site
              ;; results.
              new-site-results (as-> site-results $
                                 ;; Filter out p-site results.
                                 (filter #(not= :p-site (:site-type (second %))) $)
                                 (into (sorted-map) $)
                                 ;; Place stacked-result at index -1
                                 ;; so that it may take priority over
                                 ;; t-sites.
                                 (assoc $ -1 p-site-aggregated-result))]
          ;; Return both the final result, and also the list of
          ;; candidate-results that were passed to
          ;; aggregate-max-conf (so that they can be used when
          ;; evaluating accuracy without particular trouble-site(s)).
          {:final-result (aggregate-max-conf new-site-results)
           :candidate-results new-site-results}))
      Aggregator
      (aggregate-results [this record site-results]
        (:final-result (aggregate-results-with-candidates this record site-results)))
      (describe-aggregator [this]
        {:p-site-aggregator (describe-aggregator p-site-aggregator)}))))

(defn classification-without-sites
  "Returns the classification that would have been achieved for the
  result if the given site-ids had not been considered."
  [result site-ids]
  (let [site-results (:candidate-results result)]
    (when (not (every? #(contains? site-results %) site-ids))
      (throw (Exception. "Some excluded site-ids not in map of site-results")))
    (as-> site-results $
      (apply dissoc $ site-ids)
      (aggregate-max-conf $))))

;; (defn changed-by-sites?
;;   "Returns true if the result was changed by the contribution of the
;;   given sites."
;;   [result site-ids]
;;   (not= (:class result)
;;         (classification-without-sites result site-ids)))

;; (defn worsened-by-sites?
;;   "Returns true if the result is incorrect and would have been correct
;;   without the contribution of the given sites."
;;   [result site-ids]
;;   (and (not (:correct? result))
;;        (= (:truth result) (classification-without-sites result site-ids))))

;; (defn improved-by-sites?
;;   "Returns true if the result is correct and would have been incorrect
;;   without the contribution of the given sites."
;;   [result site-ids]
;;   (and (:correct? result)
;;        (changed-by-sites? result site-ids)))

;; (defn improved-by-site?
;;   "Returns true if the result is correct and would have been incorrect
;;   without the contribution of the given site."
;;   [result site-id]
;;   (improved-by-sites? result [site-id]))
