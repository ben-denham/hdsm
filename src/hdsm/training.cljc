(ns hdsm.training
  (:require
   [hdsm.dataset.base
    :refer [pop-record-and-rest get-schema]]
   [hdsm.evaluation
    :refer [results-summary]]
   [hdsm.classifier.distributed.base
    :refer [get-all-sites get-site-changelog]]
   [hdsm.classifier.distributed.dynamic-distributed
    :refer [dynamic-distributed-classifier]]
   [hdsm.classifier.distributed.dynamic-monitors
    :refer [make-creation-significant-agreement-monitor-factory
            make-removal-inverted-agreement-monitor-factory
            make-removal-accuracy-usage-monitor-factory
            make-blacklist-permanent-monitor-factory
            make-blacklist-source-accuracy-monitor-factory]]
   [hdsm.classifier.base
    :refer [process-record describe-model]]
   [hdsm.utils
    :refer [map-vals percent debug]]
   [hdsm.utils.random
    :refer [seeded-shuffle]]
   [hdsm.classifier.distributed.sites
    :refer [make-site-structure p-site t-site]]))

(defn train-classifier
  ([classifier dataset]
   (train-classifier classifier dataset nil))
  ([classifier dataset progress-atom]
   (loop [dataset dataset
          results []
          i 0]
     (let [[record rest-dataset] (pop-record-and-rest dataset)]
       (if record
         (let [raw-result (doall (process-record classifier record))
               result (assoc raw-result :truth (last (:values record)))]
           (when (and (= (mod i 1000) 0) progress-atom)
             (reset! progress-atom i))
           #_(debug
              (if (= (mod i 1000) 0)
                (println "Processed" i "records")))
           (recur rest-dataset (conj results result) (inc i)))
         results)))))

(defn build-classifier
  [base-classifier dataset base-site-structure
   {:keys [site-window-size site-training-time shared-sources?
           creation-window-size creation-time-threshold
           removal-window-size removal-time-threshold
           trouble-factor t-site-input-type creation-agreement-threshold
           removal-accuracy-threshold removal-usage-threshold]}
   & {:keys [p-site-aggregation-rule moving-average-generator
             detrend-confidence? confidence-jitter trouble-classifier
             disable-monitors disable-monitor-logging]}]
  (let [creation-monitor-factory (make-creation-significant-agreement-monitor-factory
                                  creation-window-size
                                  creation-agreement-threshold
                                  creation-time-threshold)
        removal-monitor-factory (make-removal-accuracy-usage-monitor-factory
                                 removal-window-size
                                 removal-accuracy-threshold
                                 removal-usage-threshold
                                 removal-time-threshold)
        blacklist-monitor-factory (make-blacklist-source-accuracy-monitor-factory)]
    (dynamic-distributed-classifier base-site-structure
                                    (get-schema dataset)
                                    base-classifier
                                    moving-average-generator
                                    site-window-size
                                    trouble-factor
                                    t-site-input-type
                                    site-training-time
                                    shared-sources?
                                    creation-monitor-factory
                                    removal-monitor-factory
                                    blacklist-monitor-factory
                                    p-site-aggregation-rule
                                    detrend-confidence?
                                    confidence-jitter
                                    (or trouble-classifier base-classifier)
                                    :disable-monitors disable-monitors
                                    :disable-monitor-logging disable-monitor-logging)))

(defn build-naive-classifier
  [base-classifier dataset base-site-structure
   & {:keys [p-site-aggregation-rule moving-average-generator
             detrend-confidence? confidence-jitter]}]
  (dynamic-distributed-classifier base-site-structure
                                  (get-schema dataset)
                                  base-classifier
                                  moving-average-generator
                                  1
                                  0
                                  nil
                                  1
                                  false
                                  (make-creation-significant-agreement-monitor-factory 1 2 1)
                                  (make-removal-inverted-agreement-monitor-factory)
                                  (make-blacklist-permanent-monitor-factory)
                                  p-site-aggregation-rule
                                  detrend-confidence?
                                  confidence-jitter
                                  base-classifier
                                  :disable-monitors true))

(defn run-experiment
  ([base-classifier config]
   (run-experiment base-classifier config nil))
  ([base-classifier
    {:keys [label dataset-description dataset-fn
            base-site-structure system-config
            p-site-aggregation-rule detrend-confidence?
            confidence-jitter trouble-classifier
            disable-monitors disable-monitor-logging]}
    progress-atom]
   (let [dataset (dataset-fn)
         classifier (if (= :naive system-config)
                      (build-naive-classifier base-classifier dataset
                                              base-site-structure
                                              :p-site-aggregation-rule p-site-aggregation-rule
                                              :detrend-confidence? detrend-confidence?
                                              :confidence-jitter confidence-jitter)
                      (build-classifier base-classifier dataset
                                        base-site-structure
                                        system-config
                                        :p-site-aggregation-rule p-site-aggregation-rule
                                        :detrend-confidence? detrend-confidence?
                                        :confidence-jitter confidence-jitter
                                        :trouble-classifier trouble-classifier
                                        :disable-monitors disable-monitors
                                        :disable-monitor-logging disable-monitor-logging))
         results (train-classifier classifier dataset progress-atom)
         sites (get-all-sites classifier)
         model-description (describe-model classifier)]
     {:label label
      :timestamp (quot (System/currentTimeMillis) 1000)
      :dataset-description dataset-description
      :base-site-structure base-site-structure
      :system-config system-config
      :final-site-structure (->> sites
                                 (map-vals #(select-keys % [:type :label :attributes :source-sites :order])))
      :site-changelog (get-site-changelog classifier)
      :summary (results-summary sites results (get-site-changelog classifier)
                                :pretty? false)
      :model-description (if (map? (:AGGREGATOR model-description))
                           (update-in model-description
                                      [:AGGREGATOR :p-site-aggregation-rule]
                                      dissoc :classifier-generator)
                           model-description)
      :results results})))

(defn random-base-site-structure
  ([dataset-fn feature-count max-features-per-p-site]
   (random-base-site-structure dataset-fn feature-count max-features-per-p-site 1))
  ([dataset-fn feature-count max-features-per-p-site seed]
   (let [features (range 0 feature-count)
         class-index feature-count
         p-sites (->> (seeded-shuffle features seed)
                      (partition max-features-per-p-site
                                 max-features-per-p-site
                                 [])
                      (map-indexed
                       (fn [idx site-features]
                         (p-site (keyword (str "p-" (inc idx)))
                                 (vec site-features)))))]
     (apply make-site-structure class-index p-sites))))

(defn- p-site-pairs
  [feature-count]
  (let [p-site-count (/ feature-count 2)
        features (range feature-count)
        feature-pairs (clojure.math.combinatorics/combinations features 2)]
    (->> (clojure.math.combinatorics/combinations feature-pairs p-site-count)
         (filter (fn [pair-combos]
                   (= (count (flatten pair-combos))
                      (count (distinct (flatten pair-combos)))))))))

(defn p-site-permutations
  [feature-count]
  (->> (p-site-pairs feature-count)
       (map
        (fn [p-sites-features]
          (->> p-sites-features
               (map-indexed #(p-site (keyword (str "p" (inc %1)))
                                     (into [] %2))))))))
