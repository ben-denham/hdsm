(ns hdsm.evaluation.timing
  (:require
   [hdsm.utils :refer [mean rolling-difference]]
   [clojure.set :as set]))

(defn- result-time
  "Returns the time being measured for a `result`."
  [result]
  (:cpu-nano (:process-time result)))

(defn- get-breakdown-with-source-sites
  "Extend the breakdown of site-results with :source-sites from the
  sites map."
  [result sites]
  (->> (:breakdown result)
       (map
        (fn [[site-id site-result]]
          [site-id
           (assoc site-result :source-sites
                  (:source-sites (get sites site-id)))]))
       (into {})))

(defn- get-terminal-site-ids
  "Get the ids of sites that didn't forward the record in the given
  result breakdown as trouble (i.e. processing terminated at those
  sites)."
  [breakdown-with-source-sites]
  (let [;; Get the ids of any site used as a source to another
        ;; site (while processing this record).
        source-site-ids (->> (vals breakdown-with-source-sites)
                             (map :source-sites)
                             (remove nil?)
                             (flatten))]
    ;; Return the site-ids that processed the record but were not
    ;; source-sites.
    (set/difference (set (keys breakdown-with-source-sites))
                    (set source-site-ids))))

;; Response Time (Critical path time)

;; Declare `process-time-up-to-source-sites` function to allow
;; co-recursion with `process-time-up-to-site`.
(declare process-time-up-to-source-sites)

(defn- process-time-up-to-site
  "Returns the total process-time for the critical path up to the
  completion of the given `site-id` for the given result `breakdown`."
  [site-id breakdown]
  (let [site-result (get breakdown site-id)
        cpu-time (result-time site-result)]
    (if-let [source-site-ids (:source-sites site-result)]
      ;; If this is a t-site, return the CPU time taken to process
      ;; that site added to the time taken to process its
      ;; source-sites.
      (+ cpu-time (process-time-up-to-source-sites source-site-ids
                                                   breakdown))
      ;; If this is a p-site, return the CPU time taken to process the
      ;; site.
      cpu-time)))

(defn- process-time-up-to-source-sites
  "Returns the slowest process-time for the critical path up to the
  completion of any of the given `source-site-ids` for the given
  result `breakdown`."
  [source-site-ids breakdown]
  (->> source-site-ids
       (map #(process-time-up-to-site % breakdown))
       (reduce max)))

(defn result-trouble-critical-path-time
  "Calculate the time for the 'trouble' part of the classification (the
  critical path through primary and trouble-sites)."
  [result sites]
  (let [breakdown (get-breakdown-with-source-sites result sites)]
    (process-time-up-to-source-sites (get-terminal-site-ids breakdown)
                                     breakdown)))

(defn result-p-site-aggregation-critical-path-time
  "Calculate the time for the primary-site-aggregation part of the
  classification (primary-site + aggregator)."
  [result sites]
  (let [;; Find the slowest primary-site.
        breakdown (get-breakdown-with-source-sites result sites)
        p-site-ids (->> breakdown
                        (filter #(= (:site-type (second %)) :p-site))
                        (map first))
        p-site-time (process-time-up-to-source-sites p-site-ids breakdown)]
    ;; Aggregator time is that of the stacked classifier, or none.
    (if-let [stacked-classifier-result
             (->> (vals (:candidate-results result))
                  (filter #(= (:site-type %) :STACKED-CLASSIFIER))
                  (first))]
      ;; Add the primary-site and aggregation time.
      (+ p-site-time (result-time stacked-classifier-result))
      ;; No aggregation time, so just return p-site-time
      p-site-time)))

(defn result-critical-path-time
  "Find the critical-path CPU time for a result."
  [result sites]
  (max (result-p-site-aggregation-critical-path-time result sites)
       (result-trouble-critical-path-time result sites)))

(defn mean-critical-path-time
  "Find the mean critical-path CPU time for a sequence of results."
  [sites results]
  (->> results
       (map #(result-critical-path-time % sites))
       (mean)))

;; Throughput (time between responses)

;; Rules:
;; 1. A primary-site can start processing a record as soon as it has finished
;;    processing the last record.
;; 2. A trouble-site can start processing a record as soon as it has finished
;;    processing the last record and it's source-sites have finished processing
;;    the record.
;; 3. A stacked-classifier can start processing a record as soon as it has
;;    finished processing the last record and all primary-sites have finished
;;    processing the record.

;; Declare `source-sites-finish-time` function to allow co-recursion
;; with `site-finish-time`.
(declare source-sites-finish-time)

(defn- max-finish-time-pair
  [[finish-time-a free-times-a]
   [finish-time-b free-times-b]]
  [;; Take the maximum finish-time of the source-sites.
   (max finish-time-a finish-time-b)
   ;; Combine all new free-times
   (merge free-times-a free-times-b)])

(defn- site-finish-time
  "Takes a `site-id`, a `breakdown` of site-results (annotated
  with :source-sites), and the times when sites become free to start
  processing this record.

  Returns a pair containing the time when this site will have finished
  processing the given record, and a map of new free-times for this
  site and any of its source-sites that should replace the existing
  entries.

  Only considers CPU time of classifiers."
  [site-id breakdown free-times]
  (let [site-result (get breakdown site-id)
        free-time (get free-times site-id 0)
        cpu-time (result-time site-result)]
    (if-let [source-site-ids (:source-sites site-result)]
      ;; This site is a trouble-site, so it will start when the
      ;; source-sites have finished processing the current record or
      ;; when this site has finished processing the last
      ;; record (whichever is slower).
      (let [[source-finish-time new-free-times]
            (source-sites-finish-time source-site-ids breakdown free-times)
            finish-time (+ cpu-time (max free-time source-finish-time))]
        [finish-time (assoc new-free-times site-id finish-time)])
      ;; This site has no :source-sites, so must be a p-site. It will
      ;; start as soon as the p-site is finished with the last record.
      (let [finish-time (+ cpu-time free-time)]
        [finish-time {site-id finish-time}]))))

(defn- source-sites-finish-time
  "Takes a list of `source-site-ids`, a `breakdown` of
  site-results (annotated with :source-sites), and the times when
  sites become free to start processing this record.

  Returns a pair containing the time when all source-sites will have
  finished processing the given record, and a map of new free-times
  for these source-sites and any of their source-sites that should
  replace the existing entries.

  Only considers CPU time of classifiers."
  [source-site-ids breakdown free-times]
  (->> source-site-ids
       ;; Get the finish time for each source-site.
       (map #(site-finish-time % breakdown free-times))
       (reduce max-finish-time-pair)))

(defn result-trouble-finish-time
  "Returns the time when the given result would finish being processed
  by trouble-sites, assuming record is available to be processed as
  soon as classifiers are available (given by free-times), and only
  considering CPU time of classifiers."
  [result free-times sites]
  (let [breakdown (get-breakdown-with-source-sites result sites)]
    (source-sites-finish-time (get-terminal-site-ids breakdown)
                              breakdown free-times)))

(defn result-p-site-aggregation-finish-time
  "Returns the time when the given result would finish being processed
  by primary-sites and primary-site aggregation, assuming record is
  available to be processed as soon as classifiers are
  available (given by free-times), and only considering CPU time of
  classifiers."
  [result free-times sites]
  (let [;; Find the slowest primary-site.
        breakdown (get-breakdown-with-source-sites result sites)
        p-site-ids (->> breakdown
                        (filter #(= (:site-type (second %)) :p-site))
                        (map first))
        [p-site-finish-time p-site-new-free-times]
        (source-sites-finish-time p-site-ids breakdown free-times)]
    ;; Only consider time from a stacked-classification aggregator.
    (if-let [[aggregator-id aggregator-result]
             (->> (:candidate-results result)
                  (filter #(= (:site-type (second %)) :STACKED-CLASSIFIER))
                  (first))]
      ;; If there is an aggregator to consider, take it into
      ;; consideration.
      (let [aggregator-cpu-time (result-time aggregator-result)
            aggregator-free-time (get free-times aggregator-id 0)
            ;; The aggregator will start when the source-sites have
            ;; finished processing the current record or when this
            ;; site has finished processing the last record (whichever
            ;; is slower).
            aggregator-finish-time (+ aggregator-cpu-time
                                      (max aggregator-free-time
                                           p-site-finish-time))]
        [aggregator-finish-time (assoc p-site-new-free-times
                                       aggregator-id
                                       aggregator-finish-time)])
      ;; If there is no aggregator time to consider, only consider the
      ;; primary-sites.
      [p-site-finish-time p-site-new-free-times])))

(defn result-finish-time
  "Returns the time when the given result would finish being processed,
  assuming record is available to be processed as soon as classifiers
  are available (given by free-times), and only considering CPU time
  of classifiers."
  [result free-times sites]
  (max-finish-time-pair
   (result-p-site-aggregation-finish-time result free-times sites)
   (result-trouble-finish-time result free-times sites)))

(defn results-sequence-finish-times
  "Returns a sequence of times when each result would finish processing,
  assuming the first record starts processing at time 0, each record
  is available to processed as soon as classifiers are available, and
  only considering CPU time of classifiers."
  [results sites]
  (loop [results results
         finish-times []
         free-times {}]
    (if-let [result (first results)]
      (let [[finish-time new-free-times]
            (result-finish-time result free-times sites)]
        (recur (rest results)
               ;; Add the finish-time for this result.
               (conj finish-times finish-time)
               ;; Update the free-times.
               (merge free-times new-free-times)))
      finish-times)))

(defn mean-time-between-responses
  "Return the mean time between the completion of processing of
  subsequent results."
  [sites results]
  (->> (results-sequence-finish-times results sites)
       ;; It is possible that later records could finish processing
       ;; before earlier records (depending on the path taken), so we
       ;; sort the finish-times.
       (sort)
       ;; Prepend 0 to represent the start time.
       (concat [0])
       rolling-difference
       mean))
