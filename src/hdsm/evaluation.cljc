(ns hdsm.evaluation
  (:require
   [hdsm.dataset.base
    :refer [get-dataset-id-map]]
   [hdsm.classifier.distributed.base
    :refer [get-all-sites]]
   [hdsm.classifier.distributed.sites
    :refer [site-attributes get-site-features feature-count]]
   [hdsm.utils
    :refer [map-vals percent get-keys keys->map mean weighted-mean
            std-dev]]
   [hdsm.classifier.distributed.aggregation
    :refer [classification-without-sites]]
   [hdsm.evaluation.timing
    :refer [mean-critical-path-time mean-time-between-responses]]
   [clojure.pprint :refer [print-table]]
   [clojure.set :refer [difference]]))


(defn get-confusion-frequencies
  "Returns a map of true-class and assigned-class combinations to
  frequencies within the given set of results."
  [results]
  (->> results
       (map #(hash-map :truth (:truth %)
                       :assigned (:class %)))
       (frequencies)))

(defn print-confusion-matrix
  "Prints a typical confusion matrix for the given
  confusion-frequencies"
  [confusion-frequencies]
  (let [classes (->> confusion-frequencies
                     (map (comp vals first))
                     (apply concat)
                     (set))
        rows (->> classes
                  (map
                   (fn [truth-class]
                     (->> (map #(vector % (get confusion-frequencies
                                               {:truth truth-class :assigned %}))
                               classes)
                          (concat [["true v" truth-class]])
                          (into {})))))]
    (print-table (concat ["true v"] classes) rows)))

(defn get-f-score
  "Return the F1 score (f-score, f-measure) for the given precision and
  recall."
  [precision recall]
  (if (zero? (float (+ precision recall)))
    0
    (/ (* 2 precision recall)
       (+ precision recall))))

(defn get-class-performance-measures
  "Return a list of maps, where each map provides the precision,
  recall, and F1 score (f-score, f-measure) for a given class in the
  given confusion-frequencies."
  [confusion-frequencies]
  (let [group-totals (fn [group-key]
                       (->> confusion-frequencies
                            (group-by (comp group-key first))
                            (map (fn [[cls group]]
                                   [cls (->> group
                                             (map second)
                                             (reduce +))]))
                            (into (sorted-map))))
        assigned-totals (group-totals :assigned)
        truth-totals (group-totals :truth)
        classes (->> confusion-frequencies
                     (map (comp vals first))
                     (apply concat)
                     (set))]
    (for [cls classes
          :let [class-tp (get confusion-frequencies {:truth cls :assigned cls} 0)
                cls-assigned-total (get assigned-totals cls 0)
                precision (if (> cls-assigned-total 0)
                            (/ class-tp cls-assigned-total)
                            0)
                cls-truth-total (get truth-totals cls 0)
                recall (if (> cls-truth-total 0)
                         (/ class-tp cls-truth-total)
                         0)]]
      {:class cls
       :instances (or (get truth-totals cls) 0)
       :tp class-tp
       :fn (- cls-truth-total class-tp)
       :fp (- cls-assigned-total class-tp)
       :precision (float precision)
       :recall (float recall)
       :f-score (float (get-f-score precision recall))})))

(defn get-average-f-scores
  "Returns various f-score averages (over all class values). See:
  http://scikit-learn.org/stable/modules/generated/sklearn.metrics.f1_score.html"
  [class-performance-measures]
  (let [all-tp (reduce + (map :tp class-performance-measures))
        all-fn (reduce + (map :fn class-performance-measures))
        all-fp (reduce + (map :fp class-performance-measures))
        all-precision (if (+ all-tp all-fp)
                        (/ all-tp (+ all-tp all-fp))
                        0)
        all-recall (if (+ all-tp all-fn)
                     (/ all-tp (+ all-tp all-fn))
                     0)]
    {:unweighted-mean (mean (map :f-score class-performance-measures))
     :weighted-mean (weighted-mean :f-score :instances class-performance-measures)
     :micro (float (get-f-score all-precision all-recall))}))

(defn- get-communication-to-site
  "Return a map for the given site-index of its source-sites and the
  proportion of the total dataset's records they sent to the
  trouble-site."
  [site-index results]
  (->> results
       ;; Each result's site-communication is a map of site-indexes to
       ;; the list of site's that sent it a trouble-record.
       (map #(get-in % [:site-communication site-index]))
       ;; Remove entries for records where there was no communication
       ;; to the site.
       (remove nil?)
       ;; Flatten into a single list of site-indexes that sent it
       ;; trouble records (one list item for each time it sent a
       ;; trouble record).
       (flatten)
       ;; Convert the list of transmissions into a map that counts the
       ;; number of records sent as trouble from each source-site (to
       ;; the trouble-site these transmissions were sent to).
       (frequencies)
       ;; Convert each count of records into a proportion of
       ;; the total number of results.
       (map-vals #(/ % (count results)))
       (into {})))

(defn get-communication-to-sites
  "Return a map of site indexes associated with maps of their
  source-sites and the proportion of the total dataset's records they
  sent to the trouble-site."
  [results site-indexes]
  (->> site-indexes
       (map #(get-communication-to-site % results))
       (map vector site-indexes)
       (into (sorted-map))))

(defn get-site-weights
  "Return a map of site-indexes to the proportion of features they
  process."
  [sites]
  (let [site-features (map-vals #(get-site-features % sites) sites)
        total-features (-> site-features vals flatten distinct count)]
    (map-vals #(/ (count %) total-features) site-features)))

(defn weight-communication-to-site
  "Alter the communication-to-site proportions to reflect the
  feature-widths/weights of different sites."
  [communication-to-site site-weights]
  (->> communication-to-site
       (map
        (fn [[source-site-index record-proportion]]
          (* record-proportion
             (get site-weights source-site-index))))
       (map vector (keys communication-to-site))
       (into {})))

(defn weight-communication-to-sites
  "Alter the communication-to-sites proportions to reflect the
  feature-widths/weights of different sites."
  [communication-to-sites sites]
  (let [site-weights (get-site-weights sites)]
    (->> communication-to-sites
         (map-vals #(weight-communication-to-site % site-weights))
         (into (sorted-map)))))

(defn- pretty-site-communication
  "Format either a site-communication or weighted-site-communication
  map-of-maps with site-labels as keys and formatted percentage."
  [site-communication sites percent]
  (let [site-labels (map-vals :label sites)]
    (->> site-communication
         (map
          (fn [[idx transmissions-to-site]]
            [(get site-labels idx)
             (->> transmissions-to-site
                  (map
                   (fn [[source-idx proportion]]
                     [(get site-labels source-idx)
                      (percent proportion)]))
                  (into (sorted-map)))]))
         (into (sorted-map)))))

(defn get-site-usage
  "Returns a mapping for each site label to the proportion of the total
  records it was chosen by the aggregator to be the final
  classification for."
  [results]
  (->> results
       (map :site-label)
       (frequencies)
       (map-vals #(/ % (count results)))))

(defn- get-site-usage-accuracy
  "Returns a mapping for each site label to the overall accuracy
  achieved by that site on the records it was chosen to classify."
  [results]
  (->> results
       (group-by :site-label)
       (map-vals
        (fn [results-for-site]
          (let [correct-results-for-site
                (filter #(= (:truth %)
                            (:class %))
                        results-for-site)]
            (/ (count correct-results-for-site)
               (count results-for-site)))))))

(defn- get-accuracy
  [results]
  (/ (count (filter #(= (:truth %) (:class %)) results))
     (count results)))

(defn- get-proportion-transmitted
  [weighted-site-communication]
  (->> weighted-site-communication
       (vals)
       (map #(reduce + (vals %)))
       (reduce +)))

(defn- get-t-site-active-range
  "Get the IDs of the first and last records a site existed. The site
  will not have existed for processing the start record, but will have
  existed for the end record."
  [t-site-label results site-changelog]
  (let [t-site-add-events (->> site-changelog
                               (filter #(and (= :add (first %))
                                             (= t-site-label (second %)))))
        t-site-remove-events (->> site-changelog
                                  (filter #(and (= :remove (first %))
                                                (= t-site-label (second (second %))))))]
    (when (> (count t-site-add-events) 1)
      (throw (Exception. "Impossible to have added site more than once.")))
    (when (> (count t-site-remove-events) 1)
      (throw (Exception. "Impossible to have removed site more than once.")))
    (let [start-id (if-let [add-event (first t-site-add-events)]
                     (last add-event)
                     (:id (first results)))
          end-id (if-let [remove-event (first t-site-remove-events)]
                   (last remove-event)
                   (:id (last results)))]
      [start-id end-id])))

(defn- get-proportion-transmitted-to-t-site
  "Get the proportion of record features transmitted to a t-site (only
  considering periods when the t-site existed)."
  [t-site-index results sites site-changelog]
  (let [t-site-label (:label (get sites t-site-index))
        [start-id end-id] (get-t-site-active-range t-site-label results
                                                   site-changelog)
        site-weights (get-site-weights sites)
        active-results (filter #(and (> (:id %) start-id)
                                     (<= (:id %) end-id))
                               results)]
    (as-> active-results $
      (get-communication-to-site t-site-index $)
      (weight-communication-to-site $ site-weights)
      ;; Sum all transmissions to the site.
      (reduce + 0 (vals $)))))

(defn- get-proportions-transmitted-to-t-sites
  "Get the proportions of record features transmitted to t-sites (only
  counting periods when at least one t-site existed)."
  [results sites site-changelog]
  (->> sites
       (filter #(= :t-site (:type (second %))))
       (map first)
       (map #(vector % (get-proportion-transmitted-to-t-site % results sites
                                                              site-changelog)))
       (into {})))

(defn- get-proportions-at-p-sites
  "Get the proportions of features at p-sites."
  [sites results]
  (->> sites
       (filter #(= :p-site (:type (second %))))
       ;; Get p-site indexes
       (map first)
       ;; Get weights
       (select-keys (get-site-weights sites))))

(defn temporal-summary
  [site-structure all-results site-changelog partition-size]
  (->> all-results
       (partition partition-size partition-size [])
       (map
        (fn [results]
          (let [proportions-at-p-sites
                (get-proportions-at-p-sites site-structure results)
                proportions-transmitted-to-t-sites
                (get-proportions-transmitted-to-t-sites results site-structure site-changelog)]
            {:results-count (count results)
             :accuracy (get-accuracy results)
             :proportion-transmitted
             (-> results
                 (get-communication-to-sites (keys site-structure))
                 (weight-communication-to-sites site-structure)
                 (get-proportion-transmitted))
             :proportions-transmitted-to-sites (merge proportions-transmitted-to-t-sites
                                                      proportions-at-p-sites)
             :max-proportion-transmitted-to-any-t-site (->> proportions-transmitted-to-t-sites
                                                            (vals)
                                                            (apply max 0))
             :mean-critical-path-time (mean-critical-path-time site-structure results)})))))

(defn results-summary
  ([site-structure results site-changelog
    & {:keys [pretty? skip-records]
       :or {pretty? true
            skip-records [1000]}}]
   (let [sites (if (map? site-structure)
                 site-structure
                 (into (sorted-map) (map-indexed vector site-structure)))
         percent (if pretty? percent identity)
         results-count (count results)
         site-communication (get-communication-to-sites results (keys sites))
         weighted-site-communication (weight-communication-to-sites site-communication sites)
         site-hits (->> results
                        (map :breakdown)
                        (flatten)
                        (map keys)
                        (flatten)
                        (frequencies))
         site-hit-rates (->> (vals sites)
                             (map-indexed
                              (fn [idx site]
                                [(:label site)
                                 (percent (/ (get site-hits idx 0) results-count))]))
                             (into (sorted-map)))
         temporal-summary-100 (temporal-summary site-structure results site-changelog 100)]
     {:results-count results-count
      :accuracy (percent (get-accuracy results))
      :proportion-transmitted (percent (get-proportion-transmitted weighted-site-communication))
      :site-hit-rates site-hit-rates
      :site-communication (pretty-site-communication site-communication sites percent)
      :weighted-site-communication (pretty-site-communication weighted-site-communication sites percent)
      :site-usage (map-vals percent (get-site-usage results))
      :site-usage-accuracy (map-vals percent (get-site-usage-accuracy results))
      :subset-summaries
      (zipmap skip-records
              (for [skip-count skip-records]
                (results-summary site-structure
                                 (drop skip-count results)
                                 site-changelog
                                 :pretty? pretty?
                                 :skip-records [])))
      :f-score (->> results
                    get-confusion-frequencies
                    get-class-performance-measures
                    get-average-f-scores)
      :temporal-summary-100 temporal-summary-100
      :mean-max-transmission-100 (->> temporal-summary-100
                                      ;; Weighted mean by proportion
                                      ;; of results in each partition.
                                      (map #(* (:max-proportion-transmitted-to-any-t-site %)
                                               (/ (:results-count %) results-count)))
                                      (reduce +))
      :mean-critical-path-time (mean-critical-path-time sites results)})))

;; Single vs. two phase transmission

(defn- t-site-result-info
  "Compute useful statistics for the result with respect to the
  trouble-site. site-feature-counts is a map of site IDs to the number
  of features processed at that site."
  [result t-site-idx site-feature-counts]
  (let [transmitting-source-site-idxs (-> (:site-communication result)
                                          (get t-site-idx))]
    (if (zero? (count transmitting-source-site-idxs))
      nil
      {:sources-transmitted (count transmitting-source-site-idxs)
       :features-transmitted (->> transmitting-source-site-idxs
                                  (map #(get site-feature-counts %))
                                  (reduce +))
       :processed? (contains? (:breakdown result) t-site-idx)})))

(defn- summarise-site-results
  "Computes aggregates from a sequence of responses returned by
  t-site-result-info"
  [site-result-summaries]
  {:fragments (->> site-result-summaries
                   (map :sources-transmitted)
                   (reduce +))
   :fragment-features (->> site-result-summaries
                           (map :features-transmitted)
                           (reduce +))
   :physical-records (count site-result-summaries)})

(defn communication-and-agreement-summary
  "Evaluates the counts of fragments and features that were marked as
  trouble at source-sites versus how many were actually processed at
  trouble-sites."
  [sites results]
  (let [t-site-idxs (->> sites
                         (filter #(= :t-site (:type (second %))))
                         (map first))
        site-feature-counts (map-vals #(feature-count % sites) sites)]
    (as-> t-site-idxs $
      (keys->map
       (fn [t-site-idx]
         (let [site-results (->> results
                                 (map #(t-site-result-info % t-site-idx site-feature-counts))
                                 (remove nil?))]
           (as-> site-results $
             (group-by #(if (:processed? %) :processed :not-processed) $)
             (map-vals summarise-site-results $)
             (assoc $ :total (summarise-site-results site-results)))))
       $)
      (assoc $ :total
             {:processed (apply merge-with + (map :processed (vals $)))
              :not-processed (apply merge-with + (map :not-processed (vals $)))
              :total (apply merge-with + (map :total (vals $)))}))))

;; Batch-index inversion comparison.

(defn record-transmitted?
  "Was this result's record marked as 'trouble' that should be
  transmitted from the source-site to the trouble-site?"
  [result source-idx t-site-idx]
  (-> result
      (:site-communication)
      (get t-site-idx)
      (set)
      (contains? source-idx)))

(defn record-processed?
  "Was this result's record processed by the
  trouble-site? (i.e. agreement was achieved for all source-sites)."
  [result source-idx t-site-idx]
  (-> result
      (:breakdown)
      (keys)
      (set)
      (contains? t-site-idx)))

(defn summarise-unique-fragments
  "Provides a summary of the proportion of duplicate
  feature-combinations in a set of results (only taking into
  consideration fragments where features are actually transmitted
  depending on single vs. two-phase transmission)."
  [results sites dataset two-phase?]
  (let [record-map (get-dataset-id-map dataset)
        t-site-idxs (->> sites
                         (filter #(= :t-site (:type (second %))))
                         (map first))
        features-transmitted? (if two-phase?
                                record-processed?
                                record-transmitted?)]
    (into
     {}
     (for [t-site-idx t-site-idxs
           source-idx (:source-sites (get sites t-site-idx))
           :let [source-site (get sites source-idx)
                 source-feature-idxs (drop-last (site-attributes source-site sites))
                 source-feature-count (count source-feature-idxs)
                 transmitted-results (->> results
                                          (filter #(features-transmitted?
                                                    % source-idx t-site-idx)))
                 transmitted-results-count (count transmitted-results)
                 unique-fragment-count (->> transmitted-results
                                            (map #(as-> % $
                                                    (:id $)
                                                    (get record-map $)
                                                    (:values $)
                                                    (get-keys $ source-feature-idxs)))
                                            (distinct)
                                            (count))]]
       [[source-idx t-site-idx]
        {:fragments-transmitted transmitted-results-count
         :fragment-features-transmitted (* source-feature-count
                                           transmitted-results-count)
         :unique-fragments-transmitted unique-fragment-count
         :unique-fragment-features-transmitted (* source-feature-count
                                                  unique-fragment-count)}]))))

(defn evaluate-batch-index-inversion
  "Provides a summary of the proportion of duplicate
  feature-combinations within batches of transmitted trouble-fragments
  over the results of an experiment (only taking into consideration
  fragments where features are actually transmitted depending on
  single vs. two-phase transmission)."
  [results final-site-structure dataset batch-size two-phase?]
  (->> (partition batch-size results)
       (pmap #(->> (summarise-unique-fragments % final-site-structure
                                               dataset two-phase?)
                   (vals)
                   (apply merge-with +)))
       (apply merge-with +)))

;; Tier/Order/Response-time evaluation

(defn max-site-order
  "Return the maximum order value for a site that received a
  trouble-record when determining the result."
  [result]
  (->> result
       :breakdown
       vals
       (map :site-order)
       (reduce max)))

(defn get-order-usage
  "Returns a map of site-order values to the number of records that were
  transmitted to a site of that order during classification."
  [results]
  (loop [max-order-counts (->> results
                               (map max-site-order)
                               (frequencies)
                               (vec)
                               (sort-by first))
         order-usage {}]
    (if-let [[order order-count] (first max-order-counts)]
      (recur (rest max-order-counts)
             (as-> order-usage $
               (assoc $ order 0)
               (map-vals #(+ % order-count) $)))
      order-usage)))

(defn get-order-performance
  "Returns a map of site-order values to the accuracy achieved when all
  higher-order classification results are ignored."
  [results site-structure]
  (let [max-order (->> results
                       (map max-site-order)
                       (reduce max))
        result-count (count results)
        order-results
        (into (sorted-map)
              (for [order (range 0 (inc max-order))
                    :let [higher-order-site-idxs
                          (->> site-structure
                               (filter #(> (:order (second %)) order))
                               (map first))]]
                (->> results
                     (map (fn [result]
                            (as-> higher-order-site-idxs $
                              ;; Only exclude site-idxs that are candidates
                              (filter #(contains? (:candidate-results result) %) $)
                              ;; Re-classify without high-order sites
                              (classification-without-sites result $)
                              ;; Re-add truth to result
                              (assoc $ :truth (:truth result)))))
                     (vector order))))]
    (map-vals
     (fn [results]
       (let [confusion-frequencies (get-confusion-frequencies results)
             class-performance-measures (get-class-performance-measures confusion-frequencies)
             average-f-scores (get-average-f-scores class-performance-measures)]
         {:unweighted-f (:unweighted-mean average-f-scores)
          :accuracy (as-> results $
                      (filter #(= (:class %) (:truth %)) $)
                      (count $)
                      (/ $ result-count))}))
     order-results)))

(defn get-order-timing
  "Returns a map of site-order values to the mean-critical-path-time
  required when all higher-order classification results are ignored."
  [results site-structure]
  (let [max-order (->> results
                       (map max-site-order)
                       (reduce max))]
    (into (sorted-map)
          (for [order (range 0 (inc max-order))
                :let [higher-order-site-idxs
                      (->> site-structure
                           (filter #(> (:order (second %)) order))
                           (map first))
                      results-wo-higher-order-sites
                      (map (fn [result]
                             (update result :breakdown
                                     #(apply dissoc % higher-order-site-idxs)))
                           results)]]
            [order
             [(mean-critical-path-time site-structure results-wo-higher-order-sites)
              (mean-time-between-responses site-structure results-wo-higher-order-sites)]]))))

(defn get-order-summary
  "Returns a summary of the usage and accuracy of different site-order
  values."
  [results site-structure]
  (let [order-usage (get-order-usage results)
        order-accuracies (get-order-performance results site-structure)
        order-timing (get-order-timing results site-structure)
        site-orders (keys order-accuracies)]
    (->> site-orders
         (map #(let [[mean-critical-path-time mean-time-between-responses]
                     (get order-timing %)]
                 {:usage (get order-usage %)
                  :accuracy (percent (get-in order-accuracies [% :accuracy]))
                  :unweighted-f (percent (get-in order-accuracies [% :unweighted-f]))
                  :mean-critical-path-time (float mean-critical-path-time)
                  :mean-time-between-responses (float mean-time-between-responses)}))
         (map vector site-orders)
         (into (sorted-map)))))

;; Monitor Timelines

(defn creation-monitor-timelines [results]
    (let [source-site-pairs (->> results
                                 (map (comp keys :creation :monitors))
                                 (filter some?)
                                 (apply concat)
                                 (distinct))]
        (into {}
              (for [pair source-site-pairs
                    :let [states (->> results
                                      (map (comp :creation :monitors))
                                      (map #(get % pair)))]]
                  [pair {:proportion (map #(get % :proportion 0) states)
                         :threshold (map #(get % :threshold 0) states)
                         :time (map #(get % :time-counter 0) states)}]))))

(defn- removal-monitor-timelines [metric results]
    (let [source-site-pairs (->> results
                                 (map (comp keys :removal :monitors))
                                 (filter some?)
                                 (apply concat)
                                 (distinct))]
        (into {}
              (for [pair source-site-pairs
                    :let [states (->> results
                                      (map (comp :removal :monitors))
                                      (map #(get % pair))
                                      (map metric))]]
                  [pair {:proportion (map #(get % :proportion 0) states)
                         :threshold (map #(get % :threshold 0) states)
                         :time (map #(get % :time-counter 0) states)}]))))

(defn accuracy-removal-monitor-timelines [results]
    (removal-monitor-timelines :accuracy results))

(defn usage-removal-monitor-timelines [results]
    (removal-monitor-timelines :usage results))

(defn get-block-accuracies
  [results & {:keys [block-size]}]
  (let [block-size (or block-size 100)
        blocks (partition block-size block-size [] results)
        ;; Discard the last block if it is less than half the
        ;; block-size.
        blocks (if (< (count (last blocks)) (/ block-size 2.0))
                 (drop-last blocks)
                 blocks)]
    (map get-accuracy blocks)))

(defn get-block-summary-statistics
  [block-accuracies]
  {:mean (mean block-accuracies)
   :std-dev (std-dev block-accuracies)})
