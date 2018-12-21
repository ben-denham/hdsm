(ns hdsm.classifier.distributed.dynamic-distributed
  (:require [clojure.math.combinatorics
             :refer [combinations]]
            [clojure.set
             :refer [difference]]
            [hdsm.classifier.distributed.distributed
             :refer [distributed-classifier default-moving-average-generator]]
            [hdsm.classifier.distributed.sites
             :refer [init-sites-atom add-t-site! remove-t-site!
                     get-all-source-sites get-site-features]]
            [hdsm.classifier.base
             :refer [process-record describe-model]]
            [hdsm.classifier.distributed.base
             :refer [get-sites get-all-sites]]
            [hdsm.monitor.base
             :refer [fire? push-result get-reason describe-state]]
            [hdsm.monitor.window
             :refer [get-proportion]]
            [hdsm.utils
             :refer [seq-contains? indexes-where map-vals debug]])
  (:import [hdsm.classifier.base Classifier]
           [hdsm.classifier.distributed.base DistributedClassifier]
           [hdsm.dataset.base Dataset]))

;; Constructing monitors for potential trouble sites.

(defn- site-combinations
  "Return a list of possible combinations of source-sites for trouble
  sites. A separate function for expansion to more than pairs later if
  needed."
  [site-indexes]
  (combinations site-indexes 2))

(defn- get-potential-t-site-source-sites
  "Given the sorted-map of current sites, returns a list of all possible
  PAIRS of source-sites for a new trouble-site. Sites already used as
  source-sites to an existing trouble-site are not listed as possible
  source-sites."
  [sites shared-sources?]
  (let [all-site-indexes (keys sites)]
    (if (not shared-sources?)
      ;; If reusing the same source for multiple trouble-sites is NOT
      ;; allowed, get combinations of source-sites that don't include
      ;; any sites already used as source-sites.
      (-> (set all-site-indexes)
          (difference (set (get-all-source-sites sites)))
          (site-combinations))
      ;; If reusing the same source for multiple trouble-sites IS
      ;; allowed, then get combinations where the same feature is not
      ;; redundantly merged, and where no other trouble-site processes
      ;; the same set of features.
      (let [existing-site-feature-sets (map #(get-site-features % sites) (vals sites))]
        (->> all-site-indexes
             (site-combinations)
             (filter
              (fn [source-site-indexes]
                (let [source-features (->> source-site-indexes
                                           (map #(get sites %))
                                           (map #(get-site-features % sites))
                                           (flatten)
                                           (sort))]
                  (and (= (count source-features)
                          (count (distinct source-features)))
                       (not (seq-contains? existing-site-feature-sets
                                           source-features)))))))))))

(defn- get-creation-monitors
  "Returns a map with groups of source-sites as keys to creation
  monitors as values that can be used to track the level whether to
  create a site for a given set of source-sites.

  The list of source-sites is derived from the provided list of sites,
  and includes groups of source-sites that could potentially be used
  by a new trouble-site.

  The creation-monitor-factory is a function to create new
  monitors.

  If a map of previous monitors is supplied, copy over any
  existing monitors for the same source-sites to the new map.

  If a list of blacklisted-source-sites is provided, do not create
  monitors for those sets of source sites."
  ([sites trouble-factor t-site-input-type shared-sources?
    creation-monitor-factory]
   (get-creation-monitors sites trouble-factor t-site-input-type
                          shared-sources? creation-monitor-factory {} []))
  ([sites trouble-factor t-site-input-type shared-sources?
    creation-monitor-factory previous-monitors blacklisted-source-sites]
   (let [source-sites (difference (set (get-potential-t-site-source-sites
                                        sites shared-sources?))
                                  (set blacklisted-source-sites))]
     (->> source-sites
          (map
           (fn [source-sites]
             [source-sites
              (if (contains? previous-monitors source-sites)
                (get previous-monitors source-sites)
                (creation-monitor-factory source-sites trouble-factor
                                          t-site-input-type))]))
          (into {})))))

;; Updating monitors based on incoming results.

(defn- update-blacklist
  "Update each monitor in the blacklist with the new result (in the
  context of the list of sites), and remove any keys (source-sites)
  from the blacklist where the monitor has fired."
  [blacklist result sites]
  (->> blacklist
       (map-vals #(push-result % result sites))
       (filter #(not (fire? (second %))))
       (into {})))

(defn- update-potential-monitors
  "Return an updated version of the list of monitors where monitors have
  had the result applied to them (in the context of the list of
  sites)."
  [creation-monitors result sites]
  (map-vals #(push-result % result sites)
            creation-monitors))

(defn- update-site-monitors
  "Return an updated version of the list of sites where monitors have
  had the result applied to them."
  [sites result]
  (->> sites
       (map-vals (fn [site]
                   ;; Only process monitors on sites that are not
                   ;; removed and that have monitors.
                   (if (and (not (:removed site))
                            (contains? site :monitor))
                     (update site :monitor
                             #(push-result % result sites))
                     site)))
       (into (sorted-map))))

;; Creating and removing trouble sites.

(defn- create-t-sites!
  "Add t-sites to the sites-atom as deemed needed by the list of
  provided monitors (other arguments are for creating the new
  t-site).

  Return a list of added site indexes."
  [sites-atom creation-monitors trouble-factor t-site-input-type
   shared-sources? schema classifier-generator moving-average-generator
   site-training-time removal-monitor-factory]
  (let [;; Find source-sites where the monitor's criteria for creating
        ;; a new trouble-site has been achieved. Sort by descending
        ;; monitor proportion (e.g. agreement) so that if multiple
        ;; candidate trouble-sites share a source-site, we create the
        ;; one with the highest preparation.
        candidate-source-sites (->> creation-monitors
                                    (filter #(fire? (second %)))
                                    (sort-by #(get-proportion (second %)))
                                    (reverse)
                                    (map first))]
    (loop [candidate-source-sites candidate-source-sites
           all-added-source-sites []
           all-added-feature-sets []
           new-site-indexes []]
      (if-let [source-sites (first candidate-source-sites)]
        (let [new-site-features (->> source-sites
                                     (map #(get @sites-atom %))
                                     (map #(get-site-features % @sites-atom))
                                     (flatten)
                                     (sort))
              t-site-still-valid?
              (if shared-sources?
                ;; If ALLOWING shared sources, ensure the new
                ;; trouble-site is not repeating a set of features
                ;; already processed at a previously added
                ;; trouble-site.
                (not (seq-contains? all-added-feature-sets new-site-features))
                ;; If NOT allowing shared sources, do not add a
                ;; trouble-site if a previously added trouble-site
                ;; shares any of the same source-sites.
                (not-any? #(seq-contains? (flatten all-added-source-sites) %)
                          source-sites))]
          (if t-site-still-valid?
            (let [new-site-index (add-t-site! sites-atom source-sites
                                              trouble-factor
                                              t-site-input-type
                                              schema classifier-generator
                                              moving-average-generator
                                              site-training-time)
                  creation-monitor (get creation-monitors source-sites)]
              ;; Create a removal monitor to determine if/when the site
              ;; should be removed.
              (swap! sites-atom
                     #(assoc-in % [new-site-index :monitor]
                                (removal-monitor-factory
                                 creation-monitor new-site-index)))
              (recur (rest candidate-source-sites)
                     (conj all-added-source-sites source-sites)
                     (conj all-added-feature-sets new-site-features)
                     (conj new-site-indexes new-site-index)))
            (recur (rest candidate-source-sites)
                   all-added-source-sites
                   all-added-feature-sets
                   new-site-indexes)))
        ;; Return the list of new site indexes
        new-site-indexes))))

(defn- remove-t-sites!
  "Remove t-sites from the sites-atom according to the state of the
  monitors stored inside each added t-site.

  For any removed site, invert the monitor's direction and add it back
  into the creation-monitors-atom for the site's source-sites.

  Return a list of list of site indexes that were removed."
  [sites-atom creation-monitors-atom blacklist-atom
   blacklist-monitor-factory trouble-factor]
  (let [;; Get the indexes of trouble-sites where the
        ;; monitor's criteria for deletion has been achieved.
        t-site-indexes-to-remove (->> @sites-atom
                                      ;; Ignore removed sites.
                                      (filter #(not (:removed (second %))))
                                      (filter #(contains? (second %) :monitor))
                                      (filter #(fire? (:monitor (second %))))
                                      (map first))]
    ;; Use doall to force the evaluation of the mapped items (so that
    ;; remove-t-site! is called now).
    (doall
     (map
      (fn [t-site-index]
        (let [removal-reason (get-reason (:monitor (get @sites-atom t-site-index)))
              removed-site-indexes (remove-t-site! sites-atom t-site-index
                                                   removal-reason)
              t-site (get @sites-atom t-site-index)
              source-sites (:source-sites t-site)]
          ;; Add this set of source-sites to the blacklist so that a
          ;; site is not re-tried until the blacklist monitor fires.
          (swap! blacklist-atom #(assoc % source-sites
                                        (blacklist-monitor-factory source-sites
                                                                   trouble-factor)))
          ;; Return the removed site indexes.
          removed-site-indexes))
      t-site-indexes-to-remove))))

;; Creating the dynamic distributed classifier.

(defn dynamic-distributed-classifier
  "Create a new distributed classifier that dynamically alters it's site
  structure."
  [base-site-structure schema classifier-generator moving-average-generator
   window-size trouble-factor t-site-input-type site-training-time shared-sources?
   creation-monitor-factory removal-monitor-factory blacklist-monitor-factory
   p-site-aggregation-rule detrend-confidence? confidence-jitter
   trouble-classifier-generator
   & {:keys [disable-monitors disable-monitor-logging]
      :or {disable-monitors false
           disable-monitor-logging false}}]
  (let [moving-average-generator (or moving-average-generator
                                     (default-moving-average-generator window-size))
        sites-atom (init-sites-atom base-site-structure schema
                                    classifier-generator
                                    moving-average-generator)
        ;; Create a new distributed classifier with a pre-generated
        ;; sites-atom that we retain a reference to so that the
        ;; structure can be manipulated.
        classifier (distributed-classifier base-site-structure schema
                                           classifier-generator window-size
                                           :sites-atom sites-atom
                                           :p-site-aggregation-rule p-site-aggregation-rule
                                           :detrend-confidence? detrend-confidence?
                                           :confidence-jitter confidence-jitter)
        ;; Generate an initial list of t-site creation monitors.
        creation-monitors-atom (atom (get-creation-monitors
                                      (get-sites classifier)
                                      trouble-factor
                                      t-site-input-type
                                      shared-sources?
                                      creation-monitor-factory))
        ;; Maintain a blacklist of source-sites not to create. Keys
        ;; are seqs/vectors of source-sites, and values are monitors
        ;; that fire when the item can be removed from the blacklist.
        blacklist-atom (atom {})
        site-changelog-atom (atom [])]
    (reify
      Classifier
      ;; Each time a record is processed, we delegate to the
      ;; underlying distributed classifier to perform the
      ;; classification, and possibly add and/or remove trouble-sites
      ;; if deemed appropriate.
      (process-record [this record]
        (let [result (process-record classifier record)]
          (when (not disable-monitors)
            ;; Update the blacklist monitors based on the new result,
            ;; removing blacklisted source-sites as necessary.
            (let [blacklist-keys (keys @blacklist-atom)]
              (swap! blacklist-atom
                     #(update-blacklist % result (get-sites this)))
              (let [new-blacklist-keys (keys @blacklist-atom)
                    removed-keys (difference (set blacklist-keys)
                                             (set new-blacklist-keys))]
                (doseq [source-sites removed-keys]
                  (debug
                   (println "Unblacklisted sources:"
                            (->> source-sites
                                 (map #(get @sites-atom %))
                                 (map :label)
                                 (vec))))
                  (swap! site-changelog-atom
                         #(conj % [:unblacklist source-sites (:id record)])))))
            ;; Update the creation monitors based on the new result.
            (swap! creation-monitors-atom
                   #(update-potential-monitors % result (get-sites this)))
            ;; Update the removal monitor for each existing trouble-site
            ;; based on the new result.
            (swap! sites-atom
                   #(update-site-monitors % result))
            (let [;; Create and remove t-sites.
                  created-site-indexes (create-t-sites! sites-atom
                                                        @creation-monitors-atom
                                                        trouble-factor
                                                        t-site-input-type
                                                        shared-sources?
                                                        schema
                                                        trouble-classifier-generator
                                                        moving-average-generator
                                                        site-training-time
                                                        removal-monitor-factory)
                  removed-site-indexes (remove-t-sites! sites-atom
                                                        creation-monitors-atom
                                                        blacklist-atom
                                                        blacklist-monitor-factory
                                                        trouble-factor)
                  ;; Generate log entries.
                  latest-sites (get-sites this)
                  creation-logs (->> created-site-indexes
                                     (map #(get latest-sites %))
                                     (map #(vector :add (:label %) (:id record))))
                  removal-logs (->> removed-site-indexes
                                    (flatten)
                                    (map #(get @sites-atom %))
                                    (map #(vector :remove [(:label %)
                                                           (:removal-reason %)]
                                                  (:id record))))]
              ;; Update the changelog.
              (swap! site-changelog-atom
                     ;; Use (into []) to prevent huge expansion of sequence later.
                     #(into [] (concat % creation-logs removal-logs))))
            ;; Update the list of creation-monitors to reflect the new
            ;; site-structure (we should start/stop monitoring different
            ;; potential trouble-sites), while retaining any existing
            ;; data in monitors we wish to keep.
            (reset! creation-monitors-atom
                    (get-creation-monitors (get-sites this)
                                           trouble-factor
                                           t-site-input-type
                                           shared-sources?
                                           creation-monitor-factory
                                           @creation-monitors-atom
                                           (keys @blacklist-atom))))
          ;; Return the result from the distributed classifier
          (if (or disable-monitors disable-monitor-logging)
            result
            ;; Annotate result with monitor states.
            (assoc result :monitors
                   {:creation (map-vals describe-state @creation-monitors-atom)
                    :removal (->> (vals @sites-atom)
                                  (filter #(not (:removed %)))
                                  (filter #(contains? % :monitor))
                                  (map #(vector (:label %)
                                                (describe-state (:monitor %))))
                                  (into {}))}))))
      ;; Delegate all other protocol functions to the underlying
      ;; distributed-classifier.
      (describe-model [this]
        (describe-model classifier))
      DistributedClassifier
      (get-sites [this]
        (get-sites classifier))
      (get-all-sites [this]
        (get-all-sites classifier))
      (get-site-changelog [this]
        @site-changelog-atom))))
