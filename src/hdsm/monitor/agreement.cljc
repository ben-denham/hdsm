(ns hdsm.monitor.agreement
  (:require [hdsm.monitor.base
             :as monitor-base :refer [push fire? describe-state]]
            [hdsm.monitor.window
             :as monitor-window
             :refer [make-binary-window-monitor set-direction
                     get-proportion get-direction]]
            [hdsm.monitor.threshold
             :refer [prepare-agreement-proportion-threshold]]
            [hdsm.classifier.distributed.distributed
             :refer [is-troubled? compute-agreement-factor]]
            [hdsm.classifier.distributed.sites
             :refer [get-site-features]]
            [hdsm.utils
             :refer [bool-to-int]]))

(defn- get-combined-feature-count
  [site-indexes sites]
  (->> site-indexes
       (map #(get sites %))
       (map #(get-site-features % sites))
       (flatten)
       (distinct)
       (count)))

(defn sites-agree?
  "Returns a Boolean value indicating whether all of the source sites
  for a theoretical trouble-site agree that a given
  record (represented by the site-results) is 'trouble'.

  The full sorted-map of sites as well as the map of site-results must
  be provided as context for the
  [[hdsm.classifier.distributed.distributed/is-troubled?]]
  function.

  The trouble-factor argument represents the trouble-factor that is
  intended to be used at the theoretical trouble-site."
  [site-indexes sites site-results trouble-factor t-site-input-type]
  (let [total-feature-count (get-combined-feature-count site-indexes sites)]
    (every? #(is-troubled? % sites site-results
                           total-feature-count (count site-indexes)
                           trouble-factor site-indexes t-site-input-type)
            site-indexes)))

(defrecord AgreementMonitor
    [source-site-indexes trouble-factor t-site-input-type binary-window-monitor]
  monitor-base/MonitorProtocol
  (fire? [this]
    (fire? binary-window-monitor))
  monitor-base/ResultMonitorProtocol
  (push-result [this result sites]
    ;; All agreement monitors are updated for incorrectly classified
    ;; records, but only negative agreement monitors (used for
    ;; existing trouble-sites) are updated for correctly classified
    ;; records. Don't update the monitor if the record was correctly
    ;; classified and the monitor is looking for rising above the
    ;; threshold.
    (if (or (not (:correct? result))
            (not (get-direction binary-window-monitor)))
      (let [agree? (sites-agree? source-site-indexes sites
                                 (:breakdown result)
                                 trouble-factor
                                 t-site-input-type)]
        (update this :binary-window-monitor
                #(push % agree?)))
      this))
  monitor-base/DescribableMonitorProtocol
  (describe-state [this]
    (describe-state binary-window-monitor))
  monitor-window/WindowMonitorProtocol
  (get-proportion [this]
    (get-proportion binary-window-monitor))
  (set-direction [this new-rising?]
    (update this :binary-window-monitor
            #(set-direction % new-rising?)))
  (get-direction [this]
    (get-direction binary-window-monitor)))

(defn make-rising-agreement-monitor
  "Returns a new agreement monitor that will fire if agreement rises
  above the given threshold."
  [source-site-indexes trouble-factor t-site-input-type window-size agreement-threshold time-threshold]
  (AgreementMonitor. source-site-indexes trouble-factor t-site-input-type
                     (make-binary-window-monitor true
                                                 window-size
                                                 agreement-threshold
                                                 time-threshold)))

(defn- get-expected-agreement
  "Get the expected level of agreement for a list of windows recording
  transmission from source sites."
  [source-site-transmission-windows]
  (if (zero? (count (first source-site-transmission-windows)))
    0
    (->> source-site-transmission-windows
         ;; Compute the proportion
         ;; transmitted from each source-site
         (map #(/ (reduce + %) (count %)))
         ;; Multiply the transmissions
         ;; together to get the level of
         ;; agreement expected if
         ;; trouble-records were picked
         ;; randomly
         (reduce *))))

(defn- significant-agreement-threshold
  "Compute the threshold for a SignificantAgreement monitor based on the
  current level of expected agreement and the threshold
  configuration."
  [{:keys [agreement-threshold source-site-windows time-threshold]}]
  (let [expected-agreement (get-expected-agreement (vals source-site-windows))
        required-increase-threshold (prepare-agreement-proportion-threshold
                                     agreement-threshold
                                     (count (first (vals source-site-windows)))
                                     time-threshold
                                     expected-agreement)
        ;; The threshold is capped to never expect greater than 100%
        ;; agreement.
        capped-threshold (min 1 (+ expected-agreement
                                   required-increase-threshold))]
    capped-threshold))

(defrecord SignificantAgreementMonitor
    [source-site-indexes trouble-factor t-site-input-type
     window-size agreement-threshold time-threshold
     rising? source-site-windows time-counter]
  monitor-base/MonitorProtocol
  (fire? [this]
    (>= time-counter time-threshold))
  monitor-base/ResultMonitorProtocol
  (push-result [this result sites]
    ;; All agreement monitors are updated for incorrectly classified
    ;; records, but only negative agreement monitors (used for
    ;; existing trouble-sites) are updated for correctly classified
    ;; records. Don't update the monitor if the record was correctly
    ;; classified and the monitor is looking for rising above the
    ;; threshold.
    (if (or (not (:correct? result))
            (not rising?))
      (let [combined-feature-count (get-combined-feature-count source-site-indexes sites)
            new-source-site-windows
            (->> source-site-windows
                 (map
                  (fn [[site-idx window]]
                    ;; Compute whether this site transmitted the
                    ;; record as trouble.
                    (->> (is-troubled? site-idx sites (:breakdown result)
                                       combined-feature-count (count source-site-indexes)
                                       trouble-factor source-site-indexes
                                       t-site-input-type)
                         (bool-to-int)
                         ;; Append to window
                         (conj window)
                         ;; Limit window size
                         (take window-size)
                         ;; Recombine with key for map
                         (vector site-idx))))
                 (into {}))
            new-monitor (assoc this :source-site-windows new-source-site-windows)
            above-threshold? (>= (get-proportion this)
                                 (significant-agreement-threshold new-monitor))
            crossed? (= rising? above-threshold?)
            new-time-counter (if crossed? (inc time-counter) 0)]
        (assoc new-monitor :time-counter new-time-counter))
      this))
  monitor-base/DescribableMonitorProtocol
  (describe-state [this]
    {:proportion (get-proportion this)
     :threshold (significant-agreement-threshold this)
     :time-counter (/ time-counter time-threshold)})
  monitor-window/WindowMonitorProtocol
  (get-proportion [this]
    (let [agreement-window (apply map + (vals source-site-windows))
          source-site-count (count (vals source-site-windows))]
      (compute-agreement-factor agreement-window source-site-count)))
  (set-direction [this new-rising?]
    (if (not= new-rising? rising?)
      (-> this
          (assoc :rising? new-rising?)
          ;; Reset the counter.
          (assoc :time-counter 0))
      this))
  (get-direction [this]
    rising?))

(defn make-significant-agreement-monitor
  "Returns a new agreement monitor that will fire if agreement rises
  above the given threshold."
  [source-site-indexes trouble-factor t-site-input-type
   window-size agreement-threshold time-threshold]
  ;; Window must be a list for take+conj approach to work.
  (let [source-site-windows (into {} (map vector source-site-indexes (repeat '())))]
    (SignificantAgreementMonitor. source-site-indexes trouble-factor t-site-input-type
                                  window-size agreement-threshold time-threshold
                                  true source-site-windows 0)))
