(ns hdsm.monitor.accuracy
  "IMPORTANT NOTE: In all accuracy monitors, higher values indicate
  worse accuracy, as an empty window needs to assume (since we use
  accuracy classifiers to detect drops in accuracy that should lead to
  site removal)."
  (:require [hdsm.monitor.base
             :as monitor-base :refer [push push-result fire? describe-state]]
            [hdsm.monitor.window
             :as monitor-window
             :refer [make-numeric-window-monitor make-binary-window-monitor
                     get-proportion get-direction set-direction]]
            [hdsm.monitor.threshold
             :refer [prepare-proportion-threshold]]
            [hdsm.classifier.distributed.sites
             :refer [is-training? get-active-sites get-all-source-sites]]
            [hdsm.classifier.distributed.aggregation
             :refer [classification-without-sites]]
            [hdsm.utils
             :refer [seq-contains? debug]]))

;; Accuracy evaluation helpers

(defn- site-correct?
  "Returns a boolean indicating whether the given site classified the
  record correctly."
  [site-index result]
  (let [site-class (get-in result [:breakdown site-index :class])]
    (= site-class (:truth result))))

(defn- site-used?
  "Returns a boolean indicating whether the given site was used as the
  final classification for the record."
  [site-index result sites]
  (let [site-label (get-in sites [site-index :label])]
    (= site-label (:site-label result))))

;; Accuracy evaluation schemes

(defn- evaluate-site-total-accuracy
  "Monitor the accuracy of a site's classifier on all records that reach
  it.

  Return nil if the record was not classified by the site, 0 if the
  classification was correct, and 1 if the classification was
  incorrect."
  [site-index result sites]
  ;; Return nil (don't update the monitor) if the site did not
  ;; classify this record.
  (if (not (contains? (:breakdown result) site-index))
    nil
    (if (site-correct? site-index result) 0 1)))

(defn- evaluate-site-only-used-accuracy
  "Monitor the accuracy of a site's classifier only on records that it
  was used as the final classification for.

  Return nil if the site's classification was not used as the final
  classification, 0 if the classification was correct, and 1 if the
  classification was incorrect."
  [site-index result sites]
  ;; Return nil (don't update the monitor) if the site's
  ;; classification was not used as the final classification.
  (if (not (site-used? site-index result sites))
    nil
    (evaluate-site-total-accuracy site-index result sites)))

(defn- evaluate-site-usage-weighted-accuracy
  "Monitor the accuracy of a site's classifier only on records that is
  was used as the final classification for, but if the site classifier
  is unused, weight it as partially inaccurate.

  Return 1 if the site's classification was not used as the final
  classification, 0 if the classification was correct, and 2 if the
  classification was incorrect."
  [site-index result sites]
  (if (not (site-used? site-index result sites))
    1
    (if (site-correct? site-index result) 0 2)))

;; No longer supported, because aggregate-site-results may be
;; stateful (e.g. for a stacked classifier).
;;
;; (defn- evaluate-site-contributed-accuracy
;;   "Monitor the accuracy of a site's classifier only on records that is
;;   was used as the final classification for, but if the site classifier
;;   is unused or if the same correct final classification would have
;;   been reached without the classifier, weight it as partially
;;   inaccurate.

;;   Return 1 if the site's classification was not used as the final
;;   classification, 2 if the classification was incorrect, 1 if the
;;   classification was correct but the same classification would have
;;   been reached without this site, and 0 if the classification was
;;   correct and the same classification would not have been reached
;;   without this site."
;;   [site-index result sites]
;;   (if (not (site-used? site-index result sites))
;;     1
;;     (if (not (site-correct? site-index result))
;;       2
;;       (let [next-best-class (-> result
;;                                 (:breakdown)
;;                                 (dissoc site-index)
;;                                 (aggregate-site-results)
;;                                 (:class))]
;;         (if (= (:class result) next-best-class)
;;           1
;;           0)))))

(def accuracy-evaluation-schemes
  {:total evaluate-site-total-accuracy
   :only-used evaluate-site-only-used-accuracy
   :usage-weighted evaluate-site-usage-weighted-accuracy
   ;;:contributed evaluate-site-contributed-accuracy
   })

;; Accuracy monitor

(defrecord AccuracyMonitor
    [accuracy-fn site-index numeric-window-monitor]
  monitor-base/MonitorProtocol
  (fire? [this]
    (fire? numeric-window-monitor))
  monitor-base/ResultMonitorProtocol
  (push-result [this result sites]
    ;; Don't monitor the site if it is in training mode or if it is
    ;; used as a source site to another active site.
    (if (or (is-training? (get site-index sites))
            (seq-contains? (->> sites
                                (get-active-sites)
                                (get-all-source-sites))
                           site-index))
      this
      (let [accuracy-value (accuracy-fn site-index result sites)]
        ;; Do not update the monitor if the accuracy-fn returns nil.
        (if (= nil accuracy-value)
          this
          (update this :numeric-window-monitor
                  #(push % accuracy-value))))))
  monitor-base/DescribableMonitorProtocol
  (describe-state [this]
    (describe-state numeric-window-monitor))
  monitor-window/WindowMonitorProtocol
  (get-proportion [this]
    (get-proportion numeric-window-monitor))
  (set-direction [this new-rising?]
    (update this :numeric-window-monitor
            #(set-direction % new-rising?)))
  (get-direction [this]
    (get-direction numeric-window-monitor)))

(defn make-site-accuracy-monitor
  "Create a new site accuracy monitor that will track the accuracy of
  the site at the given site-index using the selected evaluation
  scheme. The monitor will fire if accuracy drops below the given
  accuracy-threshold. Other parameters control the underlying numeric
  window monitor."
  [evaluation-scheme site-index window-size accuracy-threshold time-threshold]
  (if-let [accuracy-fn (get accuracy-evaluation-schemes evaluation-scheme)]
    (AccuracyMonitor. accuracy-fn site-index
                      (make-numeric-window-monitor true
                                                   window-size
                                                   ;; The monitor will track for
                                                   ;; increasing inaccuracy.
                                                   (- 1 accuracy-threshold)
                                                   time-threshold))
    (throw (Exception. (str "Unknown accuracy evaluation scheme: "
                            evaluation-scheme)))))

;; Accuracy-Usage Monitor

(defn- accuracy-gain-threshold
  [{:keys [gain-threshold base-accuracy-window time-threshold]}]
  (+ (prepare-proportion-threshold gain-threshold
                                   (count base-accuracy-window)
                                   time-threshold)
     (if (empty? base-accuracy-window)
       0
       (/ (reduce + base-accuracy-window)
          (count base-accuracy-window)))))

(defrecord AccuracyGainMonitor
    [site-index rising? window-size gain-threshold time-threshold
     accuracy-window base-accuracy-window time-counter]
  monitor-base/MonitorProtocol
  (fire? [this]
    (>= time-counter time-threshold))
  monitor-base/ResultMonitorProtocol
  (push-result [this result sites]
    (let [site-order (get-in result [:candidate-results site-index :site-order])
          same-or-higher-order-site-indexes (->> (:candidate-results result)
                                                 (filter #(>= (:site-order (second %)) site-order))
                                                 (map first))
          lower-order-sites-result (classification-without-sites result same-or-higher-order-site-indexes)
          lower-order-sites-correct (= (:truth result) (:class lower-order-sites-result))
          new-monitor (assoc this
                             :accuracy-window (take window-size
                                                    (conj accuracy-window
                                                          (if (site-correct? site-index result) 1 0)))
                             :base-accuracy-window (take window-size
                                                         (conj base-accuracy-window
                                                               (if lower-order-sites-correct 1 0))))
          above-threshold? (>= (get-proportion new-monitor)
                               (accuracy-gain-threshold new-monitor))
          crossed? (= rising? above-threshold?)
          new-time-counter (if crossed? (inc time-counter) 0)]
      (-> new-monitor
          (assoc :time-counter new-time-counter))))
  monitor-base/DescribableMonitorProtocol
  (describe-state [this]
    {:proportion (get-proportion this)
     :threshold (accuracy-gain-threshold this)
     :time-counter (/ time-counter time-threshold)})
  monitor-window/WindowMonitorProtocol
  (get-proportion [this]
    (if (empty? accuracy-window)
      0
      (/ (reduce + accuracy-window)
         (count accuracy-window)))))

(defn- make-accuracy-gain-monitor
  [site-index rising? window-size gain-threshold time-threshold]
  (AccuracyGainMonitor. site-index rising? window-size gain-threshold
                        time-threshold '() '() 0))

(defrecord AccuracyUsageMonitor
    [site-index accuracy-monitor usage-monitor]
  monitor-base/MonitorProtocol
  (fire? [this]
    (or (fire? accuracy-monitor) (fire? usage-monitor)))
  monitor-base/ResultMonitorProtocol
  (push-result [this result sites]
    ;; Don't monitor the site if:
    ;; 1. It is used as a source site to another active site.
    (if (seq-contains? (->> sites
                            (get-active-sites)
                            (get-all-source-sites))
                       site-index)
      this
      (let [site-used? (site-used? site-index result sites)
            ;; Push usage
            new-usage-monitor (push usage-monitor site-used?)
            ;; Only monitor the accuracy gain if the site was used for
            ;; the final classification.
            new-accuracy-monitor (if site-used?
                                   (push-result accuracy-monitor
                                                result sites)
                                   accuracy-monitor)]
        (-> this
            (assoc :accuracy-monitor new-accuracy-monitor)
            (assoc :usage-monitor new-usage-monitor)))))
  monitor-base/DescribableMonitorProtocol
  (describe-state [this]
    {:accuracy (describe-state accuracy-monitor)
     :usage (describe-state usage-monitor)})
  monitor-base/ReasonedMonitorProtocol
  (get-reason [this]
    (cond
      (and (fire? accuracy-monitor)
           (fire? usage-monitor)) :accuracy-usage
      (fire? accuracy-monitor) :accuracy
      (fire? usage-monitor) :usage
      :else :not-fired)))

(defn make-site-accuracy-usage-monitor
  "Create a new site monitor that simultaneously monitors the usage of
  the site and the accuracy gain when the site is used, and uses
  separate thresholds for each. Other parameters control the
  underlying numeric window monitor."
  [site-index window-size accuracy-threshold usage-threshold time-threshold]
  (when (not (number? usage-threshold))
    (throw (Exception. "Only static thresholds are currently supported for usage.")))
  (AccuracyUsageMonitor.
   site-index
   (make-accuracy-gain-monitor site-index
                               ;; The monitor will track for falling
                               ;; accuracy.
                               false
                               window-size
                               accuracy-threshold
                               time-threshold)
   ;; The monitor will track for falling
   ;; usage.
   (make-binary-window-monitor false
                               window-size
                               usage-threshold
                               time-threshold)))
