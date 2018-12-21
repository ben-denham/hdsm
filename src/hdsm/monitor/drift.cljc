(ns hdsm.monitor.drift
  (:require [hdsm.monitor.base
             :as monitor-base :refer [push fire?]]
            [hdsm.monitor.agreement
             :refer [sites-agree?]])
  #?(:clj (:import [moa.classifiers.core.driftdetection ADWINChangeDetector])))

;; Simple drift-detection monitor

(defn- make-dd-monitor
  "Create a simple drift-detection monitor that fires when the
  underlying drift detector fires (based on boolean input values)."
  []
  (let [drift-detector (ADWINChangeDetector.)]
    (reify monitor-base/MonitorProtocol
      (push [this value]
        ( .input drift-detector (if value 1 0))
        this)
      (fire? [this]
        (.getChange drift-detector)))))

;; Global Accuracy drift-detection monitor

(defrecord GlobalAccuracyDDMonitor
    [dd-monitor]
  monitor-base/MonitorProtocol
  (fire? [this]
    (fire? dd-monitor))
  monitor-base/ResultMonitorProtocol
  (push-result [this result sites]
    (let [correct? (= (:truth result) (:class result))]
      (update this :dd-monitor #(push % correct?)))))

(defn make-global-accuracy-dd-monitor
  "Make a drift detection monitor that monitors the global
  classification accuracy."
  []
  (GlobalAccuracyDDMonitor. (make-dd-monitor)))

;; Source agreement drift-detection monitor

(defrecord SourceAgreementDDMonitor
    [source-site-indexes trouble-factor dd-monitor]
  monitor-base/MonitorProtocol
  (fire? [this]
    (fire? dd-monitor))
  monitor-base/ResultMonitorProtocol
  (push-result [this result sites]
    (let [agree? (sites-agree? source-site-indexes sites
                               (:breakdown result) trouble-factor)]
      (update this :dd-monitor #(push % agree?)))))

(defn make-source-agreement-dd-monitor
  "Make a drift detection monitor that monitors the agreement between a
  group of source sites."
  [source-site-indexes trouble-factor]
  (SourceAgreementDDMonitor. source-site-indexes
                             trouble-factor (make-dd-monitor)))

;; Source accuracy drift-detection monitor

(defn- update-source-site-dd-monitors
  "Take a mapping of source-site-indexes to dd-monitors and return an
  updated mapping where the classification result of each site has
  been applied (if the site classified the record)."
  [source-site-dd-monitors result]
  (->> source-site-dd-monitors
       (map
        (fn [[source-site-index dd-monitor]]
          [source-site-index
           ;; Only update the monitor if the class actually classified
           ;; the record.
           (if-let [site-class (get-in result [:breakdown
                                               source-site-index
                                               :class])]
             (push dd-monitor (= site-class (:truth result)))
             dd-monitor)]))
       (doall)
       (into {})))

(defrecord SourceAccuracyDDMonitor
    [source-site-dd-monitors]
  monitor-base/MonitorProtocol
  (fire? [this]
    ;; Fire if any of the source-site accuracy DD monitors fire.
    (some fire? (vals source-site-dd-monitors)))
  monitor-base/ResultMonitorProtocol
  (push-result [this result sites]
    ;; Update the map of dd-monitors with the new result
    (update this :source-site-dd-monitors
            #(update-source-site-dd-monitors % result))))

(defn make-source-accuracy-dd-monitor
  "Make a drift detection monitor that fires when any drift-detector
  fires from monitoring the accuracy at each source-site."
  [source-site-indexes]
  (SourceAccuracyDDMonitor. (->> source-site-indexes
                                 (map #(vector % (make-dd-monitor)))
                                 (into {}))))
