(ns hdsm.classifier.distributed.dynamic-monitors
  (:require [hdsm.monitor.base
             :refer [make-never-monitor make-always-monitor]]
            [hdsm.monitor.agreement
             :refer [make-rising-agreement-monitor
                     make-significant-agreement-monitor]]
            [hdsm.monitor.accuracy
             :refer [make-site-accuracy-monitor
                     make-site-accuracy-usage-monitor]]
            [hdsm.monitor.window
             :refer [set-direction]]
            [hdsm.monitor.drift
             :refer [make-global-accuracy-dd-monitor
                     make-source-agreement-dd-monitor
                     make-source-accuracy-dd-monitor]])
  (:import [hdsm.monitor.agreement
            AgreementMonitor SignificantAgreementMonitor]))

;; t-site creation monitor construction schemes.

(defn make-creation-agreement-monitor-factory
  "Returns a factory function for creating creation-monitors that
  monitor the agreement between the set of source sites."
  [agreement-window-size agreement-threshold time-threshold]
  (fn [source-sites trouble-factor t-site-input-type]
    (make-rising-agreement-monitor source-sites
                                   trouble-factor
                                   t-site-input-type
                                   agreement-window-size
                                   agreement-threshold
                                   time-threshold)))

(defn make-creation-significant-agreement-monitor-factory
  "Returns a factory function for creating creation-monitors that
  monitor for a significant level of agreement between the set of
  source sites."
  [agreement-window-size agreement-threshold time-threshold]
  (fn [source-sites trouble-factor t-site-input-type]
    (make-significant-agreement-monitor source-sites
                                        trouble-factor
                                        t-site-input-type
                                        agreement-window-size
                                        agreement-threshold
                                        time-threshold)))

;; t-site removal monitor construction schemes.

(defn make-removal-inverted-agreement-monitor-factory
  "Returns a factory function for creating t-site removal monitors
  that monitor the agreement between the t-site's source
  sites (achieved by inverting the existing creation-monitor, which is
  assumed to be an agreement monitor)."
  []
  (fn [creation-monitor site-index]
    (when (or (not= AgreementMonitor (type creation-monitor))
              (not= SignificantAgreementMonitor (type creation-monitor)))
      (throw (Exception. "Creation monitor must be an agreement monitor.")))
    (set-direction creation-monitor false)))

(defn make-removal-accuracy-monitor-factory
  "Returns a factory function for creating t-site removal monitors that
  monitor the accuracy of the t-site."
  [evaluation-scheme window-size accuracy-threshold time-threshold]
  (fn [creation-monitor site-index]
    (make-site-accuracy-monitor evaluation-scheme site-index
                                window-size accuracy-threshold
                                time-threshold)))

(defn make-removal-accuracy-usage-monitor-factory
  "Returns a factory function for creating t-site removal monitors that
  monitor the usage and usage-accuracy of the t-site."
  [window-size accuracy-threshold usage-threshold time-threshold]
  (fn [creation-monitor site-index]
    (make-site-accuracy-usage-monitor site-index window-size
                                      accuracy-threshold usage-threshold
                                      time-threshold)))

;; Blacklist monitor construction schemes.

(defn make-blacklist-permanent-monitor-factory
  "Returns a factory function for creating blacklist monitors that
  keep the source-sites permanently blacklisted."
  []
  (fn [source-sites trouble-factor]
    (make-never-monitor)))

(defn make-blacklist-immediate-removal-monitor-factory
  "Returns a factory function for creating blacklist monitors that
  immediately remove source-sites from the blacklist."
  []
  (fn [source-sites trouble-factor]
    (make-always-monitor)))

(defn make-blacklist-global-accuracy-monitor-factory
  "Returns a factory function for creating blacklist monitors that fire
  when the global accuracy drifts."
  []
  (fn [source-sites trouble-factor]
    (make-global-accuracy-dd-monitor)))

(defn make-blacklist-source-agreement-monitor-factory
  "Returns a factory function for creating blacklist monitors that fire
  when the source-site agreement drifts."
  []
  (fn [source-sites trouble-factor]
    (make-source-agreement-dd-monitor source-sites trouble-factor)))

(defn make-blacklist-source-accuracy-monitor-factory
  "Returns a factory function for creating blacklist monitors that fire
  when any source-site accuracy drifts."
  []
  (fn [source-sites trouble-factor]
    (make-source-accuracy-dd-monitor source-sites)))
