(ns hdsm.monitor.window
  (:require [hdsm.monitor.base
             :as monitor-base :refer [push fire? describe-state]]
            [hdsm.monitor.threshold
             :refer [prepare-proportion-threshold]]))

(defprotocol WindowMonitorProtocol
  (get-proportion [this]
    "Returns the current proportion of window that is measured against
    the threshold.")
  (set-direction [this new-rising?]
    "Change the rising? value of a given window monitor (used to
     invert a monitor).")
  (get-direction [this]
    "Returns the current value of rising?"))

(defrecord NumericWindowMonitor
    [rising? window-size proportion-threshold
     time-threshold window time-counter]
  monitor-base/MonitorProtocol
  (push [this value]
    (let [new-window (take window-size (conj window value))
          new-monitor (assoc this :window new-window)
          threshold (prepare-proportion-threshold proportion-threshold
                                                  (count new-window)
                                                  time-threshold)
          above-threshold? (>= (get-proportion new-monitor) threshold)
          crossed? (= rising? above-threshold?)
          new-time-counter (if crossed? (inc time-counter) 0)]
      (-> new-monitor
          (assoc :time-counter new-time-counter))))
  (fire? [this]
    (>= time-counter time-threshold))
  monitor-base/DescribableMonitorProtocol
  (describe-state [this]
    {:proportion (get-proportion this)
     :threshold (prepare-proportion-threshold proportion-threshold
                                              (count window)
                                              time-threshold)
     :time-counter (/ time-counter time-threshold)})
  WindowMonitorProtocol
  (get-proportion [this]
    (if (empty? window)
      0
      (/ (reduce + window)
         (count window))))
  (set-direction [this new-rising?]
    (if (not= new-rising? rising?)
      (-> this
          (assoc :rising? new-rising?)
          ;; Reset the counter.
          (assoc :time-counter 0))
      this))
  (get-direction [this]
    rising?))

(defn make-numeric-window-monitor
  "Create a new monitor that maintains a window of size window-size of
  numeric values and fires when the proportion (of the window sum
  divided by the window size) rises above/falls below (depending on
  rising?) the proportion-threshold for the given time-threshold."
  [rising? window-size proportion-threshold time-threshold]
  (NumericWindowMonitor.
   ;; Window must be a list for take+conj approach to work.
   rising? window-size proportion-threshold time-threshold '() 0))

(defrecord BinaryWindowMonitor
    [numeric-window-monitor]
  monitor-base/MonitorProtocol
  (push [this value]
    (let [binary-value (if value 1 0)]
      (update this :numeric-window-monitor
              #(push % binary-value))))
  (fire? [this]
    (fire? numeric-window-monitor))
  monitor-base/DescribableMonitorProtocol
  (describe-state [this]
    (describe-state numeric-window-monitor))
  WindowMonitorProtocol
  (get-proportion [this]
    (get-proportion numeric-window-monitor))
  (set-direction [this new-rising?]
    (update this :numeric-window-monitor
            #(set-direction % new-rising?)))
  (get-direction [this]
    (get-direction numeric-window-monitor)))

(defn make-binary-window-monitor
  "Create a new monitor that maintains a window of size window-size of
  binary values and fires when the proportion of true values moves
  rise above/fall below (depending on rising?) the
  proportion-threshold for the given time-threshold."
  [rising? window-size proportion-threshold time-threshold]
  (BinaryWindowMonitor.
   (make-numeric-window-monitor rising? window-size
                                proportion-threshold time-threshold)))
