(ns hdsm.monitor.base)

(defprotocol MonitorProtocol
  (push [this value]
    "Return a copy of this monitor with updated state to reflect any
    affect from the new value.")
  (fire? [this]
    "Return true if the event the monitor is monitoring for has been
    fired."))

(defprotocol ResultMonitorProtocol
  (push-result [this result sites]
    "Push a new value to the monitor for the given result in the
    context of the given sorted-map of sites.

    The full sorted-map of sites must be provided as context."))

(defprotocol DescribableMonitorProtocol
  (describe-state [this]
    "Return a description of the current state of this monitor"))

(defprotocol ReasonedMonitorProtocol
  (get-reason [this]
    "Get a short description of the reason this monitor fired."))

(defn make-never-monitor
  "Create a monitor that never fires and ignores all input."
  []
  (reify
    MonitorProtocol
    (push [this value]
      this)
    (fire? [this]
      false)
    ResultMonitorProtocol
    (push-result [this result sites]
      this)))

(defn make-always-monitor
  "Create a monitor that always fires and ignores all input."
  []
  (reify
    MonitorProtocol
    (push [this value]
      this)
    (fire? [this]
      true)
    ResultMonitorProtocol
    (push-result [this result sites]
      this)))
