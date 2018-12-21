(ns hdsm.monitor.threshold)

(defn hoeffding-bound
  [& {:keys [delta r n]}]
  (* (Math/sqrt (/ (Math/log (/ 1 delta))
                   (* 2 n)))
     r))

(defn prepare-proportion-threshold
  [proportion-threshold current-window-size time-threshold]
  (cond
    ;; Static Threshold
    (number? proportion-threshold) proportion-threshold
    ;; Hoeffding Bound
    (= :hoeffding-bound (:type proportion-threshold))
    (hoeffding-bound :delta (:delta proportion-threshold)
                     :r (:r proportion-threshold)
                     :n (+ current-window-size time-threshold))
    :else (throw (Exception. (str "Unsupported threshold: " proportion-threshold)))))

(defn prepare-agreement-proportion-threshold
  [proportion-threshold current-window-size time-threshold expected-agreement]
  (cond
    ;; Hoeffding Bound (smoothed according to expected agreement)
    (= :smoothed-hoeffding-bound (:type proportion-threshold))
    (* (hoeffding-bound :delta (:delta proportion-threshold)
                        :r (:r proportion-threshold)
                        :n (+ current-window-size time-threshold))
       (- 1 (Math/pow (/ expected-agreement 1)
                      (:sharpness proportion-threshold))))
    ;; Fallback to generic proportion threshold types
    :else (prepare-proportion-threshold proportion-threshold
                                        current-window-size
                                        time-threshold)))
