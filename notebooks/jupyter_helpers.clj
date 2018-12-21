(ns jupyter-helpers
  (:require [clojure.string :as string]
            [hdsm.utils :refer [map-vals percent
                                                         seq-contains? *debug*
                                                         safe-division]]
            [hdsm.trees.graph-tree :refer [model->dot]]
            [hdsm.training
             :refer [run-experiment]]
            [hdsm.classifier.distributed.distributed
             :refer [*trouble-selector*]]
            [hdsm.evaluation
             :refer [results-summary temporal-summary
                     get-confusion-frequencies print-confusion-matrix
                     get-class-performance-measures
                     creation-monitor-timelines
                     accuracy-removal-monitor-timelines
                     usage-removal-monitor-timelines
                     get-block-accuracies
                     get-block-summary-statistics]]
            [hdsm.evaluation.timing
             :refer [mean-critical-path-time
                     mean-time-between-responses]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh])
  (:import [com.twosigma.beakerx.table TableDisplay]
           [com.twosigma.beakerx NamespaceClient]
           [com.twosigma.beakerx.chart.legend LegendPosition LegendLayout]
           [com.twosigma.beakerx.chart.xychart Plot CombinedPlot]
           [com.twosigma.beakerx.chart.xychart.plotitem Line ConstantLine Text StrokeType]
           [java.util.concurrent Executors]
           [java.awt Color]
           [com.twosigma.beakerx.widget HTML]))

(defn- format-site-name
  [site-name]
  (-> site-name
      (string/replace #"_i[0-9]+" "")
      (string/replace #":t-" "")
      (string/replace #":p-" "")
      (string/replace #":p" "")
      (string/replace #"<" "[")
      (string/replace #">" "]")
      (string/replace #"-" ", ")))

(defn- format-event [log-event experiment]
  (case (first log-event)
    :drift "Concept Drift"
    :add (str "add " (format-site-name (second log-event)))
    :remove (str "remove " (->> log-event
                                second
                                first
                                format-site-name))
    :unblacklist (str "un-blacklist "
                      (->> (second log-event)
                           (map #(-> (:final-site-structure experiment)
                                     (get %)
                                     (:label)
                                     (format-site-name)))
                           (string/join ", ")
                           ((fn [text] (str "[" text "]")))))))

(defn- format-keyword
  [kw]
  (-> kw
      (str)
      (subs 1)
      (string/replace "-" " ")
      (string/capitalize)))

(defn- keys->strings
  [dict]
  (->> dict
       (map (fn [[k v]] [(str k) v]))
       (into {})))

(defn- format-site-summary-row
  [label usage used-accuracy transmission]
  {"Site" (str label)
   "Usage" (percent usage)
   "Contributed Accuracy" (percent (* usage used-accuracy))
   "Data Transmission" (percent transmission)})

(defn site-summary-table
  [experiment
   & {:keys [skip-records collapse-trouble-sites]
      :or {skip-records 0
           collapse-trouble-sites false}}]
  (let [summary (results-summary (:final-site-structure experiment)
                                 (drop skip-records (:results experiment))
                                 (:site-changelog experiment)
                                 :pretty? false
                                 :skip-records [])
        total-row (format-site-summary-row "Totals" 1 (:accuracy summary)
                                           (:proportion-transmitted summary))
        site-names->labels (->> (:final-site-structure experiment)
                                (into (sorted-map))
                                (vals)
                                (map :label)
                                (group-by format-site-name)
                                (sort))
        site-rows
        (if collapse-trouble-sites
          ;; Aggregate values for all trouble-sites that had the same
          ;; source-sites (same site-name).
          (->> site-names->labels
               (map
                (fn [[site-name site-labels]]
                  (let [combined-usage (->> site-labels
                                            (map #(get (:site-usage summary) % 0))
                                            (reduce +))]
                    (format-site-summary-row
                     site-name
                     combined-usage
                     (->> site-labels
                          ;; Weight average accuracy between sites by usage.
                          (map #(* (get (:site-usage-accuracy summary) % 0)
                                   (safe-division (get (:site-usage summary) % 0)
                                                  combined-usage 0)))
                          (reduce +))
                     (->> site-labels
                          (map #(get (:weighted-site-communication summary) % {}))
                          (map vals)
                          (filter some?)
                          (flatten)
                          (reduce +)))))))
          (->> (:final-site-structure experiment)
               (into (sorted-map))
               (vals)
               (map :label)
               (map
                (fn [site-label]
                  (format-site-summary-row
                   (format-site-name site-label)
                   (get (:site-usage summary) site-label 0)
                   (get (:site-usage-accuracy summary) site-label 0)
                   (->> (get (:weighted-site-communication summary) site-label {})
                        (vals)
                        (reduce +)))))))]
    (TableDisplay. (map keys->strings (concat site-rows [total-row])))))

(defn plot-events
  [experiment y-bound event-rows event-top-padding
   event-spacing log-events]
  (for [[idx log-event] (->> log-events
                             (sort-by last)
                             (map-indexed vector))]
    (let [event-line (doto
                         (ConstantLine.)
                       (.setStyle StrokeType/DOT)
                       (.setColor (case (first log-event)
                                    :add (Color. 0 200 0)
                                    :remove Color/red
                                    :unblacklist Color/gray
                                    :drift Color/blue))
                       (.setX (last log-event)))
          event-text (doto
                         (Text.)
                       (.setX (last log-event))
                       (.setY (- (- y-bound event-top-padding)
                                 (* event-spacing (mod idx event-rows))))
                       (.setText (format-event log-event experiment)))]
      [event-line event-text])))

(defn experiment-timeline
  [experiment other-experiments &
   {:keys [partition-size extra-events
           x-bounds y-bounds-acc y-bounds-data
           event-rows
           include-site-transmission-plot?
           include-timing-plot?
           width height
           event-top-padding
           event-spacing]
    :or {width 750
         height 500
         event-top-padding 0.3
         event-spacing 0.1}}]
  (let [base-colour (Color/decode "#809AE5") ;; https://www.figma.com/file/KePb0YwhpKu45pXZnN9s4TmS/Accessible-Colors-for-Data-Visualization?node-id=0%3A1
        highlight-colour (Color/decode "#D12249")
        other-experiments (if (:label other-experiments)
                            {"Distributed Ensemble" other-experiments}
                            other-experiments)
        experiment-temporal-summary (temporal-summary (:final-site-structure experiment)
                                                      (:results experiment)
                                                      (:site-changelog experiment)
                                                      partition-size)
        other-experiment-temporal-summaries (->> other-experiments
                                                 (vals)
                                                 (map #(temporal-summary (:final-site-structure %)
                                                                         (:results %)
                                                                         (:site-changelog %)
                                                                         partition-size))
                                                 (zipmap (keys other-experiments)))
        x-values (map-indexed #(+ (* %1 partition-size) (:results-count %2))
                              experiment-temporal-summary)
        acc-plot (doto
                     (Plot.)
                   (.setYLabel "Accuracy")
                   (.setLegendPosition LegendPosition/TOP)
                   (.setLegendLayout LegendLayout/HORIZONTAL)
                   (.setXBound (first x-bounds) (second x-bounds))
                   (.setYBound (first y-bounds-acc) (second y-bounds-acc)))
        tran-plot (doto
                      (Plot.)
                    (.setYLabel "Data Transmission")
                    (.setLegendPosition LegendPosition/TOP)
                    (.setLegendLayout LegendLayout/HORIZONTAL)
                    (.setXBound (first x-bounds) (second x-bounds))
                    (.setYBound (first y-bounds-data) (second y-bounds-data)))
        site-tran-plot (doto
                           (Plot.)
                         (.setYLabel "Trouble Site Data Received")
                         (.setLegendPosition LegendPosition/TOP)
                         (.setLegendLayout LegendLayout/HORIZONTAL)
                         (.setXBound (first x-bounds) (second x-bounds))
                         (.setYBound (first y-bounds-data) (second y-bounds-data)))
        timing-plot (doto
                        (Plot.)
                      (.setYLabel "Mean Response Time")
                      (.setLegendPosition LegendPosition/TOP)
                      (.setLegendLayout LegendLayout/HORIZONTAL)
                      (.setXBound (first x-bounds) (second x-bounds)))
        comb-plot (doto
                      (CombinedPlot.)
                    (.setXLabel "Records processed")
                    (.setInitWidth width)
                    (.setInitHeight height)
                    (.add acc-plot 4)
                    (.add tran-plot 2))]
    (when include-site-transmission-plot?
      (.add comb-plot site-tran-plot 2))
    (when include-timing-plot?
      (.add comb-plot timing-plot 2))
    ;; Add HDSM accuracy line
    (.add acc-plot
          (doto
              (Line.)
            (.setX x-values)
            (.setY (map (comp float :accuracy) experiment-temporal-summary))
            (.setColor highlight-colour)
            (.setDisplayName "HDSM")))
    ;; Add Naive accuracy line
    (.add acc-plot
          (doto
              (Line.)
            (.setX x-values)
            (.setY (map (comp float :accuracy)
                        (second (first other-experiment-temporal-summaries))))
            (.setColor base-colour)
            (.setDisplayName (format-keyword (first (first other-experiment-temporal-summaries))))))
    ;; Add timing lines
    (.add timing-plot
          (doto
              (Line.)
            (.setX x-values)
            (.setY (map (comp float :mean-critical-path-time)
                        experiment-temporal-summary))
            (.setColor (Color/decode "#ff96a8"))
            (.setDisplayName "HDSM")))
    (.add timing-plot
          (doto
              (Line.)
            (.setX x-values)
            (.setY (map (comp float :mean-critical-path-time)
                        (second (first other-experiment-temporal-summaries))))
            (.setColor (Color/decode "#1b2126"))
            (.setDisplayName (format-keyword (first (first other-experiment-temporal-summaries))))))
    ;; Add data transmission lines
    (.add tran-plot
          (doto
              (Line.)
            (.setColor (Color/decode "#1b2126"))
            (.setX x-values)
            (.setY (map (comp float :proportion-transmitted) experiment-temporal-summary))
            (.setDisplayName "Total transmission")))
    (.add tran-plot
          (doto
              (Line.)
            (.setColor (Color/decode "#f55774"))
            (.setX x-values)
            (.setY (map (comp float :max-proportion-transmitted-to-any-t-site)
                        experiment-temporal-summary))
            (.setDisplayName "Max transmission to one trouble site")))
    ;; Add site feature proportion lines
    (doseq [[site-idx site-label] (map-vals :label (:final-site-structure experiment))]
      (.add site-tran-plot
            (doto (Line.)
              (.setX x-values)
              (.setY (map (comp float #(get % site-idx 0) :proportions-transmitted-to-sites)
                          experiment-temporal-summary))
              (.setDisplayName (str site-label)))))
    ;; (Optional) Add other accuracy line(s)
    (doseq [[label temporal-summary] (rest other-experiment-temporal-summaries)]
      (.add acc-plot
            (doto (Line.)
              (.setX x-values)
              (.setY (map (comp float :accuracy) temporal-summary))
              (.setDisplayName (str label)))))
    ;; Add lines and text for changelog events
    (doseq [[event-line event-text] (->> (concat (:site-changelog experiment)
                                                 extra-events)
                                         (plot-events experiment (second y-bounds-acc)
                                                      event-rows
                                                      event-top-padding
                                                      event-spacing))]
      (.add acc-plot event-line)
      (.add tran-plot event-line)
      (.add timing-plot event-line)
      (.add site-tran-plot event-line)
      (.add acc-plot event-text))
    comb-plot))

(defn- update-progress-bar
  [beaker counter-atom experiments]
  (let [finished-count @counter-atom
        total-count (count experiments)
        progress (* 100 (/ finished-count total-count))
        message (str finished-count " / " total-count)]
    (.showProgressUpdate beaker message progress)))

(defn save-data [filename data]
  (io/make-parents filename)
  (with-open [w (io/writer filename)]
    (binding [*out* w]
      (pr data)))
  data)

(defn load-data
  [filename]
  (with-open [reader (java.io.PushbackReader. (io/reader filename))]
    (edn/read reader)))

(defn- pmap-pool [thread-count func coll]
  (let [pool (Executors/newFixedThreadPool thread-count)
        results (->> coll
                     (map (fn [item]
                            (fn [] (func item))))
                     (.invokeAll pool)
                     (map #(.get %))
                     (doall))]
    (.shutdown pool)
    results))

(defn run-and-save-experiments
  ([output-dir base-classifier experiments]
   (run-and-save-experiments output-dir base-classifier experiments {}))
  ([output-dir base-classifier experiments & {:keys [beaker-output thread-count]
                                              :or {thread-count 1}}]
   (let [beaker (NamespaceClient/getBeakerX)
         counter-atom (atom 0)
         progress-atoms (->> experiments
                             (map #(vector (:label %) (atom 0)))
                             (into {}))
         summary-keys [:proportion-transmitted :mean-max-transmission-100
                       :accuracy :f-score :mean-critical-path-time]]
     (update-progress-bar beaker counter-atom experiments)
     (when beaker-output
         (doseq [[label progress-atom] progress-atoms]
           (add-watch progress-atom label
                      (fn [key atom old-state new-state]
                        (.appendStdout beaker-output (str key ": Processed " new-state " records"))))))
     (->>
      experiments
      (pmap-pool thread-count
       (fn [experiment]
         ;; Don't run in debug mode, otherwise output is interspersed with
         ;; debug statements.
         (binding [*debug* false
                   *trouble-selector* (or (:trouble-selector experiment)
                                          *trouble-selector*)]
           (let [result (run-experiment base-classifier experiment
                                        (get progress-atoms (:label experiment)))]
             (save-data (str output-dir "/" (:label experiment) ".edn")
                        result)
             (swap! counter-atom inc)
             (update-progress-bar beaker counter-atom experiments)
             (select-keys result [:label :summary])))))
      (doall)
      (map #(hash-map
             :label (:label %)
             :full (select-keys (:summary %) summary-keys)
             :skip-1000 (select-keys (get-in % [:summary :subset-summaries 1000])
                                     summary-keys)))
      (save-data (str output-dir "/:summary.edn"))))))

(defn load-experiment
  [output-dir experiment-label]
  (let [filename (str output-dir "/" experiment-label ".edn")]
    (with-open [reader (java.io.PushbackReader. (io/reader filename))]
      (edn/read reader))))

(defn experiments-summary-table
  [experiments-summary]
  (let [fields [["Label" #(str (:label %))]
                ["Full Total Transmission" #(percent (-> % :full :proportion-transmitted))]
                ["Full Max Transmission 100" #(percent (-> % :full :mean-max-transmission-100))]
                ["Full Mean Critical Path Time" #(float (-> % :full :mean-critical-path-time))]
                ["Full Accuracy" #(percent (-> % :full :accuracy))]
                ;;["Full Weighted-F" #(percent (-> % :full :f-score :weighted-mean))]
                ["Full Unweighted-F" #(percent (-> % :full :f-score :unweighted-mean))]
                ;;["Full Micro-F" #(percent (-> % :full :f-score :micro))]
                ["Skip-1000 Total Transmission" #(percent (-> % :skip-1000 :proportion-transmitted))]
                ["Skip-1000 Max Transmission 100" #(percent (-> % :skip-1000 :mean-max-transmission-100))]
                ["Skip-1000 Mean Critical Path Time" #(float (-> % :skip-1000 :mean-critical-path-time))]
                ["Skip-1000 Accuracy" #(percent (-> % :skip-1000 :accuracy))]
                ;;["Skip-1000 Weighted-F" #(percent (-> % :skip-1000 :f-score :weighted-mean))]
                ["Skip-1000 Unweighted-F" #(percent (-> % :skip-1000 :f-score :unweighted-mean))]
                ;;["Skip-1000 Micro-F" #(percent (-> % :skip-1000 :f-score :micro))]
                ]]
    (as-> experiments-summary $
      (map
       (fn [summary]
         (map #((second %) summary) fields)) $)
      #_(sort-by #(get % "Full Accuracy") $)
      (TableDisplay. $ (map first fields) []))))

(defn display-html [s]
  (doto (HTML.)
    (.setValue s)
    (.display)))

(defn display-table
  [seq-of-maps]
  (->> seq-of-maps
       (map
        (fn [row-map]
          (->> row-map
               (map #(vector (str (first %)) (second %)))
               (into (sorted-map)))))
       (TableDisplay.)
       (.display)))

(defn confusion-summary
  [experiment]
  (let [confusion-frequencies (get-confusion-frequencies (:results experiment))]
    (print-confusion-matrix confusion-frequencies)
    (display-table (get-class-performance-measures confusion-frequencies))))

(defn dot->svg
  "Takes a string containing a GraphViz dot file, and returns a string containing SVG.  This requires that GraphViz
   is installed on the local machine.

  From: https://github.com/ztellman/rhizome/blob/master/src/rhizome/viz.clj#L126"
  [s]
  (let [{:keys [out err]} (sh/sh "dot" "-Tsvg" :in s)]
    (or
     out
     (throw (IllegalArgumentException. ^String (str err))))))

(defn model->svg
  "Takes a MOA model description string and returns an SVG decision tree
  graph."
  [model]
  (let [svg (->> model
                 model->dot
                 dot->svg)]
    (doto (HTML.)
      (.setValue svg))))

(defn display-experiment-models
  [experiment & {:keys [site-keys]}]
  (doseq [[site {:keys [model]}] (:model-description experiment)]
    (when (or (nil? site-keys)
              (seq-contains? site-keys site))
      (println (str "Model for " site))
      (->> model
           model->svg
           .display))))

(defn get-best-experiment-label
  [experiments-summary measure label-portion]
  (->> experiments-summary
       (sort-by #(get-in % measure))
       (map :label)
       (filter #(string/includes? (str %) label-portion))
       first))

(defn load-experiment-pair
  [output-dir experiments-summary performance-measure aggregation-method]
  {:distributed-ensemble (load-experiment output-dir (get-best-experiment-label experiments-summary performance-measure (str "naive" aggregation-method)))
   :hdsm (load-experiment output-dir (get-best-experiment-label experiments-summary performance-measure (str "hdsm" aggregation-method)))})

(defn plot-lines [bin-size lines
                  & {:keys [y-label x-label]}]
  (let [plot (doto (Plot.)
               (.setLegendPosition LegendPosition/TOP)
               (.setLegendLayout LegendLayout/HORIZONTAL)
               (.setXLabel (or x-label ""))
               (.setYLabel (or y-label "")))
        colours (concat [(Color/decode "#D12249")
                         (Color/decode "#809AE5")
                         (Color/decode "#3F4D62")]
                        (repeat nil))]
    (doseq [[[label points] colour] (map vector lines colours)]
      (let [points (->> points (partition bin-size) (map #(/ (reduce + %) (count %))))
            line (doto (Line.)
                   (.setX (map #(* bin-size %) (range (count points))))
                   (.setY (map float points))
                   (.setDisplayName (str label)))]
        (when colour
          (.setColor line colour))
        (.add plot line)))
    plot))

(defn- format-source-sites
  [source-sites]
  (as-> source-sites $
    (string/join ", " $)
    (str "[" $ "]")))

(defn- monitor-timelines-for-source-sites
  [source-sites timelines]
  (->> timelines
       (filter #(= (format-source-sites source-sites)
                   (format-site-name (first %))))
       (map second)
       (apply merge-with (fn [& vs] (apply map + vs)))))

(defn- plot-monitor-timeline
  [timeline]
  (doto (plot-lines 10 [["Monitor Signal" (:proportion timeline)]
                        ["Monitor Threshold" (:threshold timeline)]
                        ["Time Threshold Progress (%)" (:time timeline)]])
    (.setXBound 0 (count (:proportion timeline)))
    (.setYBound 0 1)))

(defn plot-monitor-timelines
  [experiment source-sites
   & {:keys [width height
             extra-events event-rows
             event-top-padding event-spacing]
      :or {event-rows 1
           width 750
           height 500
           event-top-padding 0.2
           event-spacing 0.1}}]
  (let [period-size 10
        results (:results experiment)
        creation-agreement-plot (as-> results $
                                  (creation-monitor-timelines $)
                                  (get $ source-sites)
                                  (plot-monitor-timeline $))
        removal-usage-plot (->> results
                                (usage-removal-monitor-timelines)
                                (monitor-timelines-for-source-sites source-sites)
                                (plot-monitor-timeline))
        removal-accuracy-plot (->> results
                                   (accuracy-removal-monitor-timelines)
                                   (monitor-timelines-for-source-sites source-sites)
                                   (plot-monitor-timeline))
        comb-plot (doto (CombinedPlot.)
                    (.setXLabel "Records processed")
                    (.setInitWidth width)
                    (.setInitHeight height)
                    (.add creation-agreement-plot 2)
                    (.add removal-usage-plot 2)
                    (.add removal-accuracy-plot 1))]
    (.setYLabel creation-agreement-plot "Creation - Agreement")
    (.setYLabel removal-usage-plot "Removal - Utilization")
    (.setYLabel removal-accuracy-plot "Removal - Accuracy")
    (.setShowLegend removal-accuracy-plot false)
    (.setShowLegend removal-usage-plot false)
    ;; Add site events
    (doseq [[event-line event-text]
            (->> (:site-changelog experiment)
                 (filter #(case (first %)
                            :remove (= (format-site-name (first (second %)))
                                       (format-source-sites source-sites))
                            :unblacklist (= source-sites (second %))
                            (= (format-site-name (second %))
                               (format-source-sites source-sites))))
                 (concat extra-events)
                 (plot-events experiment 1 event-rows
                              event-top-padding event-spacing))]
      (.add creation-agreement-plot event-line)
      (.add removal-accuracy-plot event-line)
      (.add removal-usage-plot event-line)
      (.add creation-agreement-plot event-text))
    comb-plot))

(defn get-timing-evaluation
  [output-dir experiment-labels skip-records]
  (for [label experiment-labels]
    (let [experiment (load-experiment output-dir label)
          sites (:final-site-structure experiment)
          results (drop skip-records (:results experiment))]
      {:label label
       :mean-critical-path-time (mean-critical-path-time sites results)
       :mean-time-between-responses (mean-time-between-responses sites results)})))

(defn timing-table
  [timing-evaluation]
  (->> timing-evaluation
       (map (fn [{:keys [label mean-critical-path-time mean-time-between-responses]}]
              {"Experiment" (str label)
               "Mean Critical Path Time" (float mean-critical-path-time)
               "Mean Time Between Responses" (float mean-time-between-responses)}))
       (TableDisplay.)))

(defn get-experiment-block-accuracies
  [output-dir experiment-labels skip-records
   & {:keys [block-size]}]
  (into
   {}
   (for [label experiment-labels]
     (let [results (->> (load-experiment output-dir label)
                        (:results)
                        (drop skip-records))
           block-accuracies (doall
                             (get-block-accuracies
                              results :block-size block-size))]
       (println "finished:" (str label))
       [label block-accuracies]))))

(defn experiment-block-accuracies-table
  [experiment-block-accuracies]
  (for [[experiment block-accuracies] experiment-block-accuracies]
    (->> block-accuracies
         (get-block-summary-statistics)
         (map-vals (comp percent float))
         (merge {:label (str experiment)}))))
