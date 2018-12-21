(ns hdsm.dataset.probabilistic
  (:require [hdsm.utils.random
             :refer [seeded-rng next-float! random-choice!]]
            [clojure.math.combinatorics
             :refer [cartesian-product]])
  (:import [hdsm.dataset.base StaticDataset]))

(defn- records->schema
  [records]
  (let [col-count (count (first records))
        column-names (->> (range 1 (inc col-count))
                          (map #(str "F" %)))]
    (->> column-names
         (map-indexed
          (fn [col-idx col-name]
            {:name col-name
             :options (->> records
                           (map #(nth % col-idx))
                           (map str)
                           (flatten)
                           (distinct)
                           (sort)
                           (into []))}))
         (into []))))

(defn make-probabilistic-dataset
  ([record-fn record-count]
   (make-probabilistic-dataset record-fn record-count 1))
  ([record-fn record-count random-seed]
   (let [rng (seeded-rng random-seed)
         records (->> (range record-count)
                      (map (fn [_] (record-fn rng)))
                      (into []))]
     (StaticDataset.
      (records->schema records)
      (map-indexed
       (fn [idx record]
         {:id (inc idx) :values record})
       records)))))

(defn next-bool-for-prob!
  [rng probability]
  ;; Use less-than because the next float is exclusive of 1.0.
  (< (next-float! rng) probability))

(defn possible-inputs
  "Return all possible feature inputs for the given number of boolean
  features."
  [boolean-features]
  (->> (repeat [true false])
       (take boolean-features)
       (apply cartesian-product)))

(defn make-balanced-probabilistic-dataset
  ([feature-count boolean-class-fn noise record-count]
   (make-balanced-probabilistic-dataset feature-count boolean-class-fn
                                        noise record-count 1))
  ([feature-count boolean-class-fn noise record-count random-seed]
   (let [class->feature-inputs (->> (possible-inputs feature-count)
                                    (group-by #(apply boolean-class-fn %)))
         make-record
         (fn [rng]
           (let [bool-class (next-bool-for-prob! rng 0.5)
                 bool-features (->> bool-class
                                    (get class->feature-inputs)
                                    (random-choice! rng))
                 ;; Flip the class noise% of the time
                 final-class (if (next-bool-for-prob! rng noise)
                               (not bool-class)
                               bool-class)]
             (map #(if % 1 0) (concat bool-features [final-class]))))]
     (make-probabilistic-dataset make-record record-count random-seed))))
