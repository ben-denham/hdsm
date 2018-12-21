(ns hdsm.dataset.tree-based
  (:require
   [hdsm.utils.random
    :refer [seeded-rng next-int!]]
   [hdsm.trees.base
    :refer [apply-tree]]
   [hdsm.dataset.base])
  (:import [hdsm.dataset.base StaticDataset]))

(defn- generate-random-record!
  "Generate a new random record of nominal values generated from the
  given random number generator (rng) and with a class value
  determined by the given tree."
  [tree rng id]
  (let [feature-values (->> (:cardinalities tree)
                            (map #(next-int! rng %)))
        class (apply-tree tree feature-values)]
    {:id id
     :values (conj (vec feature-values) class)}))

(defn- get-tree-schema
  "Returns a dataset schema derived from the given tree"
  [tree]
  (->> (map vector (:features tree) (:cardinalities tree))
       (map (fn [[feature cardinality]]
              {:name feature
               :options (vec (map #(str "v" %) (range cardinality)))}))
       (into [])
       ((fn [coll] (conj coll {:name "class"
                               :options (vec (map #(str "c" %) (range 2)))})))))

(defn tree-based-dataset
  "Generate a random (determined by the random-seed) dataset with
  nominal features and and binary class values determined by the given
  tree. Never generate more than records-limit records."
  [tree random-seed records-limit]
  (let [rng (seeded-rng random-seed)]
    (StaticDataset. (get-tree-schema tree)
                    (map #(generate-random-record! tree rng (inc %))
                         (range records-limit)))))
