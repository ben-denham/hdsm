(ns hdsm.fourier.trouble
  (:require
   [hdsm.dataset.base :refer [get-schema get-dataset-id-map get-dataset-cardinalities]]
   [hdsm.classifier.base :refer [describe-model]]
   [hdsm.classifier.distributed.base :refer [get-all-sites]]
   [hdsm.classifier.distributed.sites :refer [get-site-features]]
   [hdsm.fourier.base :refer [fourier-spectrum apply-fourier-spectrum diff-spectra filter-spectrum-by-order]]
   [hdsm.trees.base :refer [moa-tree-model-string->tree wildcard]]
   [hdsm.utils :refer [get-keys map-vals]]
   [hdsm.utils.complex-numbers :refer [c-near-zero?]]
   [clojure.math.combinatorics :refer [cartesian-product]]
   [clojure.pprint :refer [pprint]]))

(defn get-feature-index-set
  "Returns a set of the full-schema indexes of (non-class) features for
  the given site."
  [site sites]
  (set (get-site-features site sites)))

(defn get-source-site-map
  "Returns a map of site-ids to the source-sites of the given
  trouble-site."
  [classifier trouble-site-id]
  (let [sites (get-all-sites classifier)]
    (select-keys sites (:source-sites (get sites trouble-site-id)))))

(defn- trouble-sent?
  "Returns true if the record represented by given result was forwarded
  as trouble from the source-site to the trouble-site."
  [result source-site-id trouble-site-id]
  (-> result
      (get-in [:site-communication trouble-site-id])
      (set)
      (contains? source-site-id)))

(defn- get-trouble-record-ids
  "Returns a sequence of record ids that were forwarded as trouble from
  the source-site to the trouble-site."
  [source-site-id trouble-site-id results]
  (->> results
       (filter #(trouble-sent? % source-site-id trouble-site-id))
       (map :id)))

(defn- mask-features
  [feature-idxs values]
  (map-indexed (fn [i v] (if (contains? feature-idxs i) v wildcard))
               values))

(defn get-logical-record-fragment
  "Returns the record's feature values (excluding the class) with
  features not in the given list of indexes replaced with a wildcard."
  [record feature-idxs]
  (->> record
       (:values)
       (drop-last) ;; Remove class
       (mask-features feature-idxs)))

(defn get-source-site-logical-trouble-fragments
  "Returns a map (keyed by the source-site-ids of the given
  trouble-site) to a sequence of logical record fragments that were
  forwarded as trouble in the given results."
  [dataset classifier results trouble-site-id]
  (let [record-map (get-dataset-id-map dataset)
        sites (get-all-sites classifier)]
    (->> (get-source-site-map classifier trouble-site-id)
         (map (fn [[site-id site]]
                [site-id
                 (->> results
                      (get-trouble-record-ids site-id trouble-site-id)
                      (map #(get record-map %))
                      (map #(get-logical-record-fragment % (get-feature-index-set site sites)))
                      (frequencies))]))
         (filter (comp not-empty second))
         (into {}))))

(defn- compatible-fragments?
  "Returns false if any of the fragments have differing values at any
  feature index (other than wildcards)."
  [fragments]
  (if-not (apply = (map count fragments))
    (throw (Exception. (str "Fragments have different lengths: " fragments))))
  (apply map
         (fn [& values]
           (->> values
                (filter #(not= % wildcard))
                (apply =)))
         fragments))

(defn- unify-fragments
  "Return a single record fragment by merging the given record
  fragments (without wilcards where possible)."
  [fragments]
  (apply map
         (fn [& values]
           (or (->> values
                    (filter #(not= % wildcard))
                    (first))
               wildcard))
         fragments))

(defn- get-difference-spectrum
  "Computes the difference spectrum between the given source-sites."
  [dataset classifier source-site-ids]
  (let [sites (get-all-sites classifier)
        source-sites (->> source-site-ids
                          (map #(get sites %))
                          (map :label)
                          (set))]
    (->> (describe-model classifier)
         (filter #(contains? source-sites (first %)))
         (map (comp :model second))
         (map #(moa-tree-model-string->tree % (get-schema dataset)))
         (map fourier-spectrum)
         (apply diff-spectra))))

(defn spectrum-trouble-filter
  "Filters a set of unified trouble-records based on whether the
  application of the inverse fourier transform of the difference
  spectrum between the trouble-sites results in a value of 'zero'."
  [dataset classifier source-site-ids max-order trouble-records]
  (let [full-diff-spectrum (get-difference-spectrum dataset classifier source-site-ids)
        diff-spectrum (if max-order
                        (filter-spectrum-by-order full-diff-spectrum max-order)
                        full-diff-spectrum)
        feature-cardinalities (drop-last (get-dataset-cardinalities dataset))]
    (->> trouble-records
         (filter #(not (c-near-zero? (apply-fourier-spectrum diff-spectrum %
                                                             feature-cardinalities)))))))

(defn trouble-fragment-combinations
  "Builds unified trouble-records from all possible combinations of the
  fragments from multiple sites."
  [sites-fragments]
  (->> sites-fragments
       (apply cartesian-product)
       (filter compatible-fragments?)
       (map unify-fragments)))

(defn- fragment-completions
  [fragment schema wildcard-idxs]
  (let [wildcards-record (repeat (count fragment) wildcard)]
    (->> fragment
         (map-indexed
          (fn [idx value]
            (if (not= wildcard value)
              [value]
              (if (contains? wildcard-idxs idx)
                [wildcard]
                (->> (get schema idx)
                     (:options)
                     (count)
                     (range))))))
         (apply cartesian-product))))

(defn trouble-fragment-possibilities
  "Builds trouble-records from all possibilities of independently
  expanding fragments from many sites."
  [schema sites-fragments]
  (let [wildcard-idxs (->> sites-fragments
                           (map first)
                           (unify-fragments)
                           (map-indexed vector)
                           (filter #(= wildcard (second %)))
                           (map first)
                           (set))]
    (->> sites-fragments
         (apply concat)
         (map #(fragment-completions % schema wildcard-idxs))
         (apply concat)
         (distinct))))

(defn- get-site-record-fragments
  "Return the site-local record fragments for the given logical
  records."
  [site sites logical-records]
  (let [feature-idxs (get-feature-index-set site sites)]
    (->> logical-records
         (map #(mask-features feature-idxs %))
         (distinct))))

(defn sites-record-fragments
  "Return a map of site ids to site-local record fragments for the given
  logical records."
  [site-map sites records]
  (->> site-map
       (map-vals #(get-site-record-fragments % sites records))))

(defn get-fourier-based-trouble-records
  "Return the site-local logical record-fragments for all
  trouble-records as determined by the fourier-based approach."
  [dataset classifier results trouble-site-id max-order]
  (let [source-site-map (get-source-site-map classifier trouble-site-id)
        sites (get-all-sites classifier)]
    (->> (get-source-site-logical-trouble-fragments dataset classifier
                                                    results trouble-site-id)
         (vals)
         (map keys)
         (trouble-fragment-combinations)
         (spectrum-trouble-filter dataset classifier (keys source-site-map) max-order)
         (sites-record-fragments source-site-map sites))))
