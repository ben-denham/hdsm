(ns hdsm.classifier.distributed.sites
  "Functions to aid the construction of site-structures,
  initialise them into lists of usable sites, and also modify lists of
  active sites.

  In general, programmer friendly `p-site` and `t-site` `site-defs`
  are processed to produce a machine-friendly `site-structure` which
  is then initialised into a list of `sites` that can be used by a
  distributed classifier."
  (:require [clojure.string :as string]
            [hdsm.classifier.base
             :refer [process-record describe-model]]
            [hdsm.dataset.base
             :refer [get-schema pop-record-and-rest]]
            [hdsm.utils
             :refer [index-of max-fn safe-division seq-contains?
                     seq-is-subset? get-keys indexes-where
                     keys-where debug]]
            [hdsm.utils.random
             :refer [seeded-rng]])
  (:import [hdsm.classifier.base Classifier]
           [hdsm.dataset.base Dataset]))

;; SITE STRUCTURE DEFINITION

(defn p-site
  "Create a primary-site definition that can be passed to
  [[make-site-structure]]."
  [label attributes]
  {:type :p-site
   :label label
   :attributes (into [] attributes)})

(defn t-site
  "Create a trouble-site definition that can be passed to
  [[make-site-structure]]."
  ([label source-site-labels trouble-factor
    & {:keys [input-type]}]
   (let [input-type (or input-type :features)]
     (if (not (contains? #{:features :classes} input-type))
       (throw (IllegalArgumentException. (str "Unknown t-site input-type: " input-type))))
     {:type :t-site
      :label label
      :source-sites (into [] source-site-labels)
      :trouble-factor trouble-factor
      :input-type input-type})))

(defn pre-computed
  "Associate a p-site or t-site definition with a list of pre-computed
  results to be used instead of creating an actual classifier when
  `init-sites-atom` is called."
  [site-def results]
  (assoc site-def :computed-results results))

(defn t-site-label
  "Convenience function for generating a useful trouble-site label from
  a list of source-site labels."
  [source-site-labels]
  (->> source-site-labels
       (map #(string/replace % #":[tp]-" ""))
       (string/join "-")
       (#(str "t-<" % ">"))
       (keyword)))

(defn dynamic-t-site-label
  "Convenience function for generating a useful (in the context of a
  dynamic distributed classifier) trouble-site label from a list of
  source-site labels and the index of the new site."
  [source-site-labels new-site-idx]
  (-> (t-site-label source-site-labels)
      ;; Convert keyword to string
      (str)
      ;; Remove the first character (":" from
      ;; keyword conversion)
      (subs 1)
      ;; Remove the final ">" (changes type to
      ;; sequence).
      (drop-last)
      ;; Revert sequence to string.
      (string/join)
      ;; Append site-index in case multiple sites
      ;; are created/removed with the same
      ;; source-sites.
      (str "_i" new-site-idx ">")
      ;; Convert to keyword
      (keyword)))

(defn- update-site-defs
  "Update the given site definitions by applying the given func to each
  value associated with the given key in the site (ignoring any sites
  that do not have that key set)."
  [site-defs key func]
  (map (fn [site-def]
         (if (contains? site-def key)
           (update site-def key func)
           site-def))
       site-defs))

(defn- t-site-order
  "The 'order' of a trouble-site is the maximum order of all of its
  source-sites plus one."
  [source-sites]
  (->> source-sites
       (map :order)
       (reduce max)
       (inc)))

(defn- get-site-order
  "Get the order for a given site-def in the context of site-defs it may
  use as source-sites."
  [site-def site-defs]
  (case (:type site-def)
    :p-site 0 ;; A primary-site is always of order "0".
    :t-site (->> (:source-sites site-def)
                 (get-keys site-defs)
                 (t-site-order))))

(defn- set-site-orders
  "Amend the provided site-defs with the order of each site."
  [site-defs]
  (loop [input-site-defs site-defs
         output-site-defs []]
    (if-let [site-def (first input-site-defs)]
      (recur (rest input-site-defs)
             (conj output-site-defs
                   (assoc site-def :order
                          (get-site-order site-def output-site-defs))))
      output-site-defs)))

(defn- validate-site-structure
  "Takes a site-structure and validates some requirements for use in a
  distributed classifier:

  1. Trouble-sites must only reference source sites above them in the
     site-structure.

  Throws an exception in the case of an error, and returns the
  unmodified site-structure otherwise."
  [site-structure]
  (when (some (fn [[idx site]] (some #(>= % idx) (:source-sites site)))
              (map-indexed vector site-structure))
    (throw (Exception. "Trouble sites must only reference previous sites as sources (as sites are processed sequentially).")))
  site-structure)

(defn make-site-structure
  "Process a list of site-definitions to produce a site-structure that
  can be used to initialise a list of sites for a distributed
  classifier."
  [class-index & site-defs]
  (let [label-indexes (->> site-defs
                           (map-indexed
                            (fn [idx site-def]
                              {(:label site-def) idx}))
                           (into {}))]
    (-> site-defs
        ;; Convert t-site :source-sites from site labels to numeric
        ;; indexes.
        (update-site-defs :source-sites (fn [labels]
                                          (map #(get label-indexes %) labels)))
        ;; Add the class index to the :attributes list of every p-site.
        (update-site-defs :attributes #(conj % class-index))
        (set-site-orders)
        (validate-site-structure))))

;; INSPECTING SITE-STRUCTURES AND COLLECTIONS OF SITES

(defn site-attributes
  "Returns the full list of attributes/features considered by a site
  within a given sorted-map or seq of sites (can be an unitialised
  site-structure) including the class attribute.

  If site at some indexes may have been removed, then a sorted-map of
  sites must be provided."
  [site sites]
  (case (:type site)
    ;; Primary sites have a list of attributes.
    :p-site (:attributes site)
    ;; Trouble-sites have all of the attributes of their source-sites.
    :t-site (->> (:source-sites site)
                 (get-keys sites)
                 (map #(site-attributes % sites))
                 (flatten)
                 (distinct)
                 (sort))))

(defn get-site-features
  "Get the non-class features for a site in the context of a map of
  sites."
  [site sites]
  (-> site
      (site-attributes sites)
      ;; Remove class.
      (drop-last)))

(defn feature-count
  "Return the number of features processed at the given site in the
  context of a sorted-map or seq of sites (can be an unitialised
  site-structure)."
  [site sites]
  ;; Decrement by 1 to account for class attribute.
  (count (get-site-features site sites)))

(defn get-t-site-source-sites
  "Returns a list of source-site lists for the trouble sites in the
  sorted-map or seq of sites (can be an unitialised site-structure)."
  [sites]
  (->> (if (map? sites) (vals sites) sites)
       (filter #(= :t-site (:type %)))
       (map :source-sites)))

(defn get-all-source-sites
  "Return a list of all of the site-indexes that are currently used as
  source-sites by other trouble sites in the context of a sorted-map
  or seq of sites (can be an unitialised site-structure)."
  [sites]
  (->> (get-t-site-source-sites sites)
       (flatten)
       (distinct)
       (sort)))

(defn get-t-site-index-for-source-sites
  "Retrieves the index of a t-site that has the given source-sites in
  the context of a sorted-map or seq of sites (can be an unitialised
  site-structure)."
  [sites source-sites]
  (let [sources-match-pred #(= source-sites (:source-sites %))]
    (first
     (if (map? sites)
       (keys-where sources-match-pred sites)
       (indexes-where sources-match-pred sites)))))

(defn t-site-exists-for-source-sites?
  "Returns true if a t-site exists for the given source-sites in the
  context of a sorted-mpa or seq of sites (can be an unitialised
  site-structure)."
  [sites source-sites]
  (not= nil (get-t-site-index-for-source-sites sites source-sites)))

(defn get-active-sites
  "Returns a filtered copy of the provided sorted-map or seq of sites
  excluding any sites marked as :removed."
  [sites]
  (if (map? sites)
    (->> sites
         (filter #(not (:removed (second %))))
         (into (sorted-map)))
    (->> sites
         (filter #(not (:removed %))))))

(defn is-training?
  "Returns a boolean value indicating whether the site is currently in
  training mode, and should not be used for classification."
  [site]
  (not= nil (:training-countdown site)))

(defn is-stacked-t-site?
  "Returns true if the given site is a trouble-site that processes class
  values from source-sites instead of record features (i.e. it is a
  stacked classifier)"
  [site]
  (and (= (:type site) :t-site)
       (= (:input-type site) :classes)))

;; INITIALISING SITE-STRUCTURES INTO LISTS OF SITES

(defn- make-site-schema
  "Constructs the schema to be used when constructing a classifier for
  the given site within the context of a seq or sorted-map of
  sites (can be an unitialised site-structure) and the schema for the
  entire dataset.

  If site at some indexes may have been removed, then a sorted-map of
  sites must be provided. "
  [site sites schema]
  (if (is-stacked-t-site? site)
    ;; A site that will receive classes (stacking) - the schema
    ;; repeats the class once for each source site and once for the
    ;; actual class. Assumes last attribute is the class.
    (vec (repeat (inc (count (:source-sites site))) (last schema)))
    ;; A site that will receive record features/attributes
    (get-keys (vec schema) (site-attributes site sites))))

(defn- init-site
  "Initialises the classifier for a single site within the context of a
  sorted-map or seq of sites (can be an unitialised site-structure)
  and the schema for the entire dataset, and using the given
  classifier-generator to create the classifier.

  If the site contains pre-computed results, then a new classifier is
  not made, as the :computed-results can be referenced directly.

  If site at some indexes may have been removed, then a sorted-map of
  sites must be provided."
  [site sites schema classifier-generator moving-average-generator seed]
  (if (contains? site :computed-results)
    site
    (let [site-schema (make-site-schema site sites schema)]
      (assoc site
             :confidence-moving-average (moving-average-generator)
             :jitter-rng (seeded-rng seed)
             :confidence-window '()
             :agreement-window '()
             :classifier (classifier-generator site-schema)
             :site-schema site-schema))))

(defn init-sites-atom
  "Initialises all of the sites within an uninitialised site-structure
  in the context of a dataset's schema and using the given
  classifier-generator.

  The returned value is an atom so that it can act as the state within
  a distributed-classifier."
  [site-structure schema classifier-generator moving-average-generator]
  (->> site-structure
       (map vector (range (count site-structure)))
       (map #(init-site (second %)
                        site-structure schema
                        classifier-generator
                        moving-average-generator
                        (first %)))
       ;; Use a sorted-map of numeric keys so that it functions as an
       ;; ordered list, but sites can be added and removed without
       ;; affecting the indexes of other sites. TODO: Was there also
       ;; another ealier reason?
       (map-indexed vector)
       (into (sorted-map))
       ;; Return an atom.
       (atom)))

;; MANIPULATING INITIALISED SITES

(defn add-t-site!
  "Add a new trouble site to a list of sites containined in a
  `sites-atom`. The numeric indexes of the source sites are required,
  as well as other context.

  The site will be allowed to train for site-training-time number of
  records before being used for actual classification.

  Returns the label of the created site.

  TODO: The sites-atom itself could be a data-structure that retains
  some of the contextual values."
  [sites-atom source-site-indexes trouble-factor input-type schema
   classifier-generator moving-average-generator site-training-time]
  (let [all-sites @sites-atom
        sites (get-active-sites all-sites)
        site-idx-label-map (->> all-sites
                                (map (fn [[idx site]] [idx (:label site)]))
                                (into {}))
        source-site-labels (map #(get site-idx-label-map %) source-site-indexes)
        new-site-idx (inc (apply max (keys all-sites)))
        new-site-label (dynamic-t-site-label source-site-labels new-site-idx)
        order (->> (get-keys all-sites source-site-indexes)
                   (t-site-order))
        new-site-structure {:type :t-site
                            :label new-site-label
                            :source-sites source-site-indexes
                            :trouble-factor trouble-factor
                            :training-countdown site-training-time
                            :input-type input-type
                            :order order}
        new-site (init-site new-site-structure sites schema classifier-generator
                            moving-average-generator new-site-idx)]
    (debug
     (println (str "Adding t-site: " new-site-label " for sources: "
                   (vec source-site-labels))))
    ;; Add the new site.
    (swap! sites-atom #(assoc % new-site-idx new-site))
    ;; Return the new site index.
    new-site-idx))

(defn remove-t-site!
  "Removes a trouble-site at the given index in the sites-atom.

  Also removes any active trouble-sites dependent on that trouble-site.

  Returns a seq of the indexes of any removed t-sites."
  [sites-atom t-site-idx removal-reason]
  (let [all-sites @sites-atom
        dependent-site-indexes (->> all-sites
                                    (filter #(not (:removed (second %))))
                                    (filter #(seq-contains? (:source-sites (second %))
                                                            t-site-idx))
                                    (map first))
        site (get all-sites t-site-idx)]
    ;; Remove all trouble-sites that depend on the trouble-site we are
    ;; removing. Keep a vector of any removed site indexes.
    (let [removed-dependent-site-indexes
          (if (not-empty dependent-site-indexes)
            (do
              (debug
               (println "> Remove dependent t-sites of:" (:label site)))
              (->> dependent-site-indexes
                   (map #(remove-t-site! sites-atom % :dependent))
                   (flatten)))
            [])]
      ;; Ensure the site we are removing is a trouble-site, as problems
      ;; could occur if removing a primary-site.
      (when (not= :t-site (:type site))
        (throw (Exception. "Attempting to remove a non-trouble site from a sites-atom.")))
      ;; Ensure the site we are removing has not already been removed.
      (when (:removed site)
        (throw (Exception. "Attempting to remove a site that has already been removed.")))
      ;; Removed the site by marking it with :removed.
      (debug
       (println "Removing t-site:" (:label site) "for reason:" removal-reason))
      ;; Mark the site as removed.
      (swap! sites-atom #(assoc-in % [t-site-idx :removed] true))
      (swap! sites-atom #(assoc-in % [t-site-idx :removal-reason] removal-reason))
      ;; Return a sequence of all removed site labels.
      (conj removed-dependent-site-indexes t-site-idx))))
