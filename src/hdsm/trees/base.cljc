(ns hdsm.trees.base
  (:require [hdsm.trees.parse-model :refer [parse-moa-tree-model-string]]))

;; Functions for generating trees.

(defn make-tree
  "Constructs a decision tree."
  [features cardinalities root]
  {:features features
   :cardinalities cardinalities
   :root root})

(defn tree-node
  "Constructs a branch or leaf node for a decision tree, where the
  decision will follow the branch at the index determined by the
  feature."
  [feature & branches]
  {:feature feature
   :branches (into {} (map-indexed #(vector %1 %2) branches))})

(def tn tree-node)

;; Functions for applying a tree to classify an instance (x).

(defn- apply-node
  "Recursively traverses tree nodes to classify the instance defined
  by the feature-map."
  [node feature-map]
  (if-not (map? node)
    node
    (let [branching-value (feature-map (:feature node))]
      (apply-node ((:branches node) branching-value) feature-map))))

(defn- feature-list-to-map
  "Converts a list of input feature values (x) to a map with keys from
  features."
  [features x]
  (->> (map vector features x)
       (into {})))

(defn apply-tree
  "Applies a decision tree to classify a given instance (x)."
  [tree x]
  (apply-node (:root tree)
              (feature-list-to-map (:features tree) x)))

;; Functions for enumerating paths to leaf nodes in a given tree.

(def wildcard '_)

(defn- node-branch-paths
  "Recursively traverses tree nodes to build up a sequence of maps
  representing paths to unique leaves."
  [node partial-partitions]
  (if-not (map? node)
    [partial-partitions]
    (reduce concat (map (fn [[k v]]
                          (node-branch-paths v (assoc partial-partitions
                                                      (:feature node) k)))
                        (:branches node)))))

(defn tree-branch-paths
  "Returns a sequence of maps that each represent a set of
  feature/value pairs to reach a unique leaf node within the given
  tree. An indeterminate value (wildcard) is assigned to features that
  are not considered when navigating to a particular leaf."
  [tree]
  (->> (node-branch-paths (:root tree) {})
       (map (fn [branch-path]
              (map #(get branch-path % wildcard) (:features tree))))))

;; Utility Functions

(defn input-domain
  "Returns all permutations of inputs to a tree with the given feature
  cardinalities."
  [cardinalities]
  (if (empty? cardinalities)
    [[]]
    (let [head (first cardinalities)
          tail (rest cardinalities)
          subdomain (input-domain tail)]
      (reduce concat
              (map (fn [value] (map #(concat [value] %) subdomain))
                   (range head))))))

;; Build tree from hoeffding tree model

(defn- build-feature-index-lookup [schema]
  (->> schema
       (map
        (fn [{:keys [name options]}]
          [name (->> options
                     (map-indexed #(vector %2 %1))
                     (into {}))]))
       (into {})))

(defn- transform-parsed-tree-model [node index-lookup]
  (case (:type node)
    :leaf
    ;; Convert a leaf to the index of its selected class value.
    (get-in index-lookup [(:class-label node) (:class node)])
    :branch
    (-> node
        (dissoc :type)
        (update :branches
                (fn [branches]
                  (->> branches
                       (map
                        (fn [[feature-val branch]]
                          ;; Each branch should map the feature
                          ;; value index to a subtree
                          [(get-in index-lookup [(:feature node) feature-val])
                           (transform-parsed-tree-model branch index-lookup)]))
                       (into {})))))))

(defn moa-tree-model-string->tree [model-string schema]
  (let [feature-schema (drop-last schema)] ;; Drop the class
    (make-tree (map :name feature-schema)
               (map (comp count :options) feature-schema)
               (-> model-string
                   (parse-moa-tree-model-string)
                   (transform-parsed-tree-model (build-feature-index-lookup schema))))))
