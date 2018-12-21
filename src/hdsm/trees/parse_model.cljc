(ns hdsm.trees.parse-model
  (:require [clojure.string :as string]))

(def indent-step 2)

(defn- indent-string [indent]
  (apply str (repeat indent " ")))

(defn- subtree [tree indent]
  (take-while #(string/starts-with? % (indent-string indent)) tree))

(defn- if-regex [indent]
  (re-pattern (str (indent-string indent)
                   "if \\[att [0-9]+:(.+)\\] = \\{val [0-9]+:(.+)\\}:.*")))

(defn- leaf-regex [indent]
  (re-pattern (str (indent-string indent)
                   "Leaf \\[class:(.+)\\] = <class [0-9]+:(.+)> weights: \\{([0-9,\\|]+)\\}.*")))

(defn- parse-leaf-node [line indent]
  (let [[_ label class weights-string] (re-matches (leaf-regex indent) line)
        weights (->> (string/split weights-string #"\|")
                     (map #(string/replace % #"," ""))
                     (map read-string))]
    {:type :leaf
     :class-label label
     :class class
     :weights weights
     :instances (reduce + weights)
     :class-instances (apply max weights)}))

(declare lines->node)
(defn- parse-split-node [lines indent]
  (let [ifs (->> lines
                 (map-indexed
                  (fn [idx line]
                    (when-let [[_ feature val] (re-matches (if-regex indent) line)]
                      {:idx idx :feature feature :val val})))
                 (filter (comp not empty?)))
        branches (map (fn [if-start if-end]
                        (let [start (+ 1 if-start)
                              length (- if-end start)]
                          (->> lines
                               (drop start)
                               (take length))))
                      (map :idx ifs) (conj (vec (map :idx (rest ifs)))
                                           (count lines)))]
    (when (> (count (set (map :att ifs))) 1)
      (println "WARNING: Multi-attribute split at a single node."))
    {:type :branch
     :feature (:feature (first ifs))
     :branches (into {} (map #(vector (:val %1)
                                      (lines->node %2 (+ indent indent-step)))
                             ifs branches))}))

(defn- lines->node [lines indent]
  (if (= 1 (count lines))
    (parse-leaf-node (first lines) indent)
    (parse-split-node lines indent)))

(defn parse-moa-tree-model-string
  "Given a model string description from a MOA tree (e.g. Hoeffding
  tree), parse it to build a representation with Clojure data
  structures."
  [model-string]
  (-> model-string
      (string/split #"\n")
      (lines->node indent-step)))
