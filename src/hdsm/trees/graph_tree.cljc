(ns hdsm.trees.graph-tree
  (:require [clojure.string :as string]
            [rhizome.dot :refer [graph->dot]]))

(defn build-id-generator []
  (let [id-counter (atom 0)]
    (fn []
      (swap! id-counter inc)
      @id-counter)))

(def indent-step 2)

(defn indent-string [indent]
  (apply str (repeat indent " ")))

(defn subtree [tree indent]
  (take-while #(string/starts-with? % (indent-string indent)) tree))

(defn if-regex [indent]
  (re-pattern (str (indent-string indent)
                   "if \\[att [0-9]+:(.+)\\] = \\{val [0-9]+:(.+)\\}:.*")))

(defn leaf-regex [indent]
  (re-pattern (str (indent-string indent)
                   "Leaf \\[class:(.+)\\] = <class [0-9]+:(.+)> weights: \\{([0-9,\\|]+)\\}.*")))

(defn lines->node [lines indent id-generator cluster]
  (if (= 1 (count lines))
    (let [[_ label class weights-string] (re-matches (leaf-regex indent) (first lines))
          weights (->> (string/split weights-string #"\|")
                       (map #(string/replace % #"," ""))
                       (map read-string))
          class-weight (apply max weights)
          confidence (str class-weight "/" (reduce + weights))]
      {:type :leaf
       :id (id-generator)
       :cluster cluster
       :class-label label
       :class class
       :confidence confidence})
    (let [ifs (->> lines
                   (map-indexed
                    (fn [idx line]
                      (when-let [[_ att val] (re-matches (if-regex indent) line)]
                        {:idx idx :att att :val val})))
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
       :id (id-generator)
       :cluster cluster
       :att (:att (first ifs))
       :branches (into {} (map #(vector (:val %1)
                                        (lines->node %2 (+ indent indent-step)
                                                     id-generator cluster))
                               ifs branches))})))

(defn tree->nodes-list [tree]
  (concat [tree] (flatten (map tree->nodes-list (vals (:branches tree))))))

(defn node-label [node]
  (case (:type node)
    :branch (:att node)
    :leaf (str (:class node) " (" (:confidence node) ")")
    :site (:label node)))

(defn edge-label [parent child]
  (let [branch-val (some (fn [[value node]] (when (= node child) value))
                         (into [] (:branches parent)))]
    (str branch-val)))

(defn moa-tree->graph-nodes [moa-tree-string id-generator cluster]
  (if (string? moa-tree-string)
    (-> moa-tree-string
        (string/split #"\n")
        (lines->node indent-step id-generator cluster)
        (tree->nodes-list))
    []))

(defn site-models->graph-nodes [models id-generator]
  (->> models
       (into [])
       (map (fn [[label model]]
              (moa-tree->graph-nodes model id-generator label)))
       (flatten)))

(defn graph-nodes->dot [nodes]
  (graph->dot nodes #(vals (:branches %))
              :node->descriptor (fn [n] {:label (node-label n)})
              :edge->descriptor (fn [parent child]
                                  {:label (edge-label parent child)})
              :node->cluster :cluster
              :cluster->descriptor (fn [c] {:label c})
              :options {:dpi 72}))

(defn model->dot [model]
  (let [id-generator (build-id-generator)
        nodes (cond
                (map? model) (site-models->graph-nodes model id-generator)
                :else (moa-tree->graph-nodes model id-generator nil))]
    (graph-nodes->dot nodes)))

(defn save-graph-dot [model filename]
  (spit (str "models/" (string/replace filename #":" "") ".dot")
        (model->dot model)))
