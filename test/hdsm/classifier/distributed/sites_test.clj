(ns hdsm.classifier.distributed.sites-test
  (:use midje.sweet)
  (:require
   [hdsm.classifier.distributed.sites :as sites]
   [hdsm.classifier.distributed.distributed
    :refer [default-moving-average-generator]]
   [hdsm.classifier.random
    :refer [random-classifier]]
   [hdsm.utils
    :refer [*debug*]]))

(defn- dummy [& args] nil)

(defn- make-demo-schema
  [feature-count]
  (-> (for [i (range feature-count)]
        {:name (str "f" i) :options ["0" "1"]})
      (concat [{:name "class" :options ["0" "1"]}])
      (vec)))

(defn- get-sites-by-labels
  [sites labels]
  (filter #(contains? (set labels) (:label %)) (vals sites)))

(defn- orders-of-sites
  [sites]
  (->> sites
       (map :order)
       (distinct)
       (sort)
       (vec)))

;; This doesn't work reliably, as there is no way to force a map into
;; an incorrect order.
(let [feature-count 50
      schema (make-demo-schema feature-count)
      p-sites (for [i (range feature-count)]
                (sites/p-site (keyword (str "p-" i)) [i]))
      site-structure (apply
                      sites/make-site-structure
                      feature-count
                      (concat p-sites
                              [(sites/t-site :t (map :label p-sites) 1)]))
      sites-atom (sites/init-sites-atom site-structure
                                        schema
                                        random-classifier
                                        (default-moving-average-generator 1000))]
  (fact "Site indexes should be in order."
        (map first @sites-atom)) => (range (inc feature-count))
  (fact "Site order should be retained during sites initialization."
        (map (comp :label second) @sites-atom)) => (map :label site-structure))

(facts "about site-order"
       (let [feature-count 5
             schema (make-demo-schema feature-count)
             p-sites (for [i (range feature-count)]
                       (sites/p-site (keyword (str "p-" i)) [i]))
             site-structure (apply
                             sites/make-site-structure
                             feature-count
                             (concat p-sites
                                     [(sites/t-site :t-1 [:p-0 :p-1] 1)
                                      (sites/t-site :t-2 [:p-2 :p-3] 1)
                                      (sites/t-site :t-3 [:t-1 :p-4] 1)
                                      (sites/t-site :t-4 [:t-1 :t-2] 1)]))
             sites-atom (sites/init-sites-atom site-structure
                                               schema
                                               random-classifier
                                               (default-moving-average-generator 1000))]
         (fact "Primary-sites should have order of 0"
               (->> (get-sites-by-labels @sites-atom [:p-0 :p-1 :p-2 :p-3 :p-4])
                    (orders-of-sites))=> [0])
         (fact "T-sites with only p-site inputs should have order 1"
               (->> (get-sites-by-labels @sites-atom [:t-1 :t-2])
                    (orders-of-sites)) => [1])
         (fact "T-sites with t-site inputs should have order calculated accordingly"
               (->> (get-sites-by-labels @sites-atom [:t-3 :t-4])
                    (orders-of-sites)) => [2])
         (fact "A dynamically added t-site should have the correct order"
               (let [source-site-indexes
                     (->> @sites-atom
                          (filter #(contains? #{:p-0 :t-3} (:label (second %))))
                          (map first))]
                 (binding [*debug* false]
                   (sites/add-t-site! sites-atom source-site-indexes 1 :features
                                      schema dummy dummy 5))
                 (->> (get-sites-by-labels @sites-atom [:t-<0-3_i9>])
                      (orders-of-sites)) => [3]))))
