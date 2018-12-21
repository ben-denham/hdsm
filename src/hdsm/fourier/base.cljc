(ns hdsm.fourier.base
  (:require
   [hdsm.utils
    :refer [replace-in-seq debug format-number]]
   [hdsm.utils.complex-numbers
    :refer [complex *c +c -c c-conjugate c-zero? format-complex-number]]
   [hdsm.trees.base
    :refer [apply-tree tree-branch-paths input-domain wildcard]]))

;; Functions used in the construction of Fourier spectra.

(defn- branch-path-weight
  "Returns the weight for a given tree branching path based on the
  proportion of how many instances in the complete domain of possible
  inputs will reach that leaf. i.e. The more undetermined wildcard
  values, the more possible inputs would reach that leaf."
  [branch-path cardinalities]
  (let [total-cardinality (reduce * cardinalities)
        path-cardinality (reduce * (map #(if (= %1 wildcard) %2 1)
                                        branch-path cardinalities))]
    (/ path-cardinality total-cardinality)))

(defn- tree-weighted-x-domain
  "Returns a minimal subset of x values to iterate over when
  determining a coefficient. Each represents a path to a different
  leaf node in the tree, and is weighted according to the."
  [tree]
  (let [l (count (:features tree))]
    (->> (tree-branch-paths tree)
         (map #(vector % (branch-path-weight % (:cardinalities tree)))))))

(defn- build-fourier-basis-function
  "Returns a basis function for a given partition. In the context of a
  Fourier transform on a waveform, the basis functions are the
  sinusoids (sines or cosines) of different frequencies that are
  assigned coefficients depending on how much energy resides at that
  frequency.

  Note that when x[i] only affects the result of the basis function
  when partition[i] is non-zero."
  [partition cardinalities]
  (fn [x]
    ;; Importantly, if a particular feature in the input (x) is
    ;; indeterminate (wildcard), and the feature is non-zero in the
    ;; partition, then the output of the basis function is zero.
    (if (some (fn [[x-val p-val]] (and (not= p-val 0) (= x-val wildcard)))
                  (map vector x partition))
      (complex 0 0)
      ;; Raising -1 to a power will result in either -1 or 1 if the
      ;; power is an integer (which it always will be), but is a
      ;; complex number otherwise (or purely imaginary at
      ;; *.5). Because of this, it would also be possible to
      ;; apply a (modulo 2) operation to the exponent. wildcard
      ;; is replaced with 0 in order for the dot-product to work
      ;; correctly, but has no impact as the if-statement above
      ;; ensures it will only ever be multiplied by zero.
      (apply *c (map (fn [xi ji ci]
                       (let [exponent (* (/ (* 2 Math/PI) ci) xi ji)]
                         (complex (Math/cos exponent) (- (Math/sin exponent)))))
                     (replace-in-seq wildcard 0 x)
                     partition
                     cardinalities)))))

(defn binary-seq-to-number [xs]
  (->> (reverse xs)
       (map-indexed (fn [idx x] (* x (Math/pow 2 idx))))
       (reduce +)))

(defn- fourier-coefficient
  "Calculate the Fourier coefficient for a particular partition in a
  tree. This is performed by summing the products of the basis
  function and application results for all inputs (x) in the minimal
  domain (with input weighting to account for the minimal domain)."
  [tree partition]
  (debug
   (println "-- Partition:" partition))
  (let [l (count partition)
        basis (build-fourier-basis-function partition (:cardinalities tree))
        coefficient
        (->> (tree-weighted-x-domain tree)
             (map (fn [[x weight]]
                    (let [class (apply-tree tree x)
                          basis-val (basis x)
                          coefficient-component (*c (complex class 0)
                                                    basis-val
                                                    (complex weight 0))]
                      (debug
                       (println class "|"
                                x "->" basis-val "|"
                                weight "=>" coefficient-component))
                      coefficient-component)))
             (reduce +c))]
    (debug
      (println "---- Sum => " coefficient))
    coefficient))

;; Functions for generating and applying fourier spectra.

(defn fourier-spectrum
  "Returns a Fourier spectrum represented as a map of the partitions (j
  values) to their associated coefficients."
  [tree]
  (let [partitions (input-domain (:cardinalities tree))]
    (->> partitions
         (map #(fourier-coefficient tree %))
         (map #(vector (into [] %1) %2) partitions)
         (into {}))))

(defn apply-fourier-spectrum
  "Applies a spectrum to classify a given instance (x)"
  [spectrum x cardinalities]
  (->> spectrum
       (filter (comp not c-zero? second))
       (map (fn [[partition coefficient]]
              (let [basis (build-fourier-basis-function partition
                                                        cardinalities)]
                (*c coefficient (c-conjugate (basis x))))))
       (reduce +c)))

(defn test-tree
  "Compares results for the given inputs to the given tree from by
  applying both the tree itself and a fourier spectrum derived from
  the tree."
  [tree inputs]
  (let [spectrum (fourier-spectrum tree)
        tree-results (map #(apply-tree tree %) inputs)
        spectrum-results (map #(apply-fourier-spectrum
                                spectrum % (:cardinalities tree)) inputs)]
    (map #(hash-map :x (into [] %1) :same (= %2 %3)
                    :tree-result %2 :spectrum-result %3)
         inputs
         (map format-number tree-results)
         (map format-complex-number spectrum-results))))

;; Utility functions for fourier concepts.

(defn partition-order
  "The 'order' of a partition is the number of non-zero values it
  contains"
  [partition]
  (reduce + partition))

(defn filter-spectrum-by-order
  "Filters any coefficients with a partition-order greater than
  max-order out of the spectrum. As high-order partitions have
  low-energy coefficients, this will not significantly affect the
  output of the spectrum if max-order is sufficiently high."
  [spectrum max-order]
  (->> spectrum
       (filter #(<= (partition-order (second %)) max-order))
       (into {})))

(defn fourier-coefficient-energy
  "The energy of a Fourier coefficient is determined. Energy
  exponentially decays as the order of the partition the coefficient
  is for increases."
  [coefficient]
  (Math/pow coefficient 2))

(defn diff-spectra
  "Returns the differences in the coefficients between the two spectra"
  [s1 s2]
  (when (not= (sort (keys s1)) (sort (keys s2)))
    (throw (Exception. "Cannot diff spectra with different partitions.")))
  ;; For each partition, subtract the coefficient of s2 from the
  ;; coefficient of s1.
  (merge-with -c s1 s2))
