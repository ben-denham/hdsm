(ns hdsm.classifier.distributed.distributed-test
  (:use midje.sweet)
  (:require
   [hdsm.classifier.distributed.distributed :as distributed]))

(fact "example"
      (conj [1 2] 3) => [1 2 3])
