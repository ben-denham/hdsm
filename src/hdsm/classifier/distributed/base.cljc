(ns hdsm.classifier.distributed.base)

(defprotocol DistributedClassifier
  (get-sites [classifier]
    "Returns the list of active sites in this distributed classifier.")
  (get-all-sites [classifier]
    "Returns the list of ALL sites in this distributed
    classifier (including removed sites).")
  (get-site-changelog [classifier]
    "Returns a seq of addition/removal of site events in the lifetime
    of this distributed classifier."))
