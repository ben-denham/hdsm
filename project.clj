(defproject hdsm "0.1.0-SNAPSHOT"
  :description "Hierarchical Distributed Stream Miner: A Distributed Data Mining Approach to Classifying Heterogeneous Data Streams"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [nz.ac.waikato.cms.moa/moa "2017.06"]
                 [rhizome "0.2.9"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.apache.commons/commons-math3 "3.6.1"]]
  :min-lein-version "2.0.0"
  :plugins [[lein-clr "0.2.1"]
            [lein-codox "0.10.3"]
            [lein-midje "3.2.1"]
            [lein-cloverage "1.0.13"]]
  :codox {:metadata {:doc/format :markdown}}
  ;; JVM PROJECT CONFIGURATION
  :main ^:skip-aot hdsm.core
  :target-path "target/jvm/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[midje "1.9.1"]]}}
  ;; CLR PROJECT CONFIGURATION
  :warn-on-reflection false
  :clr {:cmd-templates  {:clj-dep [[?PATH "mono"] ["target/clr/clj/Clojure.1.9.0-alpha15/all/net40" %1]]
                         :nuget [[*PATH "nuget"] "install" %1 "-Version" %2 "-OutputDirectory" %3]
                         :fix-clojure-clr-linking ["../../../fix-clojure-clr-linking.sh" %1]}
        :deps-cmds     [[:nuget "Clojure" "1.9.0-alpha15" "../clj"]
                        [:fix-clojure-clr-linking "target/clr/clj/Clojure.1.9.0-alpha15"]]
        :main-cmd      [:clj-dep "Clojure.Main.exe"]
        :compile-cmd   [:clj-dep "Clojure.Compile.exe"]
        :target-path   "target/clr"})
