(defproject aoc_2017 "1.0.0-SNAPSHOT"
  :description "Yuletide programming exercises 🎄 - https://adventofcode.com/"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.3.443"]]
  :main nil
  :bootclasspath true
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:resource-paths ["resource"]}})
