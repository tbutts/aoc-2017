(ns aoc-2017.common
  (:require [clojure.java.io :as io]))

(defn input-for-day [day] (-> (str day "/input") io/resource slurp))

(defn input-filename [day] (str "src/aoc-2017/" day "/input"))

(defn print-ans
  [day part solver]
  (println (str day "." part) "answer:" (solver)))

(defn run
  [day solvers]
  (dorun (map-indexed
          #(print-ans day (inc %1) %2) solvers)))
