(ns aoc-2017.common)

(defn input-filename [day] (str "src/aoc-2017/" day "/input"))

(defn print-ans
  [day part solver]
  (println (str day "." part) "answer:" (solver)))

(defn run
  [day solvers]
  (dorun (map-indexed
          #(print-ans day (inc %1) %2) solvers)))
