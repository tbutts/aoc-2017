(ns aoc-2017.01.core
  (:require [clojure.test :refer [are is with-test]]))

(defn circular-digit-array
  [s]
  (map #(Character/digit % 10) (conj (vec s) (first s))))

(defn solve
  [input]
  (loop [acc 0
         digits (circular-digit-array input)]
    (if (> (count digits) 1)
      (let [[a b] digits]
        (recur (+ acc (if (= a b) a 0))
               (drop 1 digits)))
      acc)))

(with-test
  (def examples)

  (are [input output] (= (solve input) output)
    "1122" 3
    "1111" 4
    "1234" 0
    "91212129" 9))


(def chpt "01")
(def filename "src/aoc-2017/01/input")
(defn get-solution [] (solve (slurp filename)))

(defn -main
  [& args]
  (println "Chapter" chpt "answer:" (get-solution)))

