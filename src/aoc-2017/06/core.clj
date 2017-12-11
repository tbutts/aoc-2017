(ns aoc-2017.06.core
  (:require [aoc-2017.common :as aoc]
            [clojure.test :refer [are is with-test]]
            [clojure.string :as str]))

(defn max-val
  "max value of a [idx val] pairs, tie goes to first"
  [x y]
  (if (>= (second x) (second y)) x y))

(defn max-bank
  "[idx, val] of largest seq value"
  [banks]
  (reduce max-val (map-indexed vector banks)))

(defn next-idx
  "increment cur index, looping back to 0 if at the seq length, len"
  [cur len]
  (mod (inc cur) len))

(defn redistribute
  "increment the next bank until all blocks have been reallocated"
  [banks idx iter times]
  (if (>= iter times)
    banks
    (redistribute (assoc banks idx (inc (nth banks idx)))
                  (next-idx idx (count banks))
                  (inc iter)
                  times)))

(defn realloc-banks
  "Runs the redistribute fn until a repeat distribution of banks is
  found. Returns the number of redistribution steps until a cycle was
  found."
  [init]
  (loop [seen {init 0}
         banks init]
    (let [[idx rtimes] (max-bank banks)
          next-banks (redistribute (assoc banks idx 0)
                                   (next-idx idx (count banks))
                                   0 rtimes)
          step (count seen)]
      (if-let [cycle-start (get seen next-banks)]
        {:total step :loop-size (- step cycle-start)}
        (recur (assoc seen next-banks step) next-banks)))))

(with-test
  (def scenario1 [0 2 7 0])

  (is (= (realloc-banks scenario1) 5)))

(def day "06")
(def input
  (->> (aoc/input-filename day) slurp (#(str/split % #"\s+")) (mapv #(Integer/parseInt %))))
(defn part1 [] (-> (realloc-banks input) :total))
; => 4074

(defn part2 [] (-> (realloc-banks input) :loop-size))

