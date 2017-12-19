(ns aoc-2017.14.core
  (:require [clojure.test :refer [are is with-test]]
            [aoc-2017.10.core :refer [knot-hash]]
            [clojure.string :as str])
  (:use [aoc-2017.common]))

(defn hash-seq
  [key-str]
  (take 128
        (map (partial str key-str) (repeat "-") (range))))

(defn generate-grid
  [key-str]
  (pmap #(hex->binstr (knot-hash %)) (hash-seq key-str)))

(defn used-squares
  [grid]
  (reduce
   (fn [sum row] (+ sum (count (re-seq #"1" row))))
   0 grid))

(defn used-squares2
  [grid]
  (get (apply merge-with + (map frequencies grid)) \1))


(defn print-grid
  "Print the entire disk grid, as dots and hashes, just like the example!"
  [grid]
  (doall
   (->> grid
        (map (fn pretty [row] (str/join (map #(if (= % \0) \. \#) row))))
        (map println))))


(defn part1 []
  (-> "ljoxqyyw" generate-grid used-squares))
;; => 8316

(with-test
  (def example "flqrgnkx")

  (is (= (-> example generate-grid used-squares) 8108)))
