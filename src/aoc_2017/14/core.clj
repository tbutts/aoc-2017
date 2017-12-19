(ns aoc-2017.14.core
  (:require [clojure.test :refer [are is with-test]]
            [aoc-2017.10.core :refer [knot-hash]]
            [clojure.string :as str])
  (:use [aoc-2017.common]))

(def grid-size 128)

(defn hash-seq
  [key-str]
  (map (partial str key-str) (repeat "-") (range grid-size)))

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

(defn neighbors
  "Returns a list of all neighbors to a coordinate that are one manhattan distance away.
  E.g. (neighbors [4 0]) => ([3 0] [4 -1] [5 0] [4 1])"
  [[x y]]
  (list [(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]))

(defn invalid-loc?
  "Returns true if a coordinate has been seen before, is off the grid, or equals '\0'"
  [grid seen [x y :as coord]]
  (or (seen coord)
      (not-every? #(contains? grid %) coord)
      (= \0 (get-in grid [y x]))))

(def flood-fill
  "Finds all connected components of a vertex, using breath-first search."
  (partial bfs neighbors invalid-loc?))

(defn regions
  "Count the number of regions in a grid! Expects `grid` to be a vector."
  [grid]
  (second
   (reduce (fn [[visited n :as acc] vertex]
             (if (invalid-loc? grid visited vertex)
               acc
               [(into visited (flood-fill grid vertex)) (inc n)]))
           [#{} 0] (for [x (range grid-size) y (range grid-size)] [x y]))))


(defn print-grid
  "Print the entire disk grid, as dots and hashes, just like the example!"
  [grid]
  (doall
   (->> grid
        (map (fn pretty [row] (str/join (map #(if (= % \0) \. \#) row))))
        (map println))))

(def input "ljoxqyyw")
(defn part1 [] (-> input generate-grid used-squares)) ;; => 8316
(defn part2 [] (-> input generate-grid vec regions))  ;; => 1074

(with-test
  (def example "flqrgnkx")

  (is (= (-> example generate-grid used-squares) 8108))
  (is (= (-> example generate-grid vec regions) 1242)))
