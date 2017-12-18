(ns aoc-2017.12.core
  (:require [clojure.test :refer [are is with-test]]
            [clojure.string :as str])
  (:use [aoc-2017.common]))

(defn parse
  [input]
  (->> input
       str/split-lines
       (map #(str/replace % #"(<-> |,)" ""))
       (map (fn [s] (->> (split-spaces s) (map parse-int))))
       (reduce (fn [m [id & others]] (assoc m id others)) {})))

(defn connections
  "Find connected components of a vertex, using bfs"
  ([graph root-vertex] (connections graph [root-vertex] #{}))
  ([graph nodes visited]
   (if (empty? nodes)
     visited
     (recur graph
            (remove visited (mapcat graph nodes))
            (into visited nodes)))))


(defn part1 []
  (-> (input-for-day "12") parse (connections 0) count))
;; => 130

(with-test
  (def example
"0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5")

  (is (= (-> (parse example) (connections 0) count) 6)))

