(ns aoc-2017.11.core
  (:require [clojure.test :refer [are is with-test]]
            [clojure.java.io :as io])
  (:import (java.io Reader StringReader))
  (:use [aoc-2017.common]))

(defrecord Hex [q r])

(def origin (->Hex 0 0))
(def axial-directions
  (map-kv (partial apply ->Hex)
   {:n  [0 -1]
    :nw [-1 0]
    :sw [-1 1]
    :s  [0 1]
    :se [1 0]
    :ne [1 -1]}))

(defn neighbor
  [hex dir]
  (merge-with + hex ((keyword dir) axial-directions)))

(defn follow-directions [^Reader res]
  (with-open [rdr (clojure.java.io/reader res)]
    (reduce neighbor origin (map no-spaces (comma-seq rdr)))))

(defn distance
  ([hex] (distance origin hex))
  ([{aq :q ar :r} {bq :q br :r}]
   (/ (+ (abs (- aq bq))
         (abs (+ aq ar (- bq) (- br)))
         (abs (- ar br))) 2)))


(defn part1 []
  (-> (io/resource "11/input") follow-directions distance))
; => 705

(with-test
  (def examples)

  (are [dirs steps] (= (-> (StringReader. dirs) follow-directions distance) steps)
    "ne,ne,ne"       3
    "ne,ne,sw,sw"    0
    "ne,ne,s,s"      2
    "se,sw,se,sw,sw" 3))
