(ns aoc-2017.10.core
  (:require [clojure.test :refer [are is with-test]])
  (:use [aoc-2017.common]))

(def base-state
  {:list (range 256)
   :pos  0
   :skip 0})

(defn head-to-tail [n coll]
  (let [[head tail] (split-at n coll)]
    (concat tail head)))

(defn step
  "One iteration of the 1st part of the knot-hash function.
  List is projected on ':pos', such that the head of the updated list will be
  :pos, not the 0th index. To get back the 'correct view' of the list,
  use `resolve-list`"
  [{:keys [list pos skip] :as state} len]
  (let [[head tail] (split-at len list)
        new-list    (concat (reverse head) tail)
        proj-pos    (mod (+ len skip) (count list))
        new-pos     (mod (+ pos proj-pos) (count list))]
    {:list (head-to-tail proj-pos new-list)
     :pos new-pos
     :skip (inc skip)}))

(defn resolve-list
  [{:keys [list pos]}]
  (head-to-tail (- (count list) pos) list))

(defn hash-check [coll]
  (apply * (take 2 coll)))

(defn knot-hash
  [init-state lens]
  (->> (reduce step init-state lens)
       resolve-list
       hash-check))


(defn part1 []
  (->> (input-for-day 10)
       no-spaces
       split-commas
       (map parse-int)
       (knot-hash base-state)))
; => 19591

(with-test
  (def example {:list (range 5) :pos 0 :skip 0})

  (are [lens expect] (= (->> lens (reduce step example) resolve-list) expect)
    [3]       '(2 1 0 3 4)
    [3 4]     '(4 3 0 1 2)
    [3 4 1]   '(4 3 0 1 2)
    [3 4 1 5] '(3 4 2 1 0))

  (is (= (knot-hash example [3 4 1 5]) 12)))

