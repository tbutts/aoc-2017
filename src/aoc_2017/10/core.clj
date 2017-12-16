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

(defn knot-hash-p1
  [init-state lens]
  (->> (reduce step init-state lens)
       resolve-list
       hash-check))

(def magic-lens '(17 31 73 47 23))
(defn append-magic [xs] (concat xs magic-lens))
(def rounds 64)
(def blocks 16)

; Convert string to bytes (map bytes s)
; Append `magic-lens` to end of byte seq
; Using the lens, do the whole hash `64` times, instead of `1`
(defn sparse-hash
  [lens]
  (->> (map byte lens)
       append-magic
       cycle
       (take (* rounds (+ (count magic-lens) (count lens))))
       (reduce step base-state)
       resolve-list))

(defn dense-hash [sh]
  (->> sh
       (partition blocks)
       (map (partial reduce bit-xor))))

(def knot-hash (comp hexify dense-hash sparse-hash))

(defn part1 []
  (->> (input-for-day 10)
       no-spaces
       split-commas
       (map parse-int)
       (knot-hash-p1 base-state)))
; => 19591

(defn part2 []
  (->> (input-for-day 10)
       (.trim)
       knot-hash))
; => "62e2204d2ca4f4924f6e7a80f1288786"


(with-test
  (def example {:list (range 5) :pos 0 :skip 0})

  (are [lens expect] (= (->> lens (reduce step example) resolve-list) expect)
    [3]       '(2 1 0 3 4)
    [3 4]     '(4 3 0 1 2)
    [3 4 1]   '(4 3 0 1 2)
    [3 4 1 5] '(3 4 2 1 0))

  (is (= (knot-hash-p1 example [3 4 1 5]) 12)))

(with-test
  (def part2-examples)

  (are [in expect] (= (knot-hash in) expect)
    ""          "a2582a3a0e66e6e86e3812dcb672a272"
    "AoC 2017"  "33efeb34ea91902bb2f59c9920caa6cd"
    "1,2,3"     "3efbe78a8d82f29979031a4aa0b16a9d"
    "1,2,4"     "63960835bcdc130f0b66d7ff4f6a5a8e"))

