(ns aoc-2017.17.core
  (:require [clojure.test :refer [are is with-test]])
  (:use [aoc-2017.common]))

(defn insert [v pos item]
  (apply conj (subvec v 0 pos) item (subvec v pos)))

(defn spinlock
  "Returns a lazy sequence of [position value-buffer] items, from iterations
  of the spinlock algorithm. Each step of the spinlock, the `pos`ition pointer
  moves forward `step` number of times (looping if necessary). Then, the next
  `val` is inserted after the stopping position into `buf`."
  ([step] (spinlock step [0] 0 1))
  ([step buf pos val]
   (let [p (inc (mod (+ pos step) (count buf)))
         b (insert buf p val)]
     (cons [p b] (lazy-seq (spinlock step b p (inc val)))))))

(defn festive-spinlock [step]
  (let [[pos2017 buf] (->> (spinlock step) (drop 2016) first)]
    (buf (inc pos2017))))


(def input 377)
(defn part1 [] (festive-spinlock 377)) ;; => 596

(with-test
  (def example)

  (is (= (festive-spinlock 3) 638)))

