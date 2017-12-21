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
  ([step] (spinlock step [0] 0))
  ([step buf pos]
   (let [buf-len (count buf)
         p (inc (mod (+ pos step) buf-len))
         b (insert buf p buf-len)]
     (cons [p b] (lazy-seq (spinlock step b p))))))

(defn festive-spinlock [step]
  (let [[pos2017 buf] (->> (spinlock step) (drop 2016) first)]
    (buf (inc pos2017))))

(defn short-spinlock
  "For part 2, the spinlock algo will never touch where '0' is (first position),
  so the solution only needs to track what's in index 1 - `tracked` -
  for 5m runs (see [0]).  The buffer size is always equal to the
  iteration count, `iter`."
  ([step] (short-spinlock step 0 0 1))
  ([step tracked pos iter]
   (let [p (inc (mod (+ pos step) iter))
         one (if (= p 1) iter tracked)]
     (cons one (lazy-seq (short-spinlock step one p (inc iter)))))))

(def input 377)
(defn part1 [] (festive-spinlock input)) ;; => 596
(defn part2 [] (->> (short-spinlock input) (drop (dec 5e7)) first)) ;; => 39051595

(with-test
  (def example)

  (is (= (festive-spinlock 3) 638))

  ;; [0]
  (is (every? zero?
              (map (fn [[_ v]] (.indexOf v 0))
                   (take 1000 (spinlock input))))))

