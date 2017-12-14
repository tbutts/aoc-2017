(ns aoc-2017.08.core
  (:require [clojure.test :refer [are is with-test]]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:use [aoc-2017.common]))

(def != not=)
(def comparer (comp eval read-string))

(defn process
  [regs instruction]
  (let [[target op val _ cmp-reg cmp-test cmp-val] (str/split instruction #" ")]
    (if ((comparer cmp-test) (get regs cmp-reg 0) (Integer. cmp-val))
      (update regs target
              (fnil + 0)
              ((if (= op "inc") + -) (Integer. val)))
      regs)))

(def run-prog (partial reduce process {}))
(def largest-reg (comp #(apply max %) vals))

(defn run-prog-keep-largest [insns]
  (->> insns
       (reductions process {})
       (mapcat vals)
       (apply max)))

;; Parts 1 & 2
(def parse-and-run-p1 (comp largest-reg run-prog str/split-lines))
(def parse-and-run-p2 (comp run-prog-keep-largest str/split-lines))

(defn part1 []
  (->> "08/input" io/resource slurp parse-and-run-p1))
; => 7787

;; This part is also doable with lazy file reading:
(defn part1-lazy []
  (with-open [rdr (-> "08/input" io/resource io/reader)]
    (largest-reg (run-prog (line-seq rdr)))))

(defn part2 []
  (->> "08/input" io/resource slurp parse-and-run-p2))
; => 8997

(with-test
  (def ex
"b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

  (is (= (parse-and-run-p1 ex) 1))
  (is (= (parse-and-run-p2 ex) 10)))

