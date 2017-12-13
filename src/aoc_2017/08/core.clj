(ns aoc-2017.08.core
  (:require [clojure.test :refer [are is with-test]]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:use [aoc-2017.common]))

(def != not=)
(def comparer (comp eval read-string))

(defn process
  [regs instruction]
  (let [[target inc-dec amt _ reg cmp val] (str/split instruction #" ")
        registers (update regs target #(or %1 0))] ; Default target reg to 0
    (if ((comparer cmp) (get registers reg 0) (Integer. val))
      (update registers target + ((if (= inc-dec "inc") + -) (Integer. amt)))
      registers)))

(def run-prog (partial reduce process {}))
(def largest-reg (comp #(apply max %) vals))

;; Part 1
(def parse-and-run-p1 (comp largest-reg run-prog str/split-lines))

(defn part1 []
  (->> "08/input" io/resource slurp largest-reg-after-run))
; => 7787

;; This part is also doable with lazy file reading:
(defn part1-lazy []
  (with-open [rdr (-> "08/input" io/resource io/reader)]
    (largest-reg (run-prog (line-seq rdr)))))

(with-test
  (def ex1
"b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

  (is (= (parse-and-run-p1 ex1) 1)))

