(ns aoc-2017.05.core
  (:require [aoc-2017.common :as aoc]
            [clojure.test :refer [are is with-test]]
            [clojure.string :as str]))

(defn follow-trampolines
  [instructions]
  (loop [xs instructions
         idx 0
         steps 0]
    (let [next-jump (nth xs idx nil)]
      (if (nil? next-jump)
        steps
        (recur (assoc xs idx (inc next-jump)) (+ idx next-jump) (inc steps))))))

(with-test
  (def example [0 3 0 1 -3])

  (is (= (follow-trampolines example) 5)))

(def day "05")
(defn parse-input [day]
  (->> (aoc/input-filename day) slurp str/split-lines (mapv #(Integer/parseInt %))))
(def input (parse-input day))

(defn part1 []
  (follow-trampolines input))
; => 387096


