(ns aoc-2017.05.core
  (:require [aoc-2017.common :as aoc]
            [clojure.test :refer [are is with-test]]
            [clojure.string :as str]))

(defn follow-trampolines
  [post-jump-fn instructions]
  (loop [xs instructions
         idx 0
         steps 0]
    (let [next-jump (nth xs idx nil)]
      (if (nil? next-jump)
        steps
        (recur (assoc xs idx (post-jump-fn next-jump)) (+ idx next-jump) (inc steps))))))

; If offset was 3 or greater, decrement by 1, otherwise increment by 1
(defn p2-jump-fn [offset] ((if (>= offset 3) dec inc) offset))

(with-test
  (def example [0 3 0 1 -3])

  ; Part 1
  (is (= (follow-trampolines inc example) 5))
  ; Part 2
  (is (= (follow-trampolines p2-jump-fn example) 10)))

(def day "05")
(defn parse-input [day]
  (->> (aoc/input-filename day) slurp str/split-lines (mapv #(Integer/parseInt %))))
(def input (parse-input day))

(defn part1 []
  (follow-trampolines inc input))
; => 387096

(defn part2 []
  (follow-trampolines p2-jump-fn input))
; => 28040648
;; Takes over 10 seconds to compute

