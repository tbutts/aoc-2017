(ns aoc-2017.01.core
  (:require [clojure.test :refer [are is with-test]]
            [aoc-2017.common :as aoc]))

(defn half [n] (/ n 2))

(defn digit-array
  [s]
  (map #(Character/digit % 10) (seq s)))

(defn find-matches
  [input digit-selector]
  (let [digits (digit-array input)
        len (count digits)]
    (apply + (map (fn select-when-matching [a pos]
                    (let [b (nth digits (mod (+ pos (digit-selector len)) len))]
                      (if (= a b) a 0)))
                  digits (range)))))

(defn solver [f] (fn solve-x [input] (find-matches input f)))
(def solve-01 (solver (constantly 1)))
(def solve-02 (solver #(half %)))

(with-test
  (def examples)

  ; Part 1 Tests
  (are [input output] (= (solve-01 input) output)
    "1122" 3
    "1111" 4
    "1234" 0
    "91212129" 9)

  ; Part 2 Tests
  (are [in out] (= (solve-02 in) out)
    "1212" 6
    "1221" 0
    "123425" 4
    "123123" 12
    "12131415" 4))


(def day "01")
(def challenge-input (slurp (aoc/input-filename day)))

(def get-solution-01 #(solve-01 challenge-input))
(def get-solution-02 #(solve-02 challenge-input))

(defn -main
  [& args]
  (aoc/run day [get-solution-01 get-solution-02]))

; =>
; 01.1 answer: 1141
; 01.2 answer: 950

