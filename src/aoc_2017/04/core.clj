(ns aoc-2017.04.core
  (:require [aoc-2017.common :as aoc]
            [clojure.test :refer [are is with-test]]
            [clojure.string :as str]))

(defn split-spaces [s] (str/split s #"\s+"))
(defn count-trues [xs] (count (filter identity xs)))

; distinct? checks if every element of a collection is unique. Easy!
(defn valid-passphrase? [p] (apply distinct? (split-spaces p)))

(def day "04")
(defn parse-input [day]
  (->> (aoc/input-filename day) (slurp) (str/split-lines) (remove str/blank?)))
(def input (parse-input day))

(defn part1 []
  (->> (map valid-passphrase? input)
       (count-trues)))
; => 455

(defn part2 []
  (->> (map split-spaces input) ; Ready input lines
       (map (partial map sort)) ; For each word in each line, sort their characters
       (map (partial apply distinct?)) ; For each line, check if the sorted character sequences are unique
       (count-trues)))
; => 186

