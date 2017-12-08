(ns aoc-2017.04.core
  (:require [aoc-2017.common :as aoc]
            [clojure.test :refer [are is with-test]]
            [clojure.string :as str]))

; distinct? checks if every element of a collection is unique. Easy!
(defn valid-passphrase? [p] (apply distinct? (str/split p #"\s+")))

(def day "04")
(defn parse-input [day]
  (->> (aoc/input-filename day) (slurp) (str/split-lines) (remove str/blank?)))
(def input (parse-input day))

(defn part1 [] (->> (map valid? input) (filter identity) (count)))
; => 455

