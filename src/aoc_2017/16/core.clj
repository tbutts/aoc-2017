(ns aoc-2017.16.core
  (:require [clojure.test :refer [are is with-test]]
            [clojure.string :as str])
  (:import (java.io BufferedReader StringReader))
  (:use [aoc-2017.common]))

(def parse-line (comp rest (partial re-matches #"([sxp])(\w+)(?:/(\w+))?")))

(defn dance
  [starting-lineup steps]
  (->> (BufferedReader. (StringReader. steps))
       (comma-seq)
       (map parse-line)
       (reduce
        (fn [lineup [[move] a b]]
          (case move
            \s (vec (rotate-r (parse-int a) lineup))
            \x (swap-idx lineup (parse-int a) (parse-int b))
            \p (let [s (str/join lineup)]
                 (apply swap-idx lineup (map #(str/index-of s %) [a b])))
            lineup))
        (vec starting-lineup))
       str/join))

(def prog-lineup (str/join (map #(char (+ 97 %)) (range 16))))
(defn part1 []
  (->> (input-for-day "16") (.trim) (dance prog-lineup)))

(with-test
  (def eg-steps "s1,x3/4,pe/b")

  (is (= (dance "abcde" eg-steps) "baedc")))

