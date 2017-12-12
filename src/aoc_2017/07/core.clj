(ns aoc-2017.07.core
  (:require [aoc-2017.common :as aoc]
            [clojure.test :refer [are is with-test]]
            [clojure.string :as str]
            [clojure.set :as set]))

;(defn parse-int [n] (Integer/parseInt n 10))

(def blacklist-re #"[(),>\\-]")
(defn parse-line [line] (-> line (str/replace blacklist-re "") (str/split #"\s+")))
(defn parse-input [input] (map parse-line (str/split-lines input)))

(defn parents-and-kids
  "Collects two sets of all parent progs and child progs, returning the two in a vector."
  [progs]
  (reduce (fn add-rk-from-line [[roots kids] prog-line]
            [(into roots (take 1 prog-line)) (into kids (drop 2 prog-line))])
          [#{} #{}] progs))

(defn find-root
  "Rinds the root program in the tower.
  `progs` is a seq of seqs, '([<program> <weight> <sub-prog> ...] [...])"
  [progs]
  (->> (parents-and-kids progs) (apply set/difference) first))

(with-test
  (def example
"pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")

  (is (= (find-root (parse-input example)) "tknk")))

(def day "07")
(def input
  (->> (aoc/input-filename day) (slurp) parse-input))


