(ns aoc-2017.13.core
  (:require [clojure.test :refer [are is with-test]]
            [clojure.string :as str])
  (:use [aoc-2017.common]))

(defn parse-line [line] (->> (str/split line #": ") (map parse-int)))
(defn str->firewall [input]
  (->> input str/split-lines (mapcat parse-line) (apply array-map)))

(defn tri-wave
  [period time]
  (- period (abs (- (mod time (* 2 (max period 1))) period))))

(def caught? (comp zero? tri-wave))

(defn trip-severity [alerts]
  (reduce + (map (partial apply *) alerts)))

(defn take-trip
  "Finds all the alerts triggered while the packet passes through the firewall."
  [fw]
  (reduce (fn [alerts [r d :as scanner]]
               (if (caught? (dec d) r) (conj alerts scanner) alerts))
          [] fw))

(defn find-lowest-opening-picosecond
  [fw]
  (->> (range)
       (filter (fn [time]
                 (not-any? (fn [[range depth]] (caught? (dec depth) (+ range time))) fw)))
       (first)))


(defn part1 []
  (->> (input-for-day "13") parse take-trip trip-severity))
;; => 1612

(defn part2 []
  (->> (input-for-day "13") parse find-lowest-opening-picosecond))
;; => 3907994

(with-test
  (def example
"0: 3
1: 2
4: 4
6: 4")

  (is (= (->> (parse example) take-trip trip-severity) 24))
  (is (= (->> (parse example) find-lowest-opening-picosecond) 10)))
