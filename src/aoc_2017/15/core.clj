(ns aoc-2017.15.core
  (:require [clojure.test :refer [are is with-test]])
  (:use [aoc-2017.common]))

(def magic-divisor 2147483647)

(defn gen [factor n]
  (-> (* factor n) (rem magic-divisor)))
(def factors {:A 16807 :B 48271})
(def a-gen (partial gen (:A factors)))
(def b-gen (partial gen (:B factors)))

(def =low-4-bytes (comp zero? #(bit-and 0xFFFF %) bit-xor))

(defn judge
  ([apred bpred a b] (judge apred bpred a b 0))
  ([apred bpred a b n]
   (let [da (some #(when (apred %) %) (drop 1 (iterate a-gen a)))
         db (some #(when (bpred %) %) (drop 1 (iterate b-gen b)))
         dn ((if (=low-4-bytes da db) inc identity) n)]
     (cons dn (lazy-seq (judge apred bpred da db dn))))))

(defn multiple4? [x] (zero? (mod x 4)))
(defn multiple8? [x] (zero? (mod x 8)))

(def lax-judge (partial judge identity identity))
(def picky-judge (partial judge multiple4? multiple8?))

(def warhammer-judger-40m (comp first #(drop (dec 4e7) %) lax-judge))
(def judge-reinhold-5m    (comp first #(drop (dec 5e6) %) picky-judge))

(defn parse-gen-starts [in] (->> (re-seq #"\d+" in) (map parse-int)))
(defn part1 []
  (->> (input-for-day "15") parse-gen-starts (apply warhammer-judger-40m)))
;; => 650
(defn part2 []
  (->> (input-for-day "15") parse-gen-starts (apply judge-reinhold-5m)))
;; => 336
;; Both parts run pretty slowly (30s + 18s on my laptop). Would like to improve speeds.

(with-test
  (def example-seeds [65 8921])

  (is (= (apply warhammer-judger-40m example-seeds) 588))
  (is (= (apply judge-reinhold-5m example-seeds) 309)))

