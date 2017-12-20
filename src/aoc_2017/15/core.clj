(ns aoc-2017.15.core
  (:require [clojure.test :refer [are is with-test]])
  (:use [aoc-2017.common]))

(def factors {:A 16807 :B 48271})
(def magic-divisor 2147483647)

(def =low-4-bytes (comp zero? #(bit-and 0xFFFF %) bit-xor))

(defn gen [factor n]
  (-> (* factor n) (rem magic-divisor)))
(def a-gen (partial gen (:A factors)))
(def b-gen (partial gen (:B factors)))

(defn judge
  ([a b] (judge a b 0))
  ([a b n]
   (let [da (a-gen a)
         db (b-gen b)
         dn ((if (=low-4-bytes da db) inc identity) n)]
     (cons dn (lazy-seq (judge da db dn))))))

(def warhammer-judger-40m (comp first #(drop (- 4e7 1) %) judge))


(defn parse-gen-starts
  [in]
  (->> (re-seq #"\d+" in)
       (map parse-int)))

(defn part1 []
  (->> (input-for-day "15") parse-gen-starts (apply warhammer-judger-40m)))
;; => 650

(with-test
  (def example-seeds [65 8921])

  (is (= (apply warhammer-judger-40m example-seeds) 588)))

