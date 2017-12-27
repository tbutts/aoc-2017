(ns aoc-2017.23.core
  (:require
   [clojure.test :refer [are is with-test]]
   [clojure.string :as str])
  (:use [aoc-2017.common]))

;; Copy of parser/helper functions from day 18,
(defn parse-line [line]
  (let [[op & vars] (re-seq #"\S+" (.trim line))
        [x y] (map #(if (and % (re-matches #"-?\d+" %)) (parse-int %) (first %)) vars)]
    (list op x y)))

(defn parse-input [input] (->> input (.trim) str/split-lines (mapv parse-line)))

;; Changed `read-arith` to only include operations in day 23
(defn read-arith [op]
  (case op
    "sub" -'   ; Use math fns that auto-promote to real big numbers
    "mul" *'))

(defn get-r
  "Returns either `n` if `n` is a number, or the value stored in `n` of the `regs` map"
  [regs n]
  (if (number? n) n (get regs n)))

(def init-regs (zipmap (seq "abcdefgh") (repeat 0)))

;; And changed the main iterative function
(defn bizarre-coprocessor [input]
  (let [insns (parse-input input)]
    (loop [{:keys [pc regs muls] :as state}
           {:pc 0 :regs init-regs :muls 0}]
      (if-let [[op x y] (nth insns pc nil)]
        (recur
         (-> (case op
            "set" (assoc state :regs (merge regs {x (get-r regs y)}))
            "sub" (assoc state :regs (merge-with -' regs {x (get-r regs y)}))
            "mul" (->
                   (assoc state :regs (merge-with *' regs {x (get-r regs y)}))
                   (update :muls inc))
            "jnz" (if-not (zero? (get-r regs x))
                    (update state :pc + (get-r regs y) -1)
                    state))
             (update :pc inc)))
        {:muls muls}))))

(defn part1 [] (-> (input-for-day "23") bizarre-coprocessor :muls))

(with-test
  (def example)

  ;; What the! There are no examples for part 1!
  (is (= 0 0)))

