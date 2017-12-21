(ns aoc-2017.18.core
  (:require [clojure.test :refer [are is with-test]]
            [clojure.string :as str])
  (:use [aoc-2017.common]))

(defn parse-line [line]
  (let [[op x y] (re-seq #"\S+" (.trim line))
        y-parsed (if (and y (re-matches #"-?\d+" y)) (parse-int y) y)]
    (list op x y-parsed)))

(defn parse-input [input] (->> input (.trim) str/split-lines (mapv parse-line)))

(defn read-arith [op]
  (case op
    "add" +
    "mul" *
    "mod" mod))

(defn get-r
  "Returns either `n` if `n` is a number, or the value stored in `n` of the `regs` map"
  [regs n]
  (if (number? n) n (get regs n)))

(defn bizarre-recorder [input]
  (let [insns (parse-input input)]
    (loop [{:keys [pc regs last-snd rcv] :as state}
           {:pc 0 :regs {} :last-snd 0 :rcv nil}]
      (let [[op x y] (insns pc)
            new-state
            (-> (apply assoc state
             (case op
               "set" [:regs (merge regs {x (get-r regs y)})]
               ("add" "mul" "mod") [:regs (merge-with (read-arith op)
                                                (assoc regs x (get regs x 0))
                                                {x (get-r regs y)})]
               "snd" [:last-snd (get-r regs x)]
               "rcv" [:rcv (when-not (zero? last-snd) last-snd)]
               "jgz" [:pc (if (pos? (get-r regs x))
                           (+ pc -1 (get-r regs y))
                           pc)]))
                (update :pc inc))]
        (if-let [freq (:rcv new-state)]
          freq
          (recur new-state))))))


(defn part1 [] (bizarre-recorder (input-for-day "18"))) ;; => 3423

(with-test
  (def example "
set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2
")

  (is (= (bizarre-recorder example) 4)))

