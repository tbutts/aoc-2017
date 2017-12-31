(ns aoc-2017.25.core
  (:require
   [clojure.test :refer [are is with-test]]
   [clojure.string :as str])
  (:use [aoc-2017.common]))

(def state-re #"In state ([A-Z]):
  If the current value is ([01]):
    - Write the value ([01])\.
    - Move one slot to the (left|right)\.
    - Continue with state ([A-Z])\.
  If the current value is ([01]):
    - Write the value ([01])\.
    - Move one slot to the (left|right)\.
    - Continue with state ([A-Z])\.")

(defn regex-groups->state
  [[full-match state-name & rem]]
  {(first state-name)
   (apply merge (map (fn [[cval nval move cont]]
                       {(byte (parse-int cval))
                        {:write       (byte (parse-int nval))
                         :move        (if (= move "right") inc dec)
                         :next-state  (first cont)}})
                     (split-at 4 rem)))})

(defn str->tm-instructions
  [input]
  {:start  (->> input (re-find #"Begin in state (\w).") second first)
   :steps  (->> input (re-find #"diagnostic checksum after (\d+)") second parse-int)
   :states (->> input (re-seq state-re) (map regex-groups->state) (apply merge {}))})

(defn run-turing-machine
  [machine]
  (loop [iters (:steps machine)
         pos   0
         tape  (transient {})
         state (:start machine)]
    (if (zero? iters)
      (vals (persistent! tape))
      (let [state-actions (get-in machine [:states state])
            {:keys [write move next-state]} (state-actions (get tape pos 0))]
        (recur
         (dec iters)
         (move pos)
         (assoc! tape pos write)
         next-state)))))

(def checksum (partial reduce +))

(defn input->tape-checksum [input]
  (->> input str->tm-instructions run-turing-machine checksum))

(defn part1 [] (->> (input-for-day "25") input->tape-checksum)) ; => 4217

(with-test
  (def example (.trim "
Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A."))

  (is (= (input->tape-checksum example) 3)))

