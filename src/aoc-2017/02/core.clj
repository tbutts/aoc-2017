(ns aoc-2017.02.core
  (:require [clojure.test :refer [are is with-test]]
            [clojure.string :as str]))

(def maxs (partial apply max))
(def mins (partial apply min))
(def -s (partial apply -))

(defn diff-max-min
  [nums]
  (-s ((juxt maxs mins) nums)))

(defn divisor-or-zero
  [n div]
  (let [ans (/ n div)]
    (if (integer? ans) ans 0)))

(defn only-even-divisors
  [nums]
  (loop [[x & xs] (sort > nums)]
    (if-let [res (some #(when (not (zero? %)) %)
                       (map (partial divisor-or-zero x) xs))]
      res
      (if (= (count xs) 0) 0 (recur xs)))))

(defn checksum
  [transformer data]
  (reduce + 0 (map transformer data)))

(def checksum1 (partial checksum diff-max-min))
(def checksum2 (partial checksum only-even-divisors))

(defn digit-array
  [s]
  (map #(Integer/parseInt % 10) (str/split s #"\s+")))

(defn parse-input
  [input]
  (map digit-array (str/split-lines input)))

(with-test
  (def example "5 1 9 5\n7 5 3\n2 4 6 8")

  (are [in out] (= (diff-max-min in) out)
    [5 1 9 5] 8
    [7 5 3] 4
    [2 4 6 8] 6)

  (is (= (checksum1 (parse-input example)) 18))

  (are [in out] (= (only-even-divisors in) out)
    [5 9 2 8] 4
    [9 4 7 3] 3
    [3 8 6 5] 2))


(def day "02")
(def filename (str "src/aoc-2017/" day "/input"))
(def challenge-input (slurp filename))

(def solve-01 #(checksum1 (parse-input challenge-input)))
(def solve-02 #(checksum2 (parse-input challenge-input)))

(defn print-ans [day part solver] (println (str day "." part) "answer:" (solver)))
(defn run [solvers] (dorun (map-indexed #(print-ans day (inc %1) %2) solvers)))

(defn -main
  [& args]
  (run [solve-01 solve-02]))

