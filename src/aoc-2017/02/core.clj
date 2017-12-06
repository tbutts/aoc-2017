(ns aoc-2017.02.core
  (:require [clojure.test :refer [are is with-test]]
            [clojure.string :as str]))

(defmacro applier
  [f]
  `(def ~(symbol (str (name f) "s"))
     (fn [xs#] (apply ~f xs#))))

(applier max)
(applier min)
(applier -)

(defn diff-max-min
  [nums]
  (-s ((juxt maxs mins) nums)))

(defn checksum
  [data]
  (reduce #(+ %1 (diff-max-min %2)) 0 data))

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

  (is (= (checksum (parse-input example)) 18)))


(def day "02")
(def filename (str "src/aoc-2017/" day "/input"))
(def challenge-input (slurp filename))

(def solve-01 #(checksum (parse-input challenge-input)))

(defn print-ans [day part solver] (println (str day "." part) "answer:" (solver)))
(defn run [solvers] (dorun (map-indexed #(print-ans day (inc %1) %2) solvers)))

(defn -main
  [& args]
  (run [solve-01]))

