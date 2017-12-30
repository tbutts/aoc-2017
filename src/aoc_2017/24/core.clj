(ns aoc-2017.24.core
  (:require
   [clojure.test :refer [are is with-test]]
   [clojure.string :as str])
  (:use [aoc-2017.common]))

(defn update-graph
  [f pg a b]
  (-> pg
      (update a f b)
      (update b f a)))

;; Together with `clojure.core/update`, works like python's defaultdict
(def conj-set (fnil conj #{}))

(def add-component (partial update-graph conj-set))
(def del-component (partial update-graph disj))

(defn str->portgraph
  "From lines of '0/1', '3/9', etc. returns a map of {0 #{1 3 4}, 1 #{0}, ...}

  The map is built to act as an undirected graph."
  [input]
  (reduce (fn [pg line]
            (let [[a b] (->> (re-seq #"\d+" line) (map parse-int))]
              (add-component pg a b)))
          {} (str/split-lines input)))

(defn build-bridges
  "Returns all the longest possible paths, starting from vertex '0'"
  [portgraph]
  (let [paths (transient [])] ; Disgusting mutable data! ...Makes this cleaner.
    ((fn build [nodes path]
       (let [a (peek path)              ; Use the latest vertex,
             neighbors (nodes a)]       ; get all possible connections.
         (if (empty? neighbors)         ; If there aren't any
           (conj! paths path)           ;    the path is fully constructed!
           (doseq [b neighbors]         ; Otherwise, use each connection
             (build                     ;    and keep building out the path,
              (del-component nodes a b) ;    removing the used connection,
              (conj path a b))))))      ;    and appending a new latest vertex.
     portgraph [0])
    (persistent! paths)))

(defn score
  [bridge-seq]
  (apply + bridge-seq))

(defn strongest-bridge-score
  [bridge-seqs]
  (reduce (fn [best bseq] (max best (score bseq))) 0 bridge-seqs))


(defn input->strongest [input]
  (->> input str->portgraph build-bridges strongest-bridge-score))

(defn part1 [] (->> (input-for-day "24") input->strongest)) ; => 1695

(with-test
  (def example (.trim "
0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10
"))

  (is (= (input->strongest example) 31))
  (is (= (-> example str->portgraph build-bridges count) 5)))

