(ns aoc-2017.19.core
  (:require
   [clojure.test :refer [are is with-test]]
   [clojure.string :as str])
  (:use [aoc-2017.common]))

(def dirs-map (array-map
   :n [dec identity]
   :e [identity dec]
   :s [inc identity]
   :w [identity inc]))

(def turns-map
  "Generate a mapping like {:n {:left :w :right :e} ...} using some fiddly
  hoo-wa on the ordered keys of `dirs-map` , by shifting the vector left & right
  to get the cardinal directions to the left and right of each direction.

  I should just have hand-wrote this, but this is more fun."
  (into {} (apply map #(vector %1 {:left %2 :right %3})
                  ((juxt identity #(rotate-r 1 %) #(rotate 1 %)) (keys dirs-map)))))

(defn next-forward [dir coord]
  (mapv #(%1 %2) (dir dirs-map) coord))

(defn next-turn
  "Returns either valid neighbor on `grid` to the left or right of `coord` based on
  the current direction `cdir`. A neighbor is valid if the character on the grid in
  the neighbor's position is not a space. Returns nil if neither neighbor is valid."
  [grid cdir coord]
  (some (fn check [dir]
          (let [pos (next-forward dir coord)
                marker (get-in grid pos)]
            (when (and marker (not= marker \ ))
              [dir pos])))
        (map #(get-in turns-map [%1 %2]) (repeat 2 cdir) [:left :right])))

(defn find-start [grid] (str/index-of (grid 0) \|))

(defn follow-tubes
  "Navigates the ASCII path, returning the string of letters passed over along the way.

  `grid` is expected to be a vector of strings. The first row should contain only one
  non-whitespace character, (pipe `|`), used as the starting position headed south.
  The journey follows straight along pipes | or hyphens - until a plus + is
  encountered, signalling a turn either left or right (whichever doesn't lead to a
  space character). This ends when the `grid` leads to a dead end (either a space in
  front, or out of bounds)."
  [grid]
  (loop [pos [0 (find-start grid)]
         dir :s
         path []]
    (let [ch (get-in grid pos)]
      #_(println [pos dir path ch])
      (case ch
        (nil \ ) (str/join path)
        (\| \-) (recur (next-forward dir pos) dir path)
        \+      (let [[ndir npos] (next-turn grid dir pos)]
                  (recur npos ndir path))
        ;; Capitals are path markers (Case statements need literals, so the case
        ;; is copied out long-form here)
        (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z)
        (recur (next-forward dir pos) dir (conj path ch))))))


(defn parse-input [in]
  (-> in (str/replace #"(^ *\n|\n *$)" "") str/split-lines vec))

(defn part1 [] (-> (input-for-day "19") parse-input follow-tubes)) ; => "GINOWKYXH"

(with-test
  (def example "
     |
     |  +--+
     A  |  C
 F---|----E|--+
     |  |  |  D
     +B-+  +--+
")

  (is (= (->> example parse-input follow-tubes) "ABCDEF")))

