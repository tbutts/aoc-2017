(ns aoc-2017.20.core
  (:require
   [clojure.test :refer [are is with-test]]
   [clojure.string :as str])
  (:use [aoc-2017.common]))

(defrecord Point3d [x y z])
(defrecord Particle [^Point3d p ^Point3d v ^Point3d a])

; Sum together x, y, & z fields on two or more points
(def add (partial merge-with +))

(defn particle-path
  "Infinite sequence of the particle's state, as it travels through the 3d plane.

  Something something tensors!"
  [^Particle {:keys [p v a]}]
  (let [dv (add v a)                    ; Add acceleration to velocity
        dp (add p dv)                   ; Add the latest velocity to position
        state (->Particle dp dv a)]     ; Make a new particle!
    (cons state (lazy-seq (particle-path state)))))

(def manhattan-dist
  "[...] simply the sum of the absolute values of a particle's X, Y, and Z position."
  (comp (partial apply +) (partial map abs) vals))

(defn time-close-to-origin
  "Follow the `particle` path until the particle's manhattan distance goes beyond
  `dist-threshold`. Count the iterations it took to get that far."
  [dist-threshold particle]
  (->> (particle-path particle)
       (map :p)
       (take-while #(<= (manhattan-dist %) dist-threshold))
       count))

(defn track-particles
  "Follow every particle in `swarm`, and find the particle that took the longest to
  move out of `dist-threshold` from the origin (0,0,0)."
  [dist-threshold swarm]
  (let [dists (vec (pmap (partial time-close-to-origin dist-threshold) swarm))]
    (loop [i 0 best [-1 -1]]
      (if (>= i (count dists))
        (first best)
        (recur (inc i) (if (>= (dists i) (second best))
                         [i (dists i)]
                         best))))))

(defn consistently-closest
  "Run the simulation a handful of times with different stopping points, and
  tally up the particles that remained closest at those different points.
  The particle that appears the most is probably tending to stick closest to
  the origin the longest, answering the puzzle!"
  [swarm]
  (->>
   (map #(track-particles % swarm) (take 6 (iterate #(* % 10) 1000)))
   frequencies
   (reduce-kv #(if (> %3 (second %1)) [%2 %3] %1) [-1 -1])
   first))


(defn parse-particle [in]
  (->> in
       (re-seq #"<.*?>")                ; Break apart the <x,y,z>, ... parts
       (map #(->> %                     ; On each <x,y,z>
                  (re-seq #"-?\d+")     ; Grab the number-like strings
                  (map parse-int)       ; Convert str -> int
                  (apply ->Point3d)))   ; Build a 3d point
       (apply ->Particle)))             ; Take the three 3d points, and make a Particle

(defn parse-input [in]
  (->> in (.trim) str/split-lines (map parse-particle)))

(defn part1 [] (-> (input-for-day "20") parse-input consistently-closest)) ; => 144

(with-test
  (def example "
p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>
")

  (is (= (-> example parse-input consistently-closest) 0)))
