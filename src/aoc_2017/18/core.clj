(ns aoc-2017.18.core
  (:require
   [clojure.test :refer [are is with-test]]
   [clojure.string :as str]
   [clojure.core.async :refer [>! <! <!! alts! chan go-loop offer! timeout]])
  (:use [aoc-2017.common]))

(defn parse-line [line]
  (let [[op & vars] (re-seq #"\S+" (.trim line))
        [x y] (map #(if (and % (re-matches #"-?\d+" %)) (parse-int %) %) vars)]
    (list op x y)))

(defn parse-input [input] (->> input (.trim) str/split-lines (mapv parse-line)))

(defn read-arith [op]
  (case op
    "add" +'   ; Use math fns that auto-promote to real big numbers
    "mul" *'
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

(def duet-wait-ms 100)

;; Part 2 is similar to part 1, but trying to reuse the old function while adding
;; async comms and changing the rules made the code even less legible than it already
;; is. But! It was definitely fun to break out core.async when the puzzle fits, as it
;; did here.
(defn duet-partner [insns id in out ret]
  (go-loop [{:keys [pc regs sends] :as state}
            {:pc 0 :regs {"p" id} :sends 0}]
    (do
      #_(spit (str id "-trace.txt") (str id ": " state "\n") :append true)
      (let [[op x y] (insns pc)
            new-state
            (-> (apply assoc state
              (case op
                "set" [:regs (merge regs {x (get-r regs y)})]
                ("add" "mul" "mod") [:regs (merge-with (read-arith op)
                                                 (assoc regs x (get regs x 0))
                                                 {x (get-r regs y)})]
                ;; Send `x` to the other duet-partner program, and add to the tally
                "snd" [:sends (do (>! out (get-r regs x))
                                  (inc sends))]
                ;; Either receive from the other prog, or timeout and terminate below
                "rcv" [:regs (merge regs
                              (let [[val ch] (alts! [in (timeout duet-wait-ms)])]
                                (if (not= ch in) ;; Hacky way of signalling deadlock
                                  {:timeout true}
                                  {x val})))] ;; Otherwise, set reg as usual
                "jgz" [:pc (if (pos? (get-r regs x))
                             (+ pc -1 (get-r regs y))
                             pc)]))
             (update :pc inc))]
        (if (get-in new-state [:regs :timeout])
          (offer! ret (:sends new-state))
          (recur new-state))))))

(defn perform
  "Orchestrate the duet, linking together channels between the two
  programs for them to talk on. Start the duo, then wait for & return
  prog1's return value - the number of <snd> operations performed."
  [input]
  (let [insns    (parse-input input)
        [c0 c1]  (repeatedly 2 #(chan 1000))
        ret1     (chan)
        leader   (duet-partner insns 0 c0 c1 (chan))
        follower (duet-partner insns 1 c1 c0 ret1)]
    (<!! ret1)))


(defn part1 [] (bizarre-recorder (input-for-day "18"))) ;; => 3423
(defn part2 [] (perform (input-for-day "18"))) ;; => 7493

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

  (is (= (bizarre-recorder example) 4))
  (is (= (perform example) 1)))

