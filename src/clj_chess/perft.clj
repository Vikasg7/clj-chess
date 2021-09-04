(ns clj-chess.perft
  (:use [clj-chess.utils]
        [clj-chess.notation]
        [clj-chess.core])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.core.match :refer [match]]))

(defn perft 
  ([state depth]
    (let [moves (get-all-moves state)]
    (cond (= 1 depth) (count moves)
          :else       (->> (pmap (partial perft state depth) moves)
                           (reduce +)))))
  ([state depth move]
    (perft (play-move state move) (dec depth))))

;; (defn perft 
;;   ([state depth]
;;     (let [moves (get-all-moves state)]
;;     (cond (= 1 depth) (count moves)
;;           :else       (->> (map (partial perft state depth) moves)
;;                            (reduce +)))))
;;   ([state depth move]
;;     (let [mvt (str (pos->pgn (move :src)) (pos->pgn (move :dst)))
;;           cnt (perft (play-move state move) (dec depth))]
;;     (println (state->fen state) mvt cnt)
;;     cnt)))

;; (let [state (fen->state "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10")]
;; (perft state 1))

;; (pprint (get-all-moves (fen->state "rnbq1k1r/pp1Pbppp/2p5/8/2B5/N2n4/PPP1N1PP/R1BQK2R w KQ - 3 2")))

;; (state->fen (play-pgn (fen->state "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 2") "e4"))