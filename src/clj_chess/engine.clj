(ns clj-chess.engine
  (:use [clj-chess.utils]
        [clj-chess.notation]
        [clj-chess.core])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.core.match :refer [match]]))

(defn get-all-moves [state]
  (let [board (state :board)
        playr (state :playr)
        srcs  (filter-keys (where? :player playr) board)]
  (concat (mapcat (partial get-moves state) srcs)
          (keep (partial pgn->move state) ["O-O" "O-O-O"]))))

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

(let [state (fen->state "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")]
(perft state 3))

(pprint (get-all-moves (fen->state "rnbq1k1r/pp1Pbppp/2p5/8/2B5/N2n4/PPP1N1PP/R1BQK2R w KQ - 3 2")))

(state->fen (play-pgn (fen->state "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 2") "e4"))