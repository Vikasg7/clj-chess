(ns clj-chess.engine
  (:use [clj-chess.utils]
        [clj-chess.notation]
        [clj-chess.core])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.core.match :refer [match]]))

(defn basic-move [src dst]
  {:src src, :dst dst})

(defn enhance-move [state move]
  "Enhancing the basic pawn moves while filtering the invalid pawn moves."
  (let [npson (pgn->pos (state :npson))
        board (state :board)
        src   (move :src)
        piece (board src)
        ptype (piece :type)
        playr (piece :player)
        dst   (move :dst)
        [r f] dst
        [x y] src
        prv   (case playr
                "w" dec
                "b" inc)]
  (match ptype
    \P    (cond (= dst npson) [(assoc move :type "enpassant" :nul [(prv r) f])]
                (#{1 8} r)    (mapv #(assoc move :type "promotion" :piece {:player playr, :type %})
                                    [\K \R \B \Q])
                :else         [move])
    :else [move])))

(defn get-moves [state src]
  (let [board (state :board)
        npson (pgn->pos (state :npson))]
  (->> (get-dsts board :move npson src)
       (mapv (partial basic-move src))
       (mapcat (partial enhance-move state)))))

(defn get-all-moves [state player]
  (let [board (state :board)
        srcs  (filter-keys (where? :player player) board)]
  (concat (mapcat (partial get-moves state) srcs)
          (keep (partial pgn->moves state) ["O-O" "O-O-O"]))))

(time (count (get-all-moves (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") "w")))
(time (count (get-all-moves (fen->state "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8") "w")))
(time (count (get-all-moves (fen->state "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10") "w")))


(let [state (fen->state "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")
      board (state :board)]
(->> (get-all-moves state "w")
     (group-by (comp :type board :src))
     (pprint)))