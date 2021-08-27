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
  (let [board (state :board)
        src   (move :src)
        piece (board src)
        ptype (piece :type)]
  (match ptype
    \P    (let [npson (pgn->pos (state :npson))
                playr (piece :player)
                dst   (move :dst)
                [r f] dst
                [x y] src
                prv   (case playr
                        "w" dec
                        "b" inc)] 
          (cond (= dst npson) [(assoc move :type "enpassant" :nul [(prv r) f])]
                (#{1 8} r)    (mapv #(assoc move :type "promotion" :piece {:player playr, :type %})
                                    [\K \R \B \Q])
                :else         [move]))
    :else [move])))

(defn get-moves [state src]
  (let [board (state :board)
        npson (pgn->pos (state :npson))]
  (->> (get-dsts board :move npson src)
       (mapv (partial basic-move src))
       (mapcat (partial enhance-move state)))))

(defn get-all-moves [player state]
  (let [board (state :board)
        srcs  (filter-keys (where? :player player) board)]
  (concat (mapcat (partial get-moves state) srcs)
          (keep (partial pgn->moves state) ["O-O" "O-O-O"]))))

(let [fens ["rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
            "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
            "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"]
      mvcnt (comp count
                  (partial get-all-moves "w")
                  fen->state)]
(mapv mvcnt fens))