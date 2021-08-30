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
       (mapcat (partial enhance-move state))
       (mapv vector))))

(defn get-all-moves [state]
  (let [board (state :board)
        playr (state :playr)
        srcs  (filter-keys (where? :player playr) board)]
  (concat (mapcat (partial get-moves state) srcs)
          (keep (partial pgn->move state) ["O-O" "O-O-O"]))))

;; (defn perft 
;;   ([state depth]
;;     (let [moves (get-all-moves state)]
;;     (cond (= 1 depth) (count moves)
;;           :else       (->> (pmap (partial perft state depth) moves)
;;                            (reduce +)))))
;;   ([state depth move]
;;     (perft (play-move state move) (dec depth))))

(defn perft 
  ([state depth]
    (let [moves (get-all-moves state)]
    (cond (= 1 depth) (count moves)
          :else       (->> (map (partial perft state depth) moves)
                           (reduce +)))))
  ([state depth [mv :as move]]
    (let [mvt (str (pos->pgn (mv :src)) (pos->pgn (mv :dst)))
          cnt (perft (play-move state move) (dec depth))]
    (println (state->fen state) mvt cnt)
    cnt)))

(let [state (fen->state "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")]
(perft state 2))

(pprint (get-all-moves (fen->state "8/2p5/3p4/KP5r/1R2Pp1k/8/6P1/8 b - e3 0 2")))

(state->fen (play-pgn (fen->state "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 2") "e4"))