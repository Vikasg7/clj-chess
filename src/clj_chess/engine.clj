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
                (not= f y)    (when (hit-enemy? (board dst) playr)
                                (cond (#{1 8} r) (mapv #(assoc move :type "promotion" :piece {:player playr, :type %})
                                                       [\K \R \B \Q])
                                      :else      [move]))
                (#{1 8} r)    (mapv #(assoc move :type "promotion" :piece {:player playr, :type %})
                                    [\K \R \B \Q])
                :else         [move])
    :else [move])))

(defn get-moves [state src]
  (let [board (state :board)]
  (->> (get-dsts board src)
       (mapv (partial basic-move src))
       (mapcat (partial enhance-move state)))))

;; TODO
(defn get-all-moves [state player])
