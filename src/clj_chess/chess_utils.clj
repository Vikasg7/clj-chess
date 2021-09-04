(ns clj-chess.chess-utils
  (:use [clj-chess.utils])
  (:require [clojure.string :refer [split]]
            [clojure.pprint :refer [pprint]]
            [clojure.core.match :refer [match]]))

(def toggle-player {"w" "b", "b" "w"})

(defn get-pos [board piece]
  (filter-keys (partial = piece) board))

(defn in-board? [pos]
  (every? (partial in-range? 1 8) pos))

(defn hit-friendly? [piece player]
  (when piece
    (= player (piece :player))))

(defn hit-enemy? [piece player]
  (when piece
    (not= player (piece :player))))

(defn poses-between 
  ([[r f :as x] [rr ff :as y]]
    (cond (= r rr) (if (< f ff)
                     (poses-between x y [0 1])
                     (poses-between x y [0 -1]))
          (= f ff) (if (< r rr)
                     (poses-between x y [1 0])
                     (poses-between x y [-1 0]))
          :else    nil))
  ([from-pos to-pos offset]
    (->> (iterate (partial add-vec offset) from-pos)
         (take-while (partial not= to-pos))
         (remove #{from-pos}))))

(defn occupied? [board pos]
  (not (nil? (board pos))))

(defn any-occupied? [board poses]
  (some (partial occupied? board) poses))

(defn threaten? [enm-poses pos]
  (some (partial = pos) enm-poses))

(defn any-threaten? [enm-poses poses]
  (some (partial threaten? enm-poses) poses))

(defn prv-sqr [player pos]
  (case player
    "w" (add-vec [-1 0] pos)
    "b" (add-vec [1 0] pos)))

(defn rank-diff [move]
  (let [[r f] (move :src)
        [a b] (move :dst)]
  (Math/abs ^Integer (- r a))))

(defn pawn? [piece]
  (= (piece :type) \P))

(defn king? [piece]
  (= (piece :type) \K))

(defn rook? [piece]
  (= (piece :type) \R))

(def rank first)
(def file second)
(def invalid? empty?)

(defn castle? [move]
  (= (move :type) "castle"))

(defn castle-side? [[km rm :as moves]]
  (when (= (count moves) 2)
    (let [rook-pos (rm :src)]
    (case (file rook-pos)
      1 :queen
      8 :king))))    