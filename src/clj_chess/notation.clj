(ns clj-chess.notation
  (:use [clj-chess.utils])
  (:require [clojure.string :refer [split join]]
            [clojure.pprint :refer [pprint]]))

(def char->piece
  {\r {:player "b", :type \R}
   \n {:player "b", :type \N}
   \b {:player "b", :type \B}
   \q {:player "b", :type \Q}
   \k {:player "b", :type \K}
   \p {:player "b", :type \P}
   \R {:player "w", :type \R}
   \N {:player "w", :type \N}
   \B {:player "w", :type \B}
   \Q {:player "w", :type \Q}
   \K {:player "w", :type \K}
   \P {:player "w", :type \P}})

(def piece->char
  {{:player "b", :type \R} \r
   {:player "b", :type \N} \n
   {:player "b", :type \B} \b
   {:player "b", :type \Q} \q
   {:player "b", :type \K} \k
   {:player "b", :type \P} \p
   {:player "w", :type \R} \R
   {:player "w", :type \N} \N
   {:player "w", :type \B} \B
   {:player "w", :type \Q} \Q
   {:player "w", :type \K} \K
   {:player "w", :type \P} \P})

(def piece->unicode
    {{:player "b", :type \R} "♖"
     {:player "b", :type \N} "♘"
     {:player "b", :type \B} "♗"
     {:player "b", :type \Q} "♕"
     {:player "b", :type \K} "♔"
     {:player "b", :type \P} "♙"
     {:player "w", :type \R} "♜"
     {:player "w", :type \N} "♞"
     {:player "w", :type \B} "♝"
     {:player "w", :type \Q} "♛"
     {:player "w", :type \K} "♚"
     {:player "w", :type \P} "♟︎"
     nil                     (char 0x3000)})

(defn parse-row [[fst & rst :as char-seq]]
  (when fst
    (let [n (to-int fst)]
    (cond (int? n) (let [empty-cells (repeat n nil)]
                   (lazy-seq (concat empty-cells (parse-row rst))))
          :else    (lazy-seq (cons (char->piece n) (parse-row rst)))))))

(defn make-coord-piece-map [coll]
  (let [rev (reverse coll)]
  (for [rank (range 8 0 -1)
        file (range 1 9)
        :let [piece (-> (nth rev (dec rank)) 
                        (nth (dec file)))]]
    [[rank file] piece])))

(defn parse-board [text]
  (->> (split text #"\/")
       (mapv (comp parse-row char-seq))
       (make-coord-piece-map)
       (into (sorted-map))))

(defn print-board [board]
  (->> (vals board)
       (mapv piece->unicode)
       (partition 8)
       (reverse)
       (mapv (partial join "|"))
       (pprint)))

(defn fen->state [fen]
  (let [[b p c e h f] (split fen #" ")]
  {:board (parse-board b)
   :playr p
   :casle (if (= c "-") [] (char-seq c))
   :npson (if (= e "-") nil e)
   :hfmvs (to-int h 0)
   :flmvs (to-int f 0)}))

(defn unparse-row [coll]
  (let [f (fn [grp] 
            (if (> (.indexOf grp nil) -1) 
              (count grp) 
              (apply str grp)))]
  (->> (mapv f coll)
       (join ""))))

(defn unparse-board [board]
  (->> (vals board)
       (mapv piece->char)
       (partition 8)
       (reverse)
       (mapv (partial partition-by char?))
       (mapv unparse-row)
       (join "/")))

(defn state->fen [state] 
  (let [{:keys [board playr casle npson hfmvs flmvs]} state]
  (join " " [(unparse-board board)
             playr
             (if (empty? casle) "-" (apply str casle))
             (if-not npson "-" npson)
             hfmvs
             flmvs])))

(def char->file
  (zipmap "abcdefgh" (range 1 9)))

(def file->char
  (zipmap (range 1 9) "abcdefgh"))

(defn cord->pgn [[r f]]
  (str (file->char f) r))
