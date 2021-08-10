(ns clj-chess.core
  (:use [clj-chess.utils])
  (:require [clojure.string :refer [split join]]
            [clojure.pprint :refer [pprint]]
            [clojure.core.match :refer [match]]))

(defn debug [& vals]
  (apply println vals)
  (identity val))

(defn to-int
  ([v]
    (to-int (str v) v))
  ([v d]
    (try (Integer/parseInt v)
    (catch Exception e d))))

(def char-seq seq)

(def char->file
  (zipmap "abcdefgh" (range 1 9)))

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

(def toggle-player {"w" "b", "b" "w"})

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
  (let [parts (zipmap [:b :p :c :e :h :f]
                      (split fen #" "))]
  {:board      (parse-board (parts :b))
   :player     (parts :p)
   :castling   (char-seq (parts :c))
   :en-passant (parts :e)
   :half-moves (to-int (parts :h) 0)
   :full-moves (to-int (parts :f) 0)}))

(defn state->fen [] 
  nil)

;; cord = [rank file]
(defn in-board? [cord]
  (every? (partial in-range? 1 8) cord))

(defn hit-friendly? [piece player]
  (= player (piece :player)))

(defn hit-enemy? [piece player]
  (not= player (piece :player)))

(defn offsets [piece]
  (match piece
    {:player _   :type \K} [[0 -1] [0 1] [-1 -1] [-1 0] [-1 1] [1 -1] [1 0] [1 1]]
    {:player _   :type \Q} [[0 -1] [0 1] [-1 -1] [-1 0] [-1 1] [1 -1] [1 0] [1 1]]
    {:player _   :type \B} [[-1 -1] [-1 1] [1 -1] [1 1]]
    {:player _   :type \R} [[0 -1] [0 1] [-1 0] [1 0]]
    {:player _   :type \N} [[-2 -1] [-2 1] [1 -2] [-1 -2] [2 -1] [2 1] [1 2] [-1 2]]
    {:player "w" :type \P} [[1 0] [2 0]]
    {:player "b" :type \P} [[-1 0] [-2 0]]))

(defn get-moves
  ([board src]
    (let [piece  (board src)
          ptype  (piece :type)
          player (piece :player)
          steps  (if (#{\K \N} ptype) 1 8)]
    (mapcat (partial get-moves board src player steps)
            (offsets piece))))
  ([board src player steps offset]
    (let [move  (add-vec offset src)
          piece (board move)]
    ;; TODO: Add pinned? clause on the top
    (cond (zero? steps)               nil
          (not (in-board? move))      nil
          (nil? piece)                (lazy-seq (cons move (get-moves board move player (dec steps) offset)))
          (hit-enemy? piece player)    (lazy-seq (cons move nil))
          (hit-friendly? piece player) nil))))

(defn get-src 
  ([board piece dst]
    (let [srcs (filter-by-val board piece)]
    (cond (= 1 (count srcs)) (first srcs)
          :else              (let [has-dst? (fn [src] (.contains (get-moves board src) dst))]
                             (filter has-dst? srcs)))))
  ([board piece dst file]
    (let [eq-file? (comp #{file} second)]
    (first (->> (get-src board piece dst)
                (filter eq-file?))))))

;; move = {:type, :piece, :src, :dst}
(defn make-move [board move]
  (let [{:keys [type src dst piece nul]} move]
  (match type
    "promotion" (assoc board src nil dst (move :piece))
    "enpassant" (assoc board src nil dst (board src) nul nil)
    :else       (assoc board src nil dst (board src)))))

(defn make-moves [board moves]
  (let [[move & res] moves]
  (cond (nil? move) board
        :else       (let [nboard (make-move board move)]
                    (recur nboard res)))))

(defn pgn->moves [state pgn]
  (let [pgn    (->> (remove #{\x \+ \#} pgn)
                    (mapv to-int))
        player (state :player)
        enpson (->> (state :en-passant)
                    (mapv to-int))]
  (case player
    "w" (match pgn
          ;; :king-side-castle
          [\O \- \O]        [{:src [1 5], :dst [1 7]}
                             {:src [1 8], :dst [1 6]}]
          ;; :queen-side-castle
          [\O \- \O \- \O]  [{:src [1 5], :dst [1 3]}
                             {:src [1 1], :dst [1 4]}]
          ;; :pawn-move
          [f r]             (let [f (char->file f)]
                            [{:src [(dec r) f], 
                              :dst [r f]}])
          ;; :pawn-promotion
          [f r \= p]        (let [f (char->file f)]
                            [{:type "promotion",
                              :piece {:player "w", :type p},
                              :src   [(dec r) f],
                              :dst   [r f]}])
          ;; :enpassant
          [(f :guard lower-case?) & ([t r] :guard #(= enpson %))]
                            (let [f (char->file f)
                                  t (char->file t)]
                            [{:type "enpassant",
                              :src  [(dec r) f],
                              :dst  [r t]
                              :nul  [(dec r) t]}])
          ;; :unambigious-pawn-move
          [(f :guard lower-case?) t r]
                            (let [f (char->file f)
                                  t (char->file t)]
                            [{:src [(dec r) f], :dst [r t]}])
          ;; :unambigious-pawn-promotion
          [(f :guard lower-case?) t r \= p]
                            (let [f (char->file f)
                                  t (char->file t)]
                            [{:type "promotion",
                              :piece {:player "w", :type p},
                              :src   [(dec r) f],
                              :dst   [r t]}])
          ;; :piece-move
          [p f r]           (let [f (char->file f)
                                  p {:player "w", :type p}]
                            [{:src (get-src (state :board) p [r f])
                              :dst [r f]}])
          ;; :unambigious-piece-move
          [p f t r]         (let [[f t] (map char->file [f t])
                                  p     {:player "w", :type p}]
                            [{:src (get-src (state :board) p [r t] f)
                              :dst [r t]}])
          :else             (Error. "Invalid pgn"))
    "b" (match pgn
          ;; :king-side-castle
          [\O \- \O]        [{:src [8 5], :dst [8 7]}
                             {:src [8 8], :dst [8 6]}]
          ;; :queen-side-castle
          [\O \- \O \- \O]  [{:src [8 5], :dst [8 3]}
                             {:src [8 1], :dst [8 4]}]
          ;; :pawn-move
          [f r]             (let [f (char->file f)]
                            [{:src [(inc r) f], 
                              :dst [r f]}])
          ;; :pawn-promotion
          [f r \= p]        (let [f (char->file f)]
                            [{:type  "promotion",
                              :piece {:player "b", :type p},
                              :src   [(inc r) f],
                              :dst   [r f]}])
          ;; :enpassant
          [(f :guard lower-case?) & ([t r] :guard #(= enpson %))]
                            (let [f (char->file f)
                                  t (char->file t)]
                            [{:type "enpassant",
                              :src  [(inc r) f],
                              :dst  [r t]
                              :nul  [(inc r) t]}])
          ;; :unambigious-pawn-move
          [(f :guard lower-case?) t r]
                            (let [f (char->file f)
                                  t (char->file t)]
                            [{:src [(inc r) f], :dst [r t]}])
          ;; :unambigious-pawn-promotion
          [(f :guard lower-case?) t r \= p]
                            (let [f (char->file f)
                                  t (char->file t)]
                            [{:type "promotion",
                              :piece {:player "b", :type p},
                              :src   [(inc r) f],
                              :dst   [r t]}])
          ;; :piece-move
          [p f r]           (let [f (char->file f)
                                  p {:player "b", :type p}]
                            [{:src (get-src (state :board) p [r f])
                              :dst [r f]}])
          ;; :unambigious-piece-move
          [p f t r]         (let [[f t] (map char->file [f t])
                                  p     {:player "b", :type p}]
                            [{:src (get-src (state :board) p [r t] f)
                              :dst [r t]}])
          :else             (Error. "Invalid pgn")))))

(-> (parse-board "2k1nrb1/4P1P1/8/8/3K4/8/8/8 w - - 0 1")
    (get-src (char->piece \K) [3 5]))

(-> (parse-board "2k1nrb1/4P1P1/8/8/3K1N2/8/8/8 w - - 0 1")
    (get-src (char->piece \N) [6 5]))
  
(-> (parse-board "2k1nrb1/4P1P1/8/2N5/3K1N2/8/8/8 w - - 0 1")
    (get-src (char->piece \N) [6 5] 6))

;; (print-board (-> (parse-board "rnbqk2r/ppp1nppp/4p3/2bpP3/3P1P2/5N2/PPP3PP/RNBQKB1R")
;;                  (make-move {:type "move" :src [5 3] :dst [4 4]})))

;; (print-board (-> (parse-board "rnbqk2r/ppp1nppp/4p3/2bpP3/3P1P2/5N2/PPP3PP/RNBQK2R")
;;                  (make-moves (pgn->moves {:player "w"} "O-O"))
;;                  (make-moves (pgn->moves {:player "b"} "O-O"))
;;                  (make-moves (pgn->moves {:player "w"} "c3"))
;;                  (make-moves (pgn->moves {:player "b"} "c6"))))

;; (print-board (-> (parse-board "8/3P4/4K3/8/8/4k3/3p4/8")
;;                  (make-moves (pgn->moves {:player "w"} "d8=Q"))
;;                  (make-moves (pgn->moves {:player "b"} "d1=Q"))))

;; (print-board (-> (parse-board "rnbqkbnr/ppp3pp/3p1p2/4p3/3PPP2/8/PPP4P/RNBQKBNR")
;;                  (make-moves (pgn->moves {:player "w"} "fxe5"))
;;                  (make-moves (pgn->moves {:player "b"} "fxe5"))
;;                  (make-moves (pgn->moves {:player "w"} "dxe5"))
;;                  (make-moves (pgn->moves {:player "b"} "dxe5"))))

;; (def state (fen->state "2k1nrb1/4P1P1/8/8/3K4/8/8/8 w - - 0 1"))
(def state (fen->state "2k1nrb1/p3P1P1/8/2N5/3K1N1P/8/8/8 w - - 0 1"))

(def pgns ["Nfe6", "a5", "h5"])

(defn update-game [state pgn]
  (let [moves (pgn->moves state pgn)]
  (-> state
      (update :board make-moves moves)
      (update :player toggle-player))))

(->> (reduce update-game state pgns)
     (:board)
     (print-board))


;; (pprint (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
;; (pprint (fen->state "rnbqk2r/ppp1nppp/4p3/2bpP3/3P1P2/5N2/PPP3PP/RNBQKB1R b KQkq d3 0 5"))

(defn -main []
  nil)
