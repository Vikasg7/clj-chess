(ns clj-chess.core
  (:use [clj-chess.utils]
        [clj-chess.notation])
  (:require [clojure.string :refer [split]]
            [clojure.pprint :refer [pprint]]
            [clojure.core.match :refer [match]]))

(declare get-moves make-move)

(def toggle-player {"w" "b", "b" "w"})

(defn get-pos [board piece]
  (filter-keys (partial = piece) board))

(defn in-check? [board player]
  (let [king     {:player player, :type \K}
        king-pos (first (get-pos board king))
        enemies  (filter-keys (where? :player (toggle-player player)) board)]
  (->> (mapcat (partial get-moves board false) enemies)
       (some (partial = king-pos)))))

(defn pinned? [board player src dst]
  (let [nboard (make-move board {:src src, :dst dst})]
  (in-check? nboard player)))

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
    {:player "w" :type \P} [[1 0] [2 0] [1 -1] [1 1]]
    {:player "b" :type \P} [[-1 0] [-2 0] [-1 1] [-1 -1]]))

(defn get-moves
  ([board src]
    (get-moves board true src))
  ([board pin-chk? src]
    (let [piece  (board src)
          ptype  (piece :type)
          player (piece :player)
          steps  (if (#{\K \N \P} ptype) 1 8)]
    (mapcat (partial get-moves board pin-chk? src player steps)
            (offsets piece))))
  ([board pin-chk? src player steps offset]
    (let [dst   (add-vec offset src)
          piece (board dst)]
    (cond (zero? steps)                 nil
          (not (in-board? dst))         nil
          (when pin-chk? (pinned? board player src dst))
                                        nil
          (nil? piece)                  (lazy-seq (cons dst (get-moves board pin-chk? dst player (dec steps) offset)))
          (hit-enemy? piece player)     (lazy-seq (cons dst nil))
          (hit-friendly? piece player)  nil))))

(defn ambiguity-hint [file rank]
  (match [file rank]
    [nil nil] identity
    [_   nil] (fn [[r f]] (= f file))
    [nil   _] (fn [[r f]] (= r rank))
    :else     (fn [[r f]] (= [r f] [rank file]))))

(defn get-srcs
  ([board piece dst]
    (get-srcs board piece dst nil nil))
  ([board piece dst file rank]
    (let [srcs (->> (get-pos board piece)
                    (filter (ambiguity-hint file rank)))]
    (match (count srcs) 
      1     srcs
      :else (let [attacks-dst? (fn [src] (.contains (get-moves board false src) dst))]
            (filter attacks-dst? srcs))))))

(def get-src (comp first get-srcs))

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
        player (state :playr)
        board  (state :board)
        enpson (->> (state :npson)
                    (mapv to-int))
        prv    (case player
                 "w" dec
                 "b" inc)
        recurr (partial pgn->moves state)]
  (match pgn
    ;; :king-side-castle
    [\O \- \O]        (case player
                        "w" (mapcat recurr ["Kg1" "Rhf1"])
                        "b" (mapcat recurr ["Kg8" "Rhf8"]))
    ;; :queen-side-castle
    [\O \- \O \- \O]  (case player
                        "w" (mapcat recurr ["Kc1" "Rad1"])
                        "b" (mapcat recurr ["Kc8" "Rad8"]))
    ;; :pawn-move
    [f r]             (let [f (char->file f)
                            p {:player player :type \P}]
                      [{:src (get-src board p [r f] f nil),
                        :dst [r f]}])
    ;; :pawn-promotion
    [f r \= p]        (let [f (char->file f)]
                      [{:type "promotion",
                        :piece {:player player, :type p},
                        :src   [(prv r) f],
                        :dst   [r f]}])
    ;; :enpassant
    [(f :guard lower-case?) & ([t r] :guard #(= enpson %))]
                      (let [[f t] (map char->file [f t])]
                      [{:type "enpassant",
                        :src  [(prv r) f],
                        :dst  [r t]
                        :nul  [(prv r) t]}])
    ;; :unambigious-pawn-move
    [(f :guard lower-case?) t r]
                      (let [[f t] (map char->file [f t])]
                      [{:src [(prv r) f], :dst [r t]}])
    ;; :unambigious-pawn-promotion
    [(f :guard lower-case?) t r \= p]
                      (let [[f t] (map char->file [f t])]
                      [{:type "promotion",
                        :piece {:player player, :type p},
                        :src   [(prv r) f],
                        :dst   [r t]}])
    ;; :piece-move
    [p f r]           (let [f (char->file f)
                            p {:player player, :type p}]
                      [{:src (get-src board p [r f])
                        :dst [r f]}])
    ;; :unambigious-piece-move (hint: file)
    [p (f :guard is-letter?) t r]         
                      (let [[f t] (map char->file [f t])
                            p     {:player player, :type p}]
                      [{:src (get-src board p [r t] f nil)
                        :dst [r t]}])
    ;; :unambigious-piece-move (hint: rank)
    [p (g :guard int?) t r]         
                      (let [t (char->file t)
                            p {:player player, :type p}]
                      [{:src (get-src board p [r t] nil g)
                        :dst [r t]}])
    ;; :unambigious-piece-move (hint: file, rank)
    [p f g t r]       (let [[f t] (map char->file [f t])]
                      [{:src [g f], :dst [r t]}])
    :else             "Invalid pgn")))

(defn npson-sqr [state [move]]
  (let [board (state :board)
        playr (state :playr)
        piece (board (move :src))
        prv   (case playr
                "w" dec
                "b" inc)
        [r f] (move :dst)]
  (when (and (= (piece :type) \P)
             (= (rank-diff move) 2)
             (or (= (get-in board [[r (inc f)] :type]) \P)
                 (= (get-in board [[r (dec f)] :type]) \P)))
    (cord->pgn [(prv r) f]))))

(defn play-move [state pgn]
  (let [moves (pgn->moves state pgn)]
  (-> state
      (update :npson (constantly (npson-sqr state moves)))
      (update :board make-moves moves)
      (update :playr toggle-player))))

(defn play-moves [state pgns]
  (reduce play-move state pgns))

(defn play-moves-traces [state pgns]
  (reductions play-move state pgns))

(defn rank-diff [move]
  (let [[r f] (move :src)
        [a b] (move :dst)]
  (Math/abs ^Integer (- r a))))

(defn -main []
  (let [state (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
        pgns (split "e4 e6 f4 d5 e5 Bc5 Nf3 Ne7 d4 Bb6 Be2 c5 Be3 Nf5 Bf2 cxd4 Nxd4 Bxd4 Bxd4 O-O Bf2 Qa5+ Nc3 Nc6 a3 d4 b4 Qd8 Ne4 b6 Bd3 Bb7 Qh5 Nce7 Ng5 h6 Ne4 Ng6 g3 Rc8 O-O Bd5 Rae1 Qc7 Re2 Bc4 Kg2 Nge7 g4 Ne3+ Bxe3 Bxd3 cxd3 dxe3 Rxe3 Qc2+ Rf2 Qc3 Nxc3 Rxc3 f5 Nd5 Rg3 exf5 gxf5 Re8 Qxh6 Ne3+ Kh3 Nxf5 Rxf5 g6 Rxg6+ fxg6 Qxg6+ Kh8 Rh5#" #" ")]
  (time (->> (play-moves state pgns)
             (:board)
             (print-board)))))
