(ns clj-chess.core
  (:use [clj-chess.utils]
        [clj-chess.notation])
  (:require [clojure.string :refer [split]]
            [clojure.pprint :refer [pprint]]
            [clojure.core.match :refer [match]]))

(declare get-dsts make-move)

(def toggle-player {"w" "b", "b" "w"})

(defn get-pos [board piece]
  (filter-keys (partial = piece) board))

(defn attacked? [board player pos]
  (let [enemies  (filter-keys (where? :player (toggle-player player)) board)]
  (->> (mapcat (partial get-dsts board :atak) enemies)
       (some (partial = pos)))))

(defn in-check? [board player]
  (let [king     {:player player, :type \K}
        king-pos (first (get-pos board king))]
  (attacked? board player king-pos)))

(defn pinned? [board player src dst]
  (let [nboard (make-move board {:src src, :dst dst})]
  (in-check? nboard player)))

;; pos = [rank file]
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

(defn castle-moves [king-move]
  (let [[r f :as src] (king-move :src)
        [a b :as dst] (king-move :dst)]
  (cond (> b f) [{:src src :dst dst} 
                 {:src [r 8] :dst [r 6]}]
        (< b f) [{:src src :dst dst} 
                 {:src [r 1] :dst [r 4]}])))

(defn can-castle? [board player move]
  (let [[km rm] (castle-moves move)
        kingpos (km :src)
        rookpos (rm :src)
        poses   (poses-between kingpos rookpos)
        not-attacked? (->> (take 2 poses)
                           (mapv (partial attacked? board player))
                           (every? nil?))
        nor-occupied? (->> (mapv board poses)
                           (every? nil?))]
  (and not-attacked? nor-occupied?)))

(defn offsets [piece]
  (match piece
    {:player _  :type \K} [[0 -1] [0 1] [-1 -1] [-1 0] [-1 1] [1 -1] [1 0] [1 1]]
    {:player _  :type \Q} [[0 -1] [0 1] [-1 -1] [-1 0] [-1 1] [1 -1] [1 0] [1 1]]
    {:player _  :type \B} [[-1 -1] [-1 1] [1 -1] [1 1]]
    {:player _  :type \R} [[0 -1] [0 1] [-1 0] [1 0]]
    {:player _  :type \N} [[-2 -1] [-2 1] [1 -2] [-1 -2] [2 -1] [2 1] [1 2] [-1 2]]
    :else                 nil))

(defn pawn-offsets [piece pos]
  (match [piece pos]
    [{:player "w" :type \P} [2 _]] [[2 0] [1 0]]
    [{:player "w" :type \P} [_ _]] [[1 0]]
    [{:player "b" :type \P} [7 _]] [[-2 0] [-1 0]]
    [{:player "b" :type \P} [_ _]] [[-1 0]]
    :else                          nil))

(defn pawn-takes-offsets [piece]
  (match piece
    {:player "w" :type \P} [[1 -1] [1 1]]
    {:player "b" :type \P} [[-1 1] [-1 -1]]
    :else                  nil))

(defn occupied? [board pos]
  (not (nil? (board pos))))

(defn any-occupied? [board poses]
  (some (partial occupied? board) poses))

(defn prv-sqr [player pos]
  (case player
    "w" (add-vec [-1 0] pos)
    "b" (add-vec [1 0] pos)))

(defn get-dsts
  ([board src]
    (get-dsts board :move nil src))
  ([board move-type src]
    (get-dsts board move-type nil src))
  ([board move-type npson-sqr src]
    (let [piece   (board src)
          ptype   (piece :type)
          player  (piece :player)
          steps   (if (#{\K \N \P} ptype) 1 8)
          ;; piece-moves
          pcmvs   (mapcat (partial get-dsts board player src steps)
                          (offsets piece))
          ;; pawn-takes-pins
          ptpin (->> (pawn-takes-offsets piece)
                     (mapcat (partial get-dsts board player src steps)))
          ;; pawn-takes-moves
          ptmvs (->> ptpin
                     (filter #(or (hit-enemy? (board %) player)
                                  (= npson-sqr %))))
          ;; forward-pawn-moves
          fpmvs (when (= move-type :move)
                (->> (pawn-offsets piece src)
                     (mapcat (partial get-dsts board player src steps))
                     (remove #(or (occupied? board %)
                                  (any-occupied? board (poses-between src %))))))]
    (case move-type 
      :move (->> (concat pcmvs fpmvs ptmvs)
                 (remove (partial pinned? board player src)))
      :atak (concat pcmvs ptpin))))
  ([board player src steps offset]
    (let [dst   (add-vec offset src)
          piece (board dst)]
    (cond (zero? steps)                 nil
          (not (in-board? dst))         nil
          (nil? piece)                  (lazy-seq (cons dst (get-dsts board player dst (dec steps) offset)))
          (hit-enemy? piece player)     (lazy-seq (cons dst nil))
          (hit-friendly? piece player)  nil))))

(defn ambiguity-hint [file rank]
  (match [file rank]
    [nil nil] identity
    [_   nil] (fn [[r f]] (= f file))
    [nil   _] (fn [[r f]] (= r rank))
    :else     (fn [[r f]] (= [r f] [rank file]))))

(defn valid-move? [state dst src]
  (let [{:keys [board npson]} state]
  (.contains (get-dsts board :move (pgn->pos npson) src) dst)))

(defn get-srcs
  ([state piece dst]
    (get-srcs state piece dst nil nil))
  ([state piece dst file rank]
    (let [board (state :board)
          srcs  (->> (get-pos board piece)
                     (filterv (ambiguity-hint file rank)))]
    (filterv (partial valid-move? state dst) srcs))))

(defn get-src [& args]
  "Moves with multiple sources ie. amibigious moves
   are marked as invalid aka nil"
  (let [srcs (apply get-srcs args)]
  (when (= 1 (count srcs))
    (first srcs))))

;; move = {:type, :piece, :src, :dst}
(defn make-move [board move]
  (let [{:keys [type src dst piece nul]} move]
  (match type
    "promotion" (assoc board src nil dst (move :piece))
    "enpassant" (assoc board src nil dst (board src) nul nil)
    "castle"    (reduce make-move board (castle-moves move))
    :else       (assoc board src nil dst (board src)))))

(defn pgn->move [state pgn]
  (let [pgn     (->> (remove #{\x \+ \#} pgn)
                     (mapv to-int))
        player  (state :playr)
        board   (state :board)
        castle  (state :casle)
        npson   (->> (state :npson)
                     (mapv to-int))
        prv     (case player
                  "w" dec
                  "b" inc)
        tgl     (case player 
                  "w" upper-case
                  "b" lower-case)]
  (match pgn
    ;; :king-side-castle
    [\O \- \O]        (when (.contains castle (tgl \k))
                        (let [move (case player
                                     "w" {:type "castle" :src [1 5], :dst [1 7]}
                                     "b" {:type "castle" :src [8 5], :dst [8 7]})]
                        (when (can-castle? board player move) move)))
    ;; :queen-side-castle
    [\O \- \O \- \O]  (when (.contains castle (tgl \q))
                        (let [move (case player
                                      "w" {:type "castle" :src [1 5], :dst [1 3]}
                                      "b" {:type "castle" :src [8 5], :dst [8 3]})]
                        (when (can-castle? board player move) move)))
    ;; :pawn-move
    [f r]             (let [f  (char->file f)
                            pc {:player player :type \P}]
                      (when-let [src (get-src state pc [r f] f nil)] 
                        {:src src, :dst [r f]}))
    ;; :pawn-promotion
    [f r \= p]        (let [f  (char->file f)
                            pc {:player player :type \P}]
                      (when-let [src (get-src state pc [r f] f nil)]
                        {:type "promotion",
                         :piece {:player player, :type p},
                         :src   src,
                         :dst   [r f]}))
    ;; :enpassant
    [(f :guard lower-case?) & ([t r] :guard #(= npson %))]
                      (let [[f t] (mapv char->file [f t])
                            pc    {:player player :type \P}]
                      (when-let [src (get-src state pc [r t] f nil)]
                        {:type "enpassant",
                         :src  src,
                         :dst  [r t]
                         :nul  [(prv r) t]}))
    ;; :unambigious-pawn-move
    [(f :guard lower-case?) t r]
                      (let [[f t] (mapv char->file [f t])
                            pc    {:player player :type \P}]
                      (when-let [src (get-src state pc [r t] f nil)]
                        {:src src, :dst [r t]}))
    ;; :unambigious-pawn-promotion
    [(f :guard lower-case?) t r \= p]
                      (let [[f t] (mapv char->file [f t])
                            pc    {:player player :type \P}]
                      (when-let [src (get-src state pc [r t] f nil)]
                        {:type "promotion",
                         :piece {:player player, :type p},
                         :src   src, 
                         :dst   [r t]}))
    ;; :piece-move
    [p f r]           (let [f  (char->file f)
                            pc {:player player, :type p}]
                      (when-let [src (get-src state pc [r f])]
                        {:src src, :dst [r f]}))
    ;; :unambigious-piece-move (hint: file)
    [p (f :guard is-letter?) t r]         
                      (let [[f t] (mapv char->file [f t])
                            pc    {:player player, :type p}]
                      (when-let [src (get-src state pc [r t] f nil)]
                        {:src src, :dst [r t]}))
    ;; :unambigious-piece-move (hint: rank)
    [p (g :guard int?) t r]
                      (let [t  (char->file t)
                            pc {:player player, :type p}]
                      (when-let [src (get-src state pc [r t] nil g)]
                        {:src src, :dst [r t]}))
    ;; :unambigious-piece-move (hint: file, rank)
    [p f g t r]       (let [[f t] (mapv char->file [f t])
                            pc    {:player player, :type p}]
                      (when-let [src (get-src state pc [r t] f g)]
                        {:src src, :dst [r t]}))
    ;; :invalid-pgn
    :else             nil)))

(defn rank-diff [move]
  (let [[r f] (move :src)
        [a b] (move :dst)]
  (Math/abs ^Integer (- r a))))

(defn npson-sqr [state move]
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
    (pos->pgn [(prv r) f]))))

(defn flmvs-cntr [cnt player]
  (case player
    "w" cnt
    "b" (inc cnt)))

(defn pawn? [piece]
  (= (piece :type) \P))

(defn hfmvs-clock [cnt board move]
  (let [{:keys [src dst]} move
        piece (board src)]
  (cond (occupied? board dst) 0 ; capture?
        (pawn? piece)         0 ; pawn-move?
        :else                 (inc cnt))))

(defn king? [piece]
  (= (piece :type) \K))

(defn rook? [piece]
  (= (piece :type) \R))

(def rank first)
(def file second)
(def invalid? empty?)

(defn castle-side? [[km rm :as moves]]
  (when (= (count moves) 2)
    (let [rook-pos (rm :src)]
    (case (file rook-pos)
      1 :queen
      8 :king))))

(defn castle? [move]
  (= (move :type) "castle"))

(defn castle-ability [ability board player move]
  (if (empty? ability)
    ability
;;(else
    (let [frn (case player 
                "w" upper-case
                "b" lower-case)
          kq  (into #{} (mapv frn "kq"))
          q   (into #{} (mapv frn "q"))
          k   (into #{} (mapv frn "k"))
          src (move :src)
          pc  (board src)
          dst (move :dst)
          moves (when (castle? move) (castle-moves move))]
    (cond (= (castle-side? moves) :king)
                          (remove kq ability)
          (= (castle-side? moves) :queen) 
                          (remove kq ability)
          (king? pc)      (remove kq ability) ; king-move?
          (rook? pc)      (match (file src)   ; rook-move?
                            1     (remove q ability)
                            8     (remove k ability)
                            :else ability)
          ;; handling rook captures
          (= dst [1 1])   (remove #{\Q} ability)
          (= dst [1 8])   (remove #{\K} ability)
          (= dst [8 1])   (remove #{\q} ability)
          (= dst [8 8])   (remove #{\k} ability)
          :else           ability))))

(defn play-move [state move]
  (let [{:keys [board
                playr
                casle
                npson
                flmvs
                hfmvs]} state]
  (when-not (invalid? move)
    {:board (make-move board move)
     :playr (toggle-player playr)
     :casle (castle-ability casle board playr move)
     :npson (npson-sqr state move)
     :flmvs (flmvs-cntr flmvs playr)
     :hfmvs (hfmvs-clock hfmvs board move)})))

(defn play-pgn [state pgn]
  (let [move (pgn->move state pgn)]
  (play-move state move)))

(defn play-pgns [state pgns]
  (reduce play-pgn state pgns))

(defn play-pgns-traces [state pgns]
  (reductions play-pgn state pgns))

(defn -main [& args]
  (let [state (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
        pgns (get-pgn-moves "1. e4 e5 2. Nf3 Nf6 3. d4 exd4 4. e5 Nd5 5. Bc4 Bb4+ 6. Bd2 Bxd2+ 7. Qxd2 Nb6 8. Qxd4 Nc6 9. Qd3 O-O 10. O-O d5 11. exd6 Nxc4 12. Qxc4 Qxd6 13. Nc3 Qg6 14. Qd3 Bf5 15. Qe3 Rfe8 16. Nh4 Rxe3 17. Nxg6 Rxc3 18. bxc3 Bxg6 19. Rab1 b6 20. Rb2 Rd8 21. Re1 h6 22. f3 Na5 23. Re2 Rd1+ 24. Kf2 Nc4 25. Rb4 Na5 26. Rb2 c5 27. a4 Nc4 28. Rb3 Rc1 29. Kg3 Rxc2 30. Rxc2 Bxc2 31. Rb5 Bxa4 32. Rb1 a5 33. Ra1 b5 34. Kf4 Bc2 35. Ra2 Bd3 36. Kg3 b4 37. cxb4 axb4 38. Ra1 b3 39. Rd1 Bc2 40. Rd8+ Kh7 41. Rb8 b2 42. Rxb2 Nxb2 43. Kf4 Nc4 44. Kg3 Kg6 45. Kf4 Kf6 46. Kg3 Ke6 47. Kg4 Kd5 48. Kg3 Nd2 49. Kg4 c4 50. Kg3 c3 51. Kg4 Bg6 52. Kg3 c2 53. Kg4 c1=Q 54. Kg3 Nc4 55. Kh3 Qe1 56. g3 f6 57. Kh4 Bf7 58. h3 Qe6 59. g4 g5+ 60. Kg3 Qe5+ 61. Kg2 h5 62. Kg1 h4 63. Kh1 Qe3 64. Kg2 Nd2 65. Kh1 Qf2 66. f4 Nf3 67. f5 Qh2# 0-1")]
  (time (->> (play-pgns state pgns)
             (:board)
             (print-board)))
  (->> (play-pgns-traces state pgns)
       (mapv state->fen)
       (mapv println))))
