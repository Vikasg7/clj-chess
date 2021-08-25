(ns clj-chess.engine-test
  (:use [clojure.test]
        [clj-chess.notation]
        [clj-chess.core]
        [clj-chess.engine])
  (:require [clojure.string :refer [split]]))

(deftest get-moves-test
  (testing "King moves - 1"
    (is (= (get-moves (fen->state "8/k7/8/3p4/8/4K3/8/8  w - - 0 1") [3 5])
           (mapv basic-move (repeat [3 5]) [[3 4] [3 6] [2 4] [2 5] [2 6] [4 4] [4 6]]))))
  (testing "King moves - 2"
    (is (= (get-moves (fen->state "8/k2p4/8/8/4K3/8/8/8 w - - 0 1") [4 5])
           (mapv basic-move (repeat [4 5]) [[4 4] [4 6] [3 4] [3 5] [3 6] [5 4] [5 5] [5 6]]))))
  (testing "Pawn promotion"
    (is (= (count (get-moves (fen->state "8/4P3/8/3k1K2/8/8/8/8 w - - 0 1") [7 5]))
           4)))
  (testing "Pawn promotion with capture"
    (is (= (count (get-moves (fen->state "5n2/4P3/8/3k1K2/8/8/8/8 w - - 0 1") [7 5]))
           8)))
  (testing "Enpassant moves"
    (is (= (count (get-moves (fen->state "r1bqkbnr/ppppp1pp/2n5/4Pp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3") [5 5]))
           2))))

(run-tests)
