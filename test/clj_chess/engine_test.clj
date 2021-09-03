(ns clj-chess.engine-test
  (:use [clojure.test]
        [clj-chess.notation]
        [clj-chess.core]
        [clj-chess.engine])
  (:require [clojure.string :refer [split]]))

(deftest get-moves-test
  (testing "King moves - 1"
    (is (= (count (get-moves (fen->state "8/k7/8/3p4/8/4K3/8/8  w - - 0 1") [3 5]))
           7)))
  (testing "King moves - 2"
    (is (= (count (get-moves (fen->state "8/k2p4/8/8/4K3/8/8/8 w - - 0 1") [4 5]))
           8)))
  (testing "Pawn promotion"
    (is (= (count (get-moves (fen->state "8/4P3/8/3k1K2/8/8/8/8 w - - 0 1") [7 5]))
           4)))
  (testing "Pawn promotion with capture"
    (is (= (count (get-moves (fen->state "5n2/4P3/8/3k1K2/8/8/8/8 w - - 0 1") [7 5]))
           8)))
  (testing "Enpassant moves"
    (is (= (count (get-moves (fen->state "r1bqkbnr/ppppp1pp/2n5/4Pp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3") [5 5]))
           2))))

(deftest perft-test
  (testing "Default fen"
    (is (= (let [state (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")]
           (mapv #(perft state %) [1 2 3]))
           [20 400 8902])))
  (testing "Kiwipete"
    (is (= (let [state (fen->state "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")]
           (mapv #(perft state %) [1 2 3]))
           [48 2039 97862])))
  (testing "Position 3"
    (is (= (let [state (fen->state "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")]
           (mapv #(perft state %) [1 2 3 4]))
           [14 191 2812 43238])))
  (testing "Position 4"
    (is (= (let [state (fen->state "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")]
           (mapv #(perft state %) [1 2 3]))
           [6 264 9467])))
  (testing "Position 5"
    (is (= (let [state (fen->state "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")]
           (mapv #(perft state %) [1 2 3]))
           [44 1486 62379]))))

(run-tests)
