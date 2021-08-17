(ns clj-chess.core-test
  (:use [clojure.test]
        [clj-chess.notation]
        [clj-chess.core])
  (:require [clojure.string :refer [split]]))

(deftest in-check?-test
  (testing "Black king is in check"
    (is (in-check? (parse-board "4r2k/p7/1p4Q1/4P2R/1P6/P1rP3K/7P/8") "b")))
  (testing "White king is not in check"
    (is (nil? (in-check? (parse-board "4r2k/p7/1p4Q1/4P2R/1P6/P1rP3K/7P/8") "w")))))

(deftest pinned?-test
  (testing "c4 is pinned"
    (is (pinned? (parse-board "8/4k3/8/8/1KP3r1/8/8/8") "w" [4 3] [5 3]))))

(deftest play-moves-test
  (testing "Playing moves from a random game to produce desired board fen"
    (is (=  (let [state (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                  pgns (split "e4 e6 f4 d5 e5 Bc5 Nf3 Ne7 d4 Bb6 Be2 c5 Be3 Nf5 Bf2 cxd4 Nxd4 Bxd4 Bxd4 O-O Bf2 Qa5+ Nc3 Nc6 a3 d4 b4 Qd8 Ne4 b6 Bd3 Bb7 Qh5 Nce7 Ng5 h6 Ne4 Ng6 g3 Rc8 O-O Bd5 Rae1 Qc7 Re2 Bc4 Kg2 Nge7 g4 Ne3+ Bxe3 Bxd3 cxd3 dxe3 Rxe3 Qc2+ Rf2 Qc3 Nxc3 Rxc3 f5 Nd5 Rg3 exf5 gxf5 Re8 Qxh6 Ne3+ Kh3 Nxf5 Rxf5 g6 Rxg6+ fxg6 Qxg6+ Kh8 Rh5#" #" ")]
            (->> (play-moves state pgns)
                 (:board)
                 (unparse-board)))
        "4r2k/p7/1p4Q1/4P2R/1P6/P1rP3K/7P/8")))
  (testing ":unambigious-piece-move (hint: rank)"
    (is (=  (let [state (fen->state "7Q/5Q2/8/5Q1Q/1KP5/8/k7/7Q w - - 0 1")
                  pgns ["Q1h2"]]
            (->> (play-moves state pgns)
                 (:board)
                 (unparse-board)))
        "7Q/5Q2/8/5Q1Q/1KP5/8/k6Q/8")))
  (testing ":unambigious-piece-move (hint: file, rank)"
    (is (=  (let [state (fen->state "7Q/5Q2/8/5Q1Q/1KP5/8/k7/7Q w - - 0 1")
                  pgns ["Qf5h7"]]
            (->> (play-moves state pgns)
                 (:board)
                 (unparse-board)))
        "7Q/5Q1Q/8/7Q/1KP5/8/k7/7Q"))))

(run-tests)
