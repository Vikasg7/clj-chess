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
    (is (pinned? (parse-board "8/4k3/8/8/1KP3r1/8/8/8") "w" {:src [4 3] :dst [5 3]}))))

(deftest npson-srq-test
  (testing "f6 is npson target for white"
    (is (= (-> (fen->state "r1bqkbnr/pp2pppp/2np4/6P1/2pPP3/2N5/PPP2P1P/R1BQKBNR b KQkq - 0 5")
               (npson-sqr {:src [7 6] :dst [5 6]}))
           "f6")))
  (testing "d3 is npson target for black"
    (is (= (-> (fen->state "rnbqkbnr/pp1ppppp/8/8/2p1P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 0 3")
               (npson-sqr {:src [2 4] :dst [4 4]}))
           "d3"))))

(deftest play-pgns-test
  (testing "Playing moves from a random game to produce desired board fen"
    (is (=  (let [state (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                  pgns (split "e4 e6 f4 d5 e5 Bc5 Nf3 Ne7 d4 Bb6 Be2 c5 Be3 Nf5 Bf2 cxd4 Nxd4 Bxd4 Bxd4 O-O Bf2 Qa5+ Nc3 Nc6 a3 d4 b4 Qd8 Ne4 b6 Bd3 Bb7 Qh5 Nce7 Ng5 h6 Ne4 Ng6 g3 Rc8 O-O Bd5 Rae1 Qc7 Re2 Bc4 Kg2 Nge7 g4 Ne3+ Bxe3 Bxd3 cxd3 dxe3 Rxe3 Qc2+ Rf2 Qc3 Nxc3 Rxc3 f5 Nd5 Rg3 exf5 gxf5 Re8 Qxh6 Ne3+ Kh3 Nxf5 Rxf5 g6 Rxg6+ fxg6 Qxg6+ Kh8 Rh5#" #" ")]
            (->> (play-pgns state pgns)
                 (state->fen)))
            "4r2k/p7/1p4Q1/4P2R/1P6/P1rP3K/7P/8 b - - 2 39")))
  (testing ":unambigious-piece-move (hint: rank)"
    (is (=  (let [state (fen->state "7Q/5Q2/8/5Q1Q/1KP5/8/k7/7Q w - - 0 1")
                  pgns ["Q1h2"]]
            (->> (play-pgns state pgns)
                 (state->fen)))
            "7Q/5Q2/8/5Q1Q/1KP5/8/k6Q/8 b - - 1 1")))
  (testing ":unambigious-piece-move (hint: file, rank)"
    (is (=  (let [state (fen->state "7Q/5Q2/8/5Q1Q/1KP5/8/k7/7Q w - - 0 1")
                  pgns ["Qf5h7"]]
            (->> (play-pgns state pgns)
                 (state->fen)))
            "7Q/5Q1Q/8/7Q/1KP5/8/k7/7Q b - - 1 1")))
  (testing ":unambigious-pawn-promotion"
    (is (=  (let [state (fen->state "5b2/4P1P1/1k6/8/8/3K4/8/8 w - - 0 1")
                  pgns ["gxf8=Q"]]
            (->> (play-pgns state pgns)
                 (state->fen)))
            "5Q2/4P3/1k6/8/8/3K4/8/8 b - - 0 1")))
  (testing "Testing another random game for pgn moves"
    (is (=  (let [state (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                  pgns (get-pgn-moves "1. e4 e5 2. Nf3 Nf6 3. d4 exd4 4. e5 Nd5 5. Bc4 Bb4+ 6. Bd2 Bxd2+ 7. Qxd2 Nb6 8. Qxd4 Nc6 9. Qd3 O-O 10. O-O d5 11. exd6 Nxc4 12. Qxc4 Qxd6 13. Nc3 Qg6 14. Qd3 Bf5 15. Qe3 Rfe8 16. Nh4 Rxe3 17. Nxg6 Rxc3 18. bxc3 Bxg6 19. Rab1 b6 20. Rb2 Rd8 21. Re1 h6 22. f3 Na5 23. Re2 Rd1+ 24. Kf2 Nc4 25. Rb4 Na5 26. Rb2 c5 27. a4 Nc4 28. Rb3 Rc1 29. Kg3 Rxc2 30. Rxc2 Bxc2 31. Rb5 Bxa4 32. Rb1 a5 33. Ra1 b5 34. Kf4 Bc2 35. Ra2 Bd3 36. Kg3 b4 37. cxb4 axb4 38. Ra1 b3 39. Rd1 Bc2 40. Rd8+ Kh7 41. Rb8 b2 42. Rxb2 Nxb2 43. Kf4 Nc4 44. Kg3 Kg6 45. Kf4 Kf6 46. Kg3 Ke6 47. Kg4 Kd5 48. Kg3 Nd2 49. Kg4 c4 50. Kg3 c3 51. Kg4 Bg6 52. Kg3 c2 53. Kg4 c1=Q 54. Kg3 Nc4 55. Kh3 Qe1 56. g3 f6 57. Kh4 Bf7 58. h3 Qe6 59. g4 g5+ 60. Kg3 Qe5+ 61. Kg2 h5 62. Kg1 h4 63. Kh1 Qe3 64. Kg2 Nd2 65. Kh1 Qf2 66. f4 Nf3 67. f5 Qh2# 0-1")]
            (->> (play-pgns state pgns)
                 (state->fen)))
            "8/5b2/5p2/3k1Pp1/6Pp/5n1P/7q/7K w - - 1 68")))
  (testing "Testing another random game for enpassont move"
    (is (=  (let [state (fen->state "r1bqkbnr/ppppp1pp/2n5/4Pp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3")
                  pgns  ["exf6"]]
            (->> (play-pgns state pgns)
                 (state->fen)))
            "r1bqkbnr/ppppp1pp/2n2P2/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3")))
  (testing "Testing effect of rook captures on castling"
    (is (=  (let [state (fen->state "r1bqkb1r/pppppppp/1N4N1/8/8/1n4n1/PPPPPPPP/R1BQKB1R w KQkq - 0 1")
                  pgns (get-pgn-moves "1. Nxa8 Nxa1 2. Nxh8 Nxh1")]
            (->> (play-pgns state pgns)
                 (state->fen)))
            "N1bqkb1N/pppppppp/8/8/8/8/PPPPPPPP/n1BQKB1n w - - 0 3"))))

(deftest castle-moves-test
  (testing "All Castling moves"
    (is (let [king-move {:type "castle" :src [1 5] :dst [1 7]}]
        (= (castle-moves king-move)
           [{:src [1 5] :dst [1 7]} 
            {:src [1 8] :dst [1 6]}])))
    (is (let [king-move {:type "castle" :src [1 5] :dst [1 3]}]
        (= (castle-moves king-move)
           [{:src [1 5] :dst [1 3]}
            {:src [1 1] :dst [1 4]}])))
    (is (let [king-move {:type "castle" :src [8 5] :dst [8 7]}]
        (= (castle-moves king-move)
           [{:src [8 5] :dst [8 7]}
            {:src [8 8] :dst [8 6]}])))
    (is (let [king-move {:type "castle" :src [8 5] :dst [8 3]}]
        (= (castle-moves king-move)
           [{:src [8 5] :dst [8 3]}
            {:src [8 1] :dst [8 4]}])))))

(run-tests)
