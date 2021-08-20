(ns clj-chess.notation-test
  (:require [clojure.string :refer [join]])
  (:use [clojure.test]
        [clj-chess.notation]))

(deftest fen->state-test
  (testing "Default fen"
    (is (= (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
           {:board (parse-board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
            :playr "w"
            :casle [\K \Q \k \q]
            :npson nil
            :hfmvs 0
            :flmvs 1})))
  (testing "No Castling fen"
    (is (= (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")
           {:board (parse-board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
            :playr "w"
            :casle []
            :npson nil
            :hfmvs 0
            :flmvs 1}))))

(deftest print-board-test
  (testing "Default board"
    (is (= (with-out-str (print-board (parse-board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")))
           (join "\r\n " ["[\"♖|♘|♗|♕|♔|♗|♘|♖\""
                           "\"♙|♙|♙|♙|♙|♙|♙|♙\""
                           "\"　|　|　|　|　|　|　|　\""
                           "\"　|　|　|　|　|　|　|　\""
                           "\"　|　|　|　|　|　|　|　\""
                           "\"　|　|　|　|　|　|　|　\""
                           "\"♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎\""
                           "\"♜|♞|♝|♛|♚|♝|♞|♜\"]\r\n"])))))

(deftest state->fen-test
  (testing "Default state"
    (is (= (state->fen {:board (parse-board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
                        :playr "w"
                        :casle [\K \Q \k \q]
                        :npson nil
                        :hfmvs 0
                        :flmvs 1})
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")))
  (testing "Random state"
    (is (= (state->fen {:board (parse-board "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR ")
                        :playr "w"
                        :casle [\K \Q \k \q]
                        :npson "c6"
                        :hfmvs 0
                        :flmvs 2})
        "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"))))

(deftest get-pgn-moves-test
  (testing "Parse all pgn moves from unannotated pgn string"
    (is (= (get-pgn-moves "1. e4 e5 2. Nf3 Nf6 3. d4 exd4 4. e5 Nd5 5. Bc4 Bb4+ 6. Bd2 Bxd2+ 7. Qxd2 Nb6 8. Qxd4 Nc6 9. Qd3 O-O 10. O-O d5 11. exd6 Nxc4 12. Qxc4 Qxd6 13. Nc3 Qg6 14. Qd3 Bf5 15. Qe3 Rfe8 16. Nh4 Rxe3 17. Nxg6 Rxc3 18. bxc3 Bxg6 19. Rab1 b6 20. Rb2 Rd8 21. Re1 h6 22. f3 Na5 23. Re2 Rd1+ 24. Kf2 Nc4 25. Rb4 Na5 26. Rb2 c5 27. a4 Nc4 28. Rb3 Rc1 29. Kg3 Rxc2 30. Rxc2 Bxc2 31. Rb5 Bxa4 32. Rb1 a5 33. Ra1 b5 34. Kf4 Bc2 35. Ra2 Bd3 36. Kg3 b4 37. cxb4 axb4 38. Ra1 b3 39. Rd1 Bc2 40. Rd8+ Kh7 41. Rb8 b2 42. Rxb2 Nxb2 43. Kf4 Nc4 44. Kg3 Kg6 45. Kf4 Kf6 46. Kg3 Ke6 47. Kg4 Kd5 48. Kg3 Nd2 49. Kg4 c4 50. Kg3 c3 51. Kg4 Bg6 52. Kg3 c2 53. Kg4 c1=Q 54. Kg3 Nc4 55. Kh3 Qe1 56. g3 f6 57. Kh4 Bf7 58. h3 Qe6 59. g4 g5+ 60. Kg3 Qe5+ 61. Kg2 h5 62. Kg1 h4 63. Kh1 Qe3 64. Kg2 Nd2 65. Kh1 Qf2 66. f4 Nf3 67. f5 Qh2# 0-1")
           ["e4" "e5" "Nf3" "Nf6" "d4" "exd4" "e5" "Nd5" "Bc4" "Bb4+" "Bd2" "Bxd2+" "Qxd2" "Nb6" "Qxd4" "Nc6" "Qd3" "O-O" "O-O" "d5" "exd6" "Nxc4" "Qxc4" "Qxd6" "Nc3" "Qg6" "Qd3" "Bf5" "Qe3" "Rfe8" "Nh4" "Rxe3" "Nxg6" "Rxc3" "bxc3" "Bxg6" "Rab1" "b6" "Rb2" "Rd8" "Re1" "h6" "f3" "Na5" "Re2" "Rd1+" "Kf2" "Nc4" "Rb4" "Na5" "Rb2" "c5" "a4" "Nc4" "Rb3" "Rc1" "Kg3" "Rxc2" "Rxc2" "Bxc2" "Rb5" "Bxa4" "Rb1" "a5" "Ra1" "b5" "Kf4" "Bc2" "Ra2" "Bd3" "Kg3" "b4" "cxb4" "axb4" "Ra1" "b3" "Rd1" "Bc2" "Rd8+" "Kh7" "Rb8" "b2" "Rxb2" "Nxb2" "Kf4" "Nc4" "Kg3" "Kg6" "Kf4" "Kf6" "Kg3" "Ke6" "Kg4" "Kd5" "Kg3" "Nd2" "Kg4" "c4" "Kg3" "c3" "Kg4" "Bg6" "Kg3" "c2" "Kg4" "c1=Q" "Kg3" "Nc4" "Kh3" "Qe1" "g3" "f6" "Kh4" "Bf7" "h3" "Qe6" "g4" "g5+" "Kg3" "Qe5+" "Kg2" "h5" "Kg1" "h4" "Kh1" "Qe3" "Kg2" "Nd2" "Kh1" "Qf2" "f4" "Nf3" "f5" "Qh2#"]))))

(run-tests)
