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
            :casle nil
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

(run-tests)
