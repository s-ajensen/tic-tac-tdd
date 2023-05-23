(ns tic-tac-toe.core-spec
  (:require [speclj.core :refer :all]
            [tic-tac-toe.core :refer :all]
            [tic-tac-toe.menu :refer :all]))

(deftype MockState [board]
  MenuLink
  (render [this] "state")
  (next-state [this x]
    (if (nil? board)
      nil
      (MockState. nil))))

(describe "tic tac toe core loop control"
  (it "exits on nil state"
    (should= "Game over!"
      (with-out-str (game-loop nil))))

  (with-stubs)
  (redefs-around [read-line (stub :mock-read-line {:return "1"})])

  (it "renders the current state of the board"
    (should-contain "state" (with-out-str (game-loop (MockState. [])))))

  (it "flushes the output stream before reading line"
    (with-redefs [flush (stub :mock-flush)]
      (with-out-str (game-loop (MockState. [])))
      (should-have-invoked :mock-read-line {:times 2})))

  (it "transfers to next state on loop"
    (with-out-str (game-loop (MockState. [])))
    (should-have-invoked :mock-read-line {:times 2})))