(ns tic-tac-toe.core-spec
  (:require [speclj.core :refer :all]
            [tic-tac-toe.core :refer :all]))

(defn new-game
  ([] (vec (repeat 9 nil)))
  ([& moves] (vec (flatten moves))))

(describe "unwinnable game of tic tac toe"
  (it "adds a move to the game"
    (should= (new-game `(\X ~(repeat 8 nil))) (move 0 0 \X (new-game))))

  (it "adds multiple moves to the game"
    (should=
      (new-game `(\X \O nil ~(repeat 6 nil)))
      (->> (new-game)
        (move 0 0 \X)
        (move 0 1 \O)))
    (should=
      (new-game `(\X nil nil \O ~(repeat 5 nil)))
      (->> (new-game)
        (move 0 0 \X)
        (move 1 0 \O))))

  (it "does not allow double-placing a move"
    (should-throw IllegalStateException "tile (0, 0) is already occupied!"
      (move 0 0 \O (new-game `(\X ~(repeat 8 nil))))))

  (it "makes a move on an empty board"
    (should-contain \X (next-move (new-game))))

  (it "determines win state by row"
    (should (win? (new-game `(\X \X \X ~(repeat 6 nil)))))
    (should (win? (new-game `(~(repeat 3 nil) \X \X \X ~(repeat 3 nil)))))
    (should (win? (new-game `(~(repeat 6 nil) \X \X \X ))))
    (should-not (win? (new-game `(\X ~(repeat 8 nil))))))

  (it "determines win state by column"
    (should (win? (new-game `(\X nil nil \X nil nil \X nil nil))))
    (should (win? (new-game `(nil \X nil nil \X nil nil \X nil))))
    (should (win? (new-game `(nil nil \X nil nil \X nil nil \X)))))

  (it "determines win state by diagonal"
    (should (win? (new-game `(\X nil nil nil \X nil nil nil \X))))
    (should (win? (new-game `(nil nil \X nil \X nil \X nil nil)))))

  (it "plays the alternate token as the player"
    (should= 1 (->> (new-game)
                 (move 1 1 \X)
                 (next-move)
                 (filter #(= \O %))
                 (count)))
    (should= 2 (->> (new-game)
                 (move 1 1 \X)
                 (move 0 1 \O)
                 (next-move)
                 (filter #(= \X %))
                 (count))))

  (it "blocks a player from taking a win"
    (should=
      (new-game `(\X \O nil \X nil nil \O nil nil))
      (next-move (new-game `(\X \O nil \X nil nil nil nil nil)))))

  (it "takes a win when available"
    (should=
      (new-game `(\X nil \O \X nil nil \X \O nil))
      (next-move (new-game `(\X nil \O nil nil nil \X \O nil)))))

  (it "prefers taking a win to blocking the player"
    (should=
      (new-game `(\X \X \X \O nil \O nil nil nil))
      (next-move (new-game `(\X nil \X \O nil \O nil nil nil)))))

  (it "should not allow the player to fork their next move"
    (should=
      (new-game `(nil nil \O nil \O \X nil \X nil))
      (next-move (new-game `(nil nil nil nil \O \X nil \X nil))))
    (should=
      (new-game `(\O nil nil nil \X nil nil \O \X))
      (next-move (new-game `(\O nil nil nil \X nil nil nil \X))))
    (should=
      (new-game `(\X nil nil nil \O \O nil nil \X))
      (next-move (new-game `(\X nil nil nil \O nil nil nil \X)))))

  (it "plays first or second based off of a roll"
    (should= (repeat 9 nil) (init-game 1))
    (should= '(\X nil nil nil nil nil nil nil nil) (init-game 0))))