(ns tic-tac-toe.game-spec
  (:require [speclj.core :refer :all]
            [tic-tac-toe.game :refer :all])
  #_(:import (tic_tac_toe.game AiGame ComputerPlayerMenu DifficultyMenu GameModeMenu MainMenu PlayerVComputerGame PvPGame)))

(defn possible-results [boards]
  (loop [unfinished boards finished '()]
    (if (empty? unfinished)
      finished
      (let [responses (for [state unfinished] (next-move nil state))
            next-moves (apply concat (for [state responses] (possible-moves state)))
            states (group-by game-over? next-moves)]
        (recur (states false) (concat finished (states true)))))))

(describe "unwinnable game of tic tac toe"
  (it "adds a move to the game"
    (should= (new-game `(\X ~(repeat 8 nil))) (move 0 \X (new-game))))

  (it "adds multiple moves to the game"
    (should=
      (new-game `(\X \O nil ~(repeat 6 nil)))
      (->> (new-game)
        (move 0 \X)
        (move 1 \O)))
    (should=
      (new-game `(\X nil nil \O ~(repeat 5 nil)))
      (->> (new-game)
        (move 0 \X)
        (move 3 \O))))

  (it "does not allow double-placing a move"
    (should-throw Exception
      (move 0 \O (new-game `(\X ~(repeat 8 nil))))))

  (it "makes a move on an empty board"
    (should-contain \X (next-move nil (new-game))))

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
                 (move 4 \X)
                 (next-move nil)
                 (filter #(= \O %))
                 (count)))
    (should= 2 (->> (new-game)
                 (move 4 \X)
                 (move 1 \O)
                 (next-move nil)
                 (filter #(= \X %))
                 (count))))

  (it "blocks a player from taking a win"
    (should=
      (new-game `(\X \O nil \X nil nil \O nil nil))
      (next-move nil (new-game `(\X \O nil \X nil nil nil nil nil)))))

  (it "takes a win when available"
    (should=
      (new-game `(\X nil \O \X nil nil \X \O nil))
      (next-move nil (new-game `(\X nil \O nil nil nil \X \O nil)))))

  (it "prefers taking a win to blocking the player"
    (should=
      (new-game `(\X \X \X \O nil \O nil nil nil))
      (next-move nil (new-game `(\X nil \X \O nil \O nil nil nil)))))

  (it "identifies the winner of a game"
    (should-be-nil (winner (new-game)))
    (should= \X (winner (new-game `(\X \X \X \O \O \X \X \O \O))))
    (should= \O (winner (new-game `(\X \X \O \X nil \X \O \O \O))))
    (should= :tie (winner (new-game `(\X \O \X \X \O \X \O \X \O)))))

  (it "identifies when a game is over"
    (should-not (game-over? (new-game)))
    (should (new-game `(\X \X \X \O \O \X nil nil \O)))
    (should (new-game `(\X \X \X \O \O \X \O \X \O)))
    (should (new-game `(\X \O \X \X \O \X \O \X \O))))

  (it "always responds with the correct move when playing first"
    (let [winners (map winner (possible-results (possible-moves (new-game))))]
      (should-not-contain \O winners)))

  (it "always responds with the correct move when playing second"
    (let [winners (map winner (possible-results (list (new-game))))]
      (should-not-contain \X winners)))

  (it "determines win state by row in 4x4 games"
    (should (win? (new-game (repeat 4 \X) (repeat 12 nil))))
    (should (win? (new-game (repeat 4 nil) (repeat 4 \X) (repeat 8 nil))))
    (should (win? (new-game (repeat 8 nil) (repeat 4 \X) (repeat 4 nil))))
    (should (win? (new-game (repeat 12 nil) (repeat 4 \X)))))

  (it "determines win state by column in 4x4 games"
    (should (win? (new-game `(\X nil nil nil \X nil nil nil \X nil nil nil \X nil nil nil))))
    (should (win? (new-game `(nil \X nil nil nil \X nil nil nil \X nil nil nil \X nil nil))))
    (should (win? (new-game `(nil nil \X nil nil nil \X nil nil nil \X nil nil nil \X nil))))
    (should (win? (new-game `(nil nil nil \X nil nil nil \X nil nil nil \X nil nil nil \X)))))

  (it "determines win state by diagonal in 4x4 games"
    (should (win? (new-game `(\X nil nil nil nil \X nil nil nil nil \X nil nil nil nil \X))))
    (should (win? (new-game `(nil nil nil \X nil nil \X nil nil \X nil nil \X nil nil nil))))))

