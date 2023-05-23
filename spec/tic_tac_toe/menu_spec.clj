(ns tic-tac-toe.menu-spec
  (:require [speclj.core :refer :all]
            [tic-tac-toe.game :refer :all]
            [tic-tac-toe.menu :refer :all]
            [tic-tac-toe.db :as db])
  (:import (tic_tac_toe.menu AiGame ComputerPlayerMenu DifficultyMenu GameModeMenu MainMenu SizeMenu PlayerVComputerGame PvPGame)))

(describe "displays tic-tac-toe game"
  (it "prompts the user again after no input"
    (should-be-a GameModeMenu (next-state (GameModeMenu. 3) nil)))

  (it "delegates the competition type based on user input"
    (let [menu (GameModeMenu. {:dim 3})]
      (should-be-a PvPGame (next-state (GameModeMenu. {:dim 3}) 1))
      (should-be-a ComputerPlayerMenu (next-state (GameModeMenu. {:dim 3}) 2))
      (should-be-a AiGame (next-state (GameModeMenu. {:dim 3}) 3))))

  (it "continues to prompt users for pvp games"
    (let [menu (PvPGame. (new-game))]
      (should-be-a PvPGame (next-state menu 0))))

  (it "ends pvp game when game is finished"
    (let [game (PvPGame. (new-game `(\X \O \X \X \O \X \O \X nil)))]
      (should-be-nil (next-state game 8))))

  (it "progresses ai game to end automatically"
    (let [game  (AiGame. (new-game))
          moves (atom (range 9))
          calls (atom 0)]
      (with-redefs [game/next-move (fn [_ board]
                                     (let [move (first @moves)]
                                       (swap! moves rest)
                                       (swap! calls inc)
                                       (game/move move (game/cur-token board) board)))
                    println (fn [& _])]
        (should-be-nil (next-state game nil))
        (should= 7 @calls))))

  (it "prompts the user to select a token"
    (let [menu (ComputerPlayerMenu. {:dim 3})]
      (should-be-a DifficultyMenu (next-state menu 1))
      (should-be-a DifficultyMenu (next-state menu 2))))

  (it "prompts the user to select a difficulty level when playing computer"
    (let [menu (DifficultyMenu. (new-game))]
      (should-be-a PlayerVComputerGame (next-state menu 1))
      (should-be-a PlayerVComputerGame (next-state menu 2))
      (should-be-a PlayerVComputerGame (next-state menu 3))))

  (it "selects the best move on hard difficulty"
    (let [board (new-game `(\X \X nil nil \O nil nil nil nil))]
      (should= (new-game `(\X \X \O nil \O nil nil nil nil)) (next-move :hard board))))

  (it "selects the worst move on easy difficulty"
    (let [board (new-game `(\X \X nil nil \O nil nil nil nil))]
      (should= (new-game `(\X \X nil nil \O nil nil nil \O)) (next-move :easy board))))

  (it "selects second priority move on medium difficulty"
    (let [board (new-game `(\X \X nil nil \O nil nil nil nil))]
      (should= (new-game `(\X \X nil \O \O nil nil nil nil)) (next-move :med board))))

  (it "prompts the user to select board size"
    (let [menu (SizeMenu.)]
      (should-be-a GameModeMenu (next-state menu 1))
      (should-be-a GameModeMenu (next-state menu 2))))

  (it "game frames render the current state of the board"
    (let [board   (new-game)
          view    (as-string board)]
      (should= view (render (PvPGame. board)))
      (should= view (render (AiGame. board)))))

  (it "difficulty menu prompts user"
    (should= "Select difficulty:\n1) Hard [unbeatable]\n2) Medium\n3) Easy\n"
      (render (DifficultyMenu. (new-game)))))

  (it "computer player menu prompts user"
    (should= "Play as:\n1) X\n2) O\n"
      (render (ComputerPlayerMenu. #{}))))

  (it "game mode menu prompts user"
    (should= "Enter game mode:\n1) Human v. Human\n2) Human v. Computer\n3) Computer v. Computer"
      (render (GameModeMenu. #{}))))

  (it "size menu prompts user"
    (should= "Select board size: \n1) 3x3 \n2) 4x4"
      (render (SizeMenu.))))

  (it "prompts the user to continue a game if there is one in progress"
    (with-redefs [db/open-games? (stub :mock-open-games {:return true})]
      (should-contain "1) Continue" (render (MainMenu.))))))