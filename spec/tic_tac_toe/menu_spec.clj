(ns tic-tac-toe.menu-spec
  (:require [speclj.core :refer :all]
            [tic-tac-toe.game :refer :all]
            [tic-tac-toe.menu :refer :all])
  (:import (tic_tac_toe.menu AiGame ComputerPlayerMenu DifficultyMenu GameModeMenu MainMenu PlayerVComputerGame PvPGame)))

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
    (let [game (AiGame. (new-game))]
      (should-be-nil (next-state game nil))))

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
    (let [menu (MainMenu.)]
      (should-be-a GameModeMenu (next-state menu 1))
      (should-be-a GameModeMenu (next-state menu 2)))))