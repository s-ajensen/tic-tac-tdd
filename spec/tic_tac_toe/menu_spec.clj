(ns tic-tac-toe.menu-spec
  (:require [speclj.core :refer :all]
            [tic-tac-toe.game :refer :all]
            [tic-tac-toe.menu :refer :all]
            [tic-tac-toe.db :as db])
  (:import (tic_tac_toe.menu AiGame ComputerPlayerMenu ContinueGameMenu DifficultyMenu GameModeMenu MainMenu ReplayMenu SizeMenu PlayerVComputerGame PvPGame)))

(def ^:dynamic empty-games-menu :empty-games-menu)
(def ^:dynamic game-in-progress-menu :game-in-progress-menu)
(def ^:dynamic continue-menu :populated-continue-menu)
(def open-game (new-game))

(describe "tic tac toe console menu"
  (it "prompts the user again after no input"
    (should-be-a GameModeMenu (next-state (GameModeMenu. 3) nil)))

  (it "delegates the competition type based on user input"
    (let [menu (GameModeMenu. {:dim 3})]
      (should-be-a PvPGame (next-state menu 1))
      (should-be-a ComputerPlayerMenu (next-state (GameModeMenu. {:dim 3}) 2))
      (should-be-a AiGame (next-state menu 3))))

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
      (with-redefs [next-move (fn [_ board]
                                     (let [first-move (first @moves)]
                                       (swap! moves rest)
                                       (swap! calls inc)
                                       (move first-move (cur-token board) board)))
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

  (describe "main menu"
    (with-stubs)
    (around [it] (binding [empty-games-menu (MainMenu. nil)
                           game-in-progress-menu (MainMenu. 1)] (it)))

    (it "prompts user to continue a game if there is one in progress"
      (should-contain "0) Continue" (render game-in-progress-menu))
      (should-not-contain "Continue" (render empty-games-menu)))

    (it "prompts user to start or replay a game"
      (should-contain "1) New Game\n2) Replay Game" (render empty-games-menu))
      (should-contain "0) Continue\n1) New Game\n2) Replay Game" (render game-in-progress-menu)))

    (it "sends user to game in progress"
      (should-be-a ContinueGameMenu (next-state game-in-progress-menu 0)))

    (it "starts a new game"
      (should-be-a SizeMenu (next-state empty-games-menu 1)))

    (it "send user to replay menu"
      (should-be-a ReplayMenu (next-state empty-games-menu 2))))

  (describe "continue game menu"
    (with-stubs)
    (around [it] (binding [continue-menu (ContinueGameMenu. open-game)] (it)))

    (it "displays open game"
      (should= (as-string open-game) (render continue-menu)))))