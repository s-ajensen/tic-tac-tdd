(ns tic-tac-toe.menu
  (:require [tic-tac-toe.game :refer :all]))

(defprotocol MenuLink
  (render [this])
  (next-state [this x]))

(defn as-string [board]
  (str
    (->> (partition (length board) board)
      (map #(apply str (replace {nil "-"} %)))
      (clojure.string/join "\n"))
    "\n"))

(deftype PvPGame [board]
  MenuLink
  (render [this]
    (as-string board))
  (next-state [_ x]
    (let [new-board (move x (cur-token board) board)]
      (if (game-over? new-board)
        (print (render (PvPGame. new-board)))
        (PvPGame. new-board)))))

(deftype PlayerVComputerGame [board difficulty]
  MenuLink
  (render [this]
    (as-string board))
  (next-state [_ x]
    (let [new-board (move x (cur-token board) board)]
      (if (game-over? new-board)
        (println (render (PlayerVComputerGame. new-board difficulty)))
        (PlayerVComputerGame. (next-move difficulty new-board) difficulty)))))

(deftype AiGame [board]
  MenuLink
  (render [this]
    (as-string board))
  (next-state [this x]
    (loop [board board]
      (println (as-string board))
      (if (not (game-over? board))
        (recur (next-move nil board))))))

(deftype DifficultyMenu [board]
  MenuLink
  (render [this]
    (str "Select difficulty:\n"
      "1) Hard [unbeatable]\n"
      "2) Medium\n"
      "3) Easy\n"))
  (next-state [_ x]
    (let [difficulties {1 :hard 2 :med 3 :easy}]
      (PlayerVComputerGame. board (difficulties x)))))

(defn empty-game [dim]
  (vec (flatten (repeat (* dim dim) nil))))

(deftype ComputerPlayerMenu [opts]
  MenuLink
  (render [this]
    (str "Play as:\n" "1) X\n" "2) O\n"))
  (next-state [_ x]
    (cond
      (= 1 x) (DifficultyMenu. (empty-game (opts :dim)))
      (= 2 x) (DifficultyMenu. (new-game \X (repeat (dec (opts :dim)) nil))))))

(def game-types   '("Human v. Human"
                     "Human v. Computer"
                     "Computer v. Computer"))

(deftype GameModeMenu [opts]
  MenuLink
  (render [this]
    (str "Enter game mode:\n"
      (->> (map-indexed list game-types)
        (map #(str (inc (first %)) ") " (last %)))
        (clojure.string/join "\n"))))
  (next-state [_ x]
    (cond
      (= 1 x) (PvPGame. (empty-game (opts :dim)))
      (= 2 x) (ComputerPlayerMenu. opts)
      (= 3 x) (AiGame. (empty-game (opts :dim)))
      :else (GameModeMenu. opts))))

(deftype MainMenu []
  MenuLink
  (render [this]
    (str "Select board size: \n" "1) 3x3 \n" "2) 4x4"))
  (next-state [_ x]
    (cond
      (= 1 x) (GameModeMenu. {:dim 3})
      (= 2 x) (GameModeMenu. {:dim 4}))))