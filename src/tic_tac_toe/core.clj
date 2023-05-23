(ns tic-tac-toe.core
  (:require [tic-tac-toe.menu :refer :all])
  (:import (tic_tac_toe.menu MainMenu)))

(defn game-loop [state]
  (if (nil? state)
    (print "Game over!")
    (do
      (println (str "\n" (render state)))
      (flush)
      (let [input (read-line)]
        (game-loop (next-state state (Integer/parseInt input)))))))

(defn main []
  (game-loop (MainMenu. nil)))

#_(main)