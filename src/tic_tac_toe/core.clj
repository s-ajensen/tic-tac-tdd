(ns tic-tac-toe.core)

(defn move
  ([x y t board]
    (let [index (+ (* 3 x) y)]
      (if (nil? (nth board index))
        (assoc board index t)
        (throw (IllegalStateException. (str "tile (" x ", " y ") is already occupied!"))))))
  ([n t board]
   (move 0 n t board)))

(defn even-moves? [board]
  (= 1 (rem (count (filter nil? board)) 2)))

(defn cur-token [board]
  (if (even-moves? board) \X \O))

(defn next-token [board]
  (if (even-moves? board) \O \X))

(defn open-moves [board]
  (->> board
    (map-indexed vector)
    (filter #(= (second %) nil))
    (map first)))

(defn winnable-lines [board]
  (let [rows (partition 3 board)
        cols (map #(take-nth 3 %) (take 3 (iterate rest board)))
        diag `(~(take-nth 4 board) ~(take 3 (take-nth 2 (drop 2 board))))]
    (concat rows cols diag)))

(defn win? [board]
  (->> (winnable-lines board)
    (map #(partition-by identity %))
    (filter #(= 1 (count %)))
    (map #(flatten %))
    (some #(not (every? nil? %)))))

(defn tie? [board]
  (and (not (win? board)) (empty? (filter nil? board))))

(declare best-move)

(defn prioritize [board]
  (cond
    (win? board) 1
    (tie? board) 0
    :else (* -1 (best-move board))))

(def prioritize (memoize prioritize))

(defn best-move [board]
  (let [moves (open-moves board)]
    (->> moves
      (map #(move % (cur-token board) board))
      (map prioritize)
      (zipmap moves)
      (sort-by second >)
      (first)
      (first))))

(defn next-move [board]
  (move (best-move board) (cur-token board) board))

(defn print-board [board]
  (doseq [line (partition 3 board)]
    (println (replace {nil "-"} line))))

(defn init-game [roll]
  (if (zero? roll)
    (vec (flatten `(\X ~(repeat 8 nil))))
    (vec (repeat 9 nil))))

(defn main []
  (loop [board (init-game (rand-int 2))]
    (print-board board)
    (cond
      (win? board) (println (str (cur-token board) " wins!"))
      (tie? board) (println "Tie!")
      :else (do
              (println "enter move:")
              (let [input (read-line)]
                (recur (next-move (move (Integer/parseInt input) (cur-token board) board))))))))
