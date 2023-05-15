(ns tic-tac-toe.game)

(defn move
  ([n t board]
   (if (nil? (nth board n))
     (assoc board n t)
     (throw (ex-info
              (str "tile " n "is already occupied!")
              {:data (str board)})))))

(defn new-game
  ([] (vec (repeat 9 nil)))
  ([& moves] (vec (flatten moves))))

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

(defn length [board] (int (Math/sqrt (count board))))

(defn reverse-diag [board]
  (flatten (reverse (partition (length board) board))))

(defn winnable-lines [board]
  (let [length (length board)]
    (let [rows (partition length board)
          cols (map #(take-nth length %) (take length (iterate rest board)))
          diag (list (take-nth (inc length) board) (take-nth (inc length) (reverse-diag board)))]
      (concat rows cols diag))))

(defn win? [board]
  (->> (winnable-lines board)
    (map #(partition-by identity %))
    (filter #(= 1 (count %)))
    (map #(flatten %))
    (some #(not (every? nil? %)))))

(defn tie? [board]
  (and (not (win? board)) (empty? (filter nil? board))))

(defn game-over? [board]
  (or (win? board) (empty? (filter nil? board))))

(defn winner [board]
  (cond
    (tie? board) :tie
    (not (win? board)) nil
    :else (next-token board)))

(declare best-move)

(defn- prioritize [board depth]
  (cond
    (win? board) (- 10 depth)
    (tie? board) 0
    :else (let [best (best-move board)]
            (* -1 (prioritize
                    (move best (cur-token board) board)
                    (inc depth))))))

(def prioritize (memoize prioritize))

(defn max-move [moves]
  (->> moves
    (sort-by second >)
    (first)
    (first)))

(defn possible-moves [board]
  (->> (open-moves board)
    (map #(move % (cur-token board) board))))

(defn sort-moves [board]
  (->> (possible-moves board)
    (map #(prioritize % 0))
    (zipmap (open-moves board))
    (sort-by second >)))

(defn best-move [board]
  (->> (sort-moves board)
    (max-move)))

(defmulti next-move
  (fn [difficulty board] difficulty))

(defmethod next-move :default [_ board]
  (move (best-move board) (cur-token board) board))

(defmethod next-move :easy [_ board]
  (let [chosen-move (->> (sort-moves board)
                      (last)
                      (first))]
    (move chosen-move (cur-token board) board)))

(defmethod next-move :med [_ board]
  (let [chosen-move (->> (sort-moves board)
                      (cycle)
                      (second)
                      (first))]
    (move chosen-move (cur-token board) board)))