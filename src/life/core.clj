(ns life.core
  (:require clojure.pprint))

(defn empty-board
  "创建一个空的"
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  ""
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))


(def glider (populate (empty-board 6 6)
                      #{[2 0] [2 1] [2 2] [1 2] [0 1]}))

(defn neighbours
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn count-neighbours
  ""
  [board loc]
  (count (filter #(get-in board %) (neighbours loc))))

(defn indexed-step
  ""
  [board]
  (let [w (count board)
        h (count (first board))]
    (loop [new-board board x 0 y 0]
      (cond
       (>= x w) new-board
       (>= y h) (recur new-board (inc x) 0)
       :else
       (let [new-liveness
             (case (count-neighbours board [x y])
               2 (get-in board [x y])
               3 :on
               nil)]
         (recur (assoc-in new-board [x y] new-liveness) x (inc y)))))))

(-> (iterate indexed-step glider) (nth 18) pprint)