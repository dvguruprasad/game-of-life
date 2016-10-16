(ns game-of-life.core)

(def deltas (range -1 2))

(defn- is-alive? [cell]
  (= :alive (:status cell)))

(defn- to-live-cells [points]
  (set (map (fn [point] {:point point :status :alive}) points)))

(defn- filter-live [cells]
  (filter is-alive? cells))

(defn- live-cell-points [cells]
  (set (map (fn [cell] (:point cell)) (filter-live cells))))

(defn- die [cell]
  (assoc cell :status :dead))

(defn- live [cell]
  (assoc cell :status :alive))

(defn- live-neighbour-count [neighbours]
  (count (filter-live neighbours)))

(defn- is-alive-at? [point live-cells]
  (some #(= {:point point :status :alive}  %) live-cells))

(defn- create-cell [point live-cells]
  {:point point
   :status (if (is-alive-at? point live-cells) :alive :dead)})

(defn- neighbouring-points [point]
  (for [x deltas y deltas :when (not= 0 x y)]
    (map + point [x y])))

(defn- neighbours [cell live-cells]
  (map #(create-cell % live-cells) (neighbouring-points (:point cell))))

(defn create-generation [live-cells]
  (let [add-neighbours
        (fn [result live-cell](concat result (neighbours live-cell live-cells)))]
    (reduce add-neighbours #{} live-cells)))

(defn next-generation-cell [cell live-cells]
  (let [live-neighbour-count (live-neighbour-count (neighbours cell live-cells))]
    (cond
      (or (< live-neighbour-count 2) (> live-neighbour-count 3)) (die cell)
      (= live-neighbour-count 3) (live cell)
      :else cell)
    ))

(defn next-generation [live-points]
  (let [live-cells (to-live-cells live-points)
        generation (create-generation live-cells)]
         (live-cell-points (map #(next-generation-cell % live-cells) generation)))) 

