(ns game-of-life.core)

(defn- to-cells [points status]
  (set (map (fn [point] {:point point :status status}) points)))

(defn- to-points [cells status]
  (set (map (fn [cell] (:point cell)) (filter #(= status (:status %)) cells))))

(defn- is-alive? [cell]
  (= :alive (:status cell)))

(defn- live-neighbour-count [neighbours]
  (count (filter is-alive? neighbours)))

(defn- is-alive-at? [point live-cells]
  (some #(= {:point point :status :alive}  %) live-cells))

(defn- cell-status [point live-cells]
  (if (is-alive-at? point live-cells) :alive :dead))

(defn- neighbouring-cell [surrounding-point live-cells]
  {:point surrounding-point
   :status (cell-status surrounding-point live-cells)})

(defn- neighbouring-cells [cell live-cells]
  (set (let [point (:point cell)
             deltas (range -1 2)]
         (for [x deltas y deltas :when (not= 0 x y)] 
           (let [surrounding-point (map + point [x y])]
             (neighbouring-cell surrounding-point live-cells))
           ))
       ))

(defn create-generation [live-cells]
  (reduce
    #(set (concat %1 (neighbouring-cells %2 live-cells)))
    #{}
    live-cells
    ))

(defn next-generation-cell [cell live-cells]
  (let [neighbours (neighbouring-cells cell live-cells)
        live-neighbour-count (live-neighbour-count neighbours)]
    (if (or (< live-neighbour-count 2) (> live-neighbour-count 3))
      {:point (:point cell) :status :dead}
      (if (= live-neighbour-count 3)
        {:point (:point cell) :status :alive}
        cell
        )
      )))

(defn next-generation [points]
  (let [live-cells (to-cells points :alive)
             generation (create-generation live-cells)]
         (to-points (map #(next-generation-cell % live-cells) generation) :alive))) 

