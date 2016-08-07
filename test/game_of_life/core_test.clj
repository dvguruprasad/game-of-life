(ns game-of-life.core-test
  (:require [clojure.test :refer :all]
            [game-of-life.core :refer :all]))

(deftest block-pattern-still-life
  (is (= #{[1 1] [1 2] [2 1] [2 2]}
         (next-generation #{[1 1], [1 2], [2 1], [2 2]})
         )))

(deftest boat-pattern-still-life
  (is (= #{[0 1] [1 0] [2 1] [0 2] [1 2]}
         (next-generation #{[0 1] [1 0] [2 1] [0 2] [1 2]})
         )))

(deftest blinker-pattern-oscillator
  (is (= #{[1 1] [0 1] [2 1]}
         (next-generation #{[1 1] [1 0] [1 2]})
         )))

(deftest blinker-pattern-oscillator
  (is (= #{[0 2] [1 1] [1 4] [2 1] [2 4] [3 3]}
         (next-generation #{[1 1] [1 2] [1 3] [2 2] [2 3] [2 4]})
         )))

