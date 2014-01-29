(ns points.cube
  (:require [points.point :as point]))

(defn cube
  "Generates a cube with one corner at (x1,y1,z1) and the opposite corner at
  (x2,y2,z2), with the cube represented as a vector of points, each point being
  a vector of 3 numbers"
  [x1 y1 z1 x2 y2 z2]
  (for [x [x1 x2]
        y [y1 y2]
        z [z1 z2]]
    [x y z]))

(defn random-cube
  "Given a size, generates a unit cube of random rotation, but none of whose
  points will be outside that given size on any axis"
  []
  (let [[rx ry rz] [(rand) (rand) (rand)]]
    (map #(point/tri-rotate % rx ry rz) (cube 0.5 0.5 0.5 -0.5 -0.5 -0.5))))
