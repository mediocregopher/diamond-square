(ns points.transform
  (:require [points.point :as point]))

(defn scale-points
  "Scales the points in an image space by the given multiplier"
  [img-space amnt]
  (assoc img-space :grid-points
    (set (map #(vec (map (partial * amnt) %)) (img-space :grid-points)))))

(defn random-scale-points
  "Scales the points in an image space a random amount relative to the origin"
  [img-space]
  (let [pad (img-space :grid-padding)
        mdim (apply min (img-space :grid-dims))
        r (+ pad (rand-int (- mdim (* 2 pad))))]
    (scale-points img-space r)))

(defn random-rotate-points
  "Rotates the points in an image space a random amount around the origin"
  [img-space]
  (assoc img-space :grid-points
    (let [[rx ry rz] [(rand 2) (rand 2) (rand 2)]
          points (img-space :grid-points)]
      (set (map #(point/tri-rotate % rx ry rz) points)))))
