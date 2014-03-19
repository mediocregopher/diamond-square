(ns points.transform
  (:require [points.point :as point]))

(defn scale-point
  [point amnt]
  (vec (map (partial * amnt) point)))

(defn scale-points
  "Scales the points in an image space by the given multiplier"
  [img-space amnt]
  (assoc img-space :grid-points
    (set (map #(scale-point % amnt) (img-space :grid-points)))))

(defn random-scale-points
  "Scales the points in an image space a random amount relative to the origin"
  [img-space limit]
  (scale-points img-space (inc (rand (dec limit)))))

(defn random-rotate-points
  "Rotates the points in an image space a random amount around the origin"
  [img-space]
  (assoc img-space :grid-points
    (let [[rx ry rz] [(rand 2) (rand 2) (rand 2)]
          points (img-space :grid-points)]
      (set (map #(point/tri-rotate % rx ry rz) points)))))

(defn- replace-point-in-polys
  [old-point new-point polys]
  (map
    #(map (fn [point] (if (= old-point point) new-point point)) %)
    polys))

(defn distort-point
  "Distorts a point by scaling it a random factor (in the range of (-err, err))
  along its line to the origin"
  [point err]
  (scale-point point (+ (- 1 err) (rand (* 2 err)))))

(defn- distort-points-and-polys
  [points polys err]
  (reduce
    (fn [[points polys] point]
      (let [new-point (distort-point point err)]
        [(conj points new-point)
         (replace-point-in-polys point new-point polys)]))
    [#{} polys]
    points))

(defn distort
  [img-space err]
  (let [[new-points new-polys] (distort-points-and-polys
                                  (img-space :grid-points)
                                  (img-space :grid-polys)
                                  err)]
    (assoc img-space :grid-points new-points :grid-polys (set new-polys))))
