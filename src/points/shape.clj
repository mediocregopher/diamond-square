(ns points.shape
  (:require [points.point :as point]))

(defn fill-shape
  "Given an img-space, a function which returns a set of points, and the
  arguments for that function, calls that function and fills in :grid-points on
  the given img-space, returning the new one"
  [img-space point-fn & args]
  (assoc img-space :grid-points
    (apply point-fn args)))

(defn cube
  "Generates a cube with one corner at (x1,y1,z1) and the opposite corner at
  (x2,y2,z2), with the cube represented as a vector of points, each point being
  a vector of 3 numbers"
  [x1 y1 z1 x2 y2 z2]
  (set
    (for [x [x1 x2]
          y [y1 y2]
          z [z1 z2]]
      [x y z])))

(defn unit-cube
  "Returns a cube of edge size 1 with its center at the origin"
  []
  (cube -0.5 -0.5 -0.5 0.5 0.5 0.5))

(defn sphere
  "Generates a set of n points representing something close to a sphere

  Code for this was basically stolen straight from:
  http://web.archive.org/web/20120107030109/http://cgafaq.info/wiki/Evenly_distributed_points_on_sphere"
  [n]
  (let [s (/ 3.6 (Math/sqrt n))
        dz (/ 2.0 n)
        z (- 1 (/ dz 2.0))]
    (set (first (reduce
      (fn [[nodes z longi] _]
        (let [r (Math/sqrt (- 1 (* z z)))
              node [(* (Math/cos longi) r) (* (Math/sin longi) r) z]
              newz (- z dz)
              newlongi (+ longi (/ s r))]
          [(cons node nodes) newz newlongi]))
      ['() z 0]
      (range n))))))

(defn blob-cube
  "Generates a set of n points whose coordinates are randomly spread throughout
  a 1x1x1 cube"
  [n]
  (let [rand-fn #(- 0.5 (rand))]
    (set (take n (repeatedly #(vector (rand-fn) (rand-fn) (rand-fn)))))))

(defn blob-sphere
  "Generates a set of n points whose coordinates are randomly spread through an
  r=1 sphere"
  [n]
  (let [rand-fn #(point/tri-rotate % (rand 2) (rand 2) (rand 2))]
    (set (take n (repeatedly #(rand-fn [(rand) 0 0]))))))
