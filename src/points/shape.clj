(ns points.shape)

(defn cube
  "Generates a cube with one corner at (x1,y1,z1) and the opposite corner at
  (x2,y2,z2), with the cube represented as a vector of points, each point being
  a vector of 3 numbers"
  [x1 y1 z1 x2 y2 z2]
  (for [x [x1 x2]
        y [y1 y2]
        z [z1 z2]]
    [x y z]))

(defn unit-cube
  "Returns a cube of edge size 1 with its center at the origin"
  ([]
    (cube -0.5 -0.5 -0.5 0.5 0.5 0.5))
  ([_]
    (unit-cube)))
