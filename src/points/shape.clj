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

(defn blob
  "Generates a set of n points representing something close to a sphere

  Code for this was basically stolen straight from:
  http://web.archive.org/web/20120107030109/http://cgafaq.info/wiki/Evenly_distributed_points_on_sphere"
  ([_ n]
    (blob n))
  ([n]
    (let [s (/ 3.6 (Math/sqrt n))
          dz (/ 2.0 n)
          z (- 1 (/ dz 2.0))]
      (first (reduce
        (fn [[nodes z longi] _]
          (let [r (Math/sqrt (- 1 (* z z)))
                node [(* (Math/cos longi) r) (* (Math/sin longi) r) z]
                newz (- z dz)
                newlongi (+ longi (/ s r))]
            [(cons node nodes) newz newlongi]))
        ['() z 0]
        (range n))))))

(comment
  (blob 20)
)
