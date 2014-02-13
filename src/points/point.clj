(ns points.point
  (:require [clojure.core.matrix :as matrix]
            [clojure.math.combinatorics :as comb]
            [clojure.set :refer [intersection difference]]))

(defn x-rot-matrix
  "Creates a 3d matrix which can rotate a point rad radians around the x-axis"
  [rad]
  [[1 0 0]
    [0 (Math/cos rad) (- (Math/sin rad))]
    [0 (Math/sin rad) (Math/cos rad)]])

(defn y-rot-matrix
  "Creates a 3d matrix which can rotate a point rad radians around the y-axis"
  [rad]
  [[(Math/cos rad) 0 (Math/sin rad)]
   [0 1 0]
   [(- (Math/sin rad)) 0 (Math/cos rad)]])

(defn z-rot-matrix
  "Creates a 3d matrix which can rotate a point rad radians around the z-axis"
  [rad]
  [[(Math/cos rad) (- (Math/sin rad)) 0]
   [(Math/sin rad) (Math/cos rad) 0]
   [0 0 1]])

(defn norm
  "Normalizes a point x in the range [A,B] to be in the range [nA,nB]"
  [A B nA nB x]
  (+ nA (/ (* (- x A) (- nB nA)) (- B A))))

(defn tri-rotate
  "Rotates a point first about the x, then y, then z axiis by the number of
  radians given for each axis"
  [point rx ry rz]
  (->> point
    (matrix/mmul (x-rot-matrix (* Math/PI rx)))
    (matrix/mmul (y-rot-matrix (* Math/PI ry)))
    (matrix/mmul (z-rot-matrix (* Math/PI rz)))))

(defn- minmax-coord
  [pred i points]
  (reduce #(if (pred (%1 i) (%2 i)) %1 %2) points))

(def min-x "Returns the point with the smallest x" (partial minmax-coord < 0))
(def max-x "Returns the point with the largest x" (partial minmax-coord > 0))
(def min-y "Returns the point with the smallest y" (partial minmax-coord < 1))
(def max-y "Returns the point with the largest y" (partial minmax-coord > 1))
(def min-z "Returns the point with the smallest z" (partial minmax-coord < 2))
(def max-z "Returns the point with the largest z" (partial minmax-coord > 2))

(defn center-point
  "Returns the center-point of a polygon"
  [poly]
  (let [xsum (reduce + (map #(% 0) poly))
        ysum (reduce + (map #(% 1) poly))
        zsum (reduce + (map #(% 2) poly))
        n (count poly)]
    [(/ xsum n) (/ ysum n) (/ zsum n)]))

(defn avg-center-point
  "Returns the center-point of a seq of polygons"
  [polys]
  (center-point
    (apply concat polys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing incremental convex hull

(defn on-normal?
  "Given a seq of three points (a face) and a fourth point, returns whether or
  not that fourth point is on the same side as the normal of the face"
  [[[x1 y1 z1]
    [x2 y2 z2]
    [x3 y3 z3]]
   [x4 y4 z4]]
  (neg?
    (matrix/det
      [[x1 x2 x3 x4]
       [y1 y2 y3 y4]
       [z1 z2 z3 z4]
       [1  1  1  1 ]])))

(defn orient-face
  "Rotates face's points around such that they form a positive tetrahedron with
  the given point (aka, their normal is pointing away from the given point)"
  [face point]
  (some #(when (not (do (on-normal? % point))) %) (comb/permutations face)))

(defn init-tetra
  "Given a seq of points, chooses four randomly and returns a vector containing
  the four faces created by those points (with the face points in the correct
  order) and the points not used in that initial tetrahedron"
  [points]
  (let [init-points     (set (take 4 points))
        faces           (comb/combinations init-points 3)
        faces-w-points  (map
                          #(vector % (first (apply disj init-points %)))
                          faces)
        oriented-faces  (set (map #(apply orient-face %) faces-w-points))
        leftover-points (difference points init-points)]
    [oriented-faces leftover-points]))

(defn init-tetra-fill
  "Debug function for viewing the init-tetra"
  [img-space]
  (first (init-tetra (img-space :grid-points))))

(defn filter-some
  "A combination of filter and some. Takes in a pred and a collection, returns
  a list of all items which matched (pred returned true) and a list of all items
  which didn't"
  [pred coll]
  (vec (map reverse
    (reduce
      (fn [[match nomatch] el]
        (if (pred el)
          [(cons el match) nomatch]
          [match (cons el nomatch)]))
      ['() '()]
      coll))))

(defn filter-map
  "A combination of filter and map. Takes in a map-fn and a collection, returns
  each (map-fn el) which is not false-y"
  [map-fn coll]
  (reverse
    (reduce (fn [r el] (if-let [mel (map-fn el)] (cons mel r) r)) '() coll)))

(defn remove-los-faces
  "Given faces and a point, returns a list of all faces that are in point's
  line-of-sight and a list of all that are not"
  [faces point]
  (filter-some #(on-normal? % point) faces))

(defn faces-to-outer-edges
  "Given a seq of faces, returns all outer edges of those faces. That is, all
  edges where the edge isn't found in another face which is in the given seq.
  The returned value is a seq of sets, each set being of two points"
  [faces]
  (->> faces
    (mapcat #(map set (comb/combinations % 2)))
    (group-by identity)
    (filter-map (fn [[k v]] (when (= (count v) 1) k)))))

(defn- point-to-orient
  "Returns the point to orient new faces to"
  [faces]
  (case (count faces)
    0 [0 0 0]
    1 (center-point (first faces))
      (avg-center-point faces)))

(defn incorporate-point
  "Given all the current faces and a point, removes all faces the point has
  line-of-sight to and connects the torn edges to the point. This is done by
  looking at each edge, concating point to it (to create a face), and calling
  orient-face on this new face, with the face oriented to the center-point of
  all the removed faces (which are presumably behind the new faces"
  [faces point]
  (let [[removed-faces remaining-faces] (remove-los-faces faces point)
        torn-edges (faces-to-outer-edges removed-faces)
        orient-point (point-to-orient removed-faces)]
    (concat remaining-faces
      (map #(orient-face (cons point %) orient-point) torn-edges))))

(defn conv-hull
  "Given a seq of points, returns a seq of all faces making up the convex hull
  (outermost shell) of those points"
  [img-space]
  (let [points (img-space :grid-points)
        [faces lpoints] (init-tetra points)]
    (assoc img-space :grid-polys
      (set (reduce #(incorporate-point %1 %2) faces lpoints)))))
