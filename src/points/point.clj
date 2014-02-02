(ns points.point
  (:require [clojure.core.matrix :as matrix]
            [clojure.math.combinatorics :as comb]
            [clojure.set :refer [intersection]]))

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

; norm-point takes a point from this grid:

;                 ^ z (gridh)
;                 |
;                 |
;                 |
;                 |
;                 |
;                 |
;                 |
; <-------------------------------> x (gridw)
;                 |
;                 |
;                 |
;                 |
;                 |
;                 |
;                 |
;                 v
;
; To this grid:
;
; -------------------> x (imgw)
; |
; |
; |
; |
; |
; |
; |
; v z (imgh)

; While keeping the same relative shape and distances

(defn norm-point
  "Normalizes a point from the original cartesian coordinate system into the
  coordinate system used for image creation"
  [imgw imgh gridw gridh [x y z]]
  [ (norm (- gridw) gridw 0 imgw x)
    (norm (- gridh) gridh 1 (int (* imgw 0.01)) y)
    (- imgh (norm (- gridh) gridh 0 imgh z)) ])

(defn norm-point-center
  "Same as norm-point, but doesn't stretch the new points to fit the image
  dimensions. Instead, centers them along the longer axis"
  [imgw imgh gridw gridh [x y z]]
  (if (> imgw imgh)
    (let [cent-imgw (int (* gridw (/ imgh gridh)))
          pad (int (/ (- imgw cent-imgw) 2))
          [x y z] (norm-point cent-imgw imgh gridw gridh [x y z])]
      [(+ pad x) y z])
    (let [cent-imgh (int (* gridh (/ imgw gridw)))
          pad (int (/ (- imgh cent-imgh) 2))
          [x y z] (norm-point imgw cent-imgh gridw gridh [x y z])]
      [x y (+ pad z)])))

(defn tri-rotate
  "Rotates a point first about the x, then y, then z axiis by the number of
  radians given for each axis"
  [point rx ry rz]
  (->> point
    (matrix/mmul (x-rot-matrix (* Math/PI rx)))
    (matrix/mmul (y-rot-matrix (* Math/PI ry)))
    (matrix/mmul (z-rot-matrix (* Math/PI rz)))))

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

(defn excluded-points
  "Given two lists of points, one a subset of the other, returns all points not
  in the subset"
  [big-list small-list]
  (into '() (apply disj (set big-list) small-list)))

(defn init-tetra
  "Given a seq of points, chooses four randomly and returns a vector containing
  the four faces created by those points (with the face points in the correct
  order) and the points not used in that initial tetrahedron"
  [points]
  (let [init-points     (take 4 (shuffle points))
        faces           (comb/combinations init-points 3)
        expoints        (map #(first (excluded-points init-points %)) faces)
        faces-w-points  (map vector faces expoints)
        oriented-faces  (map #(apply orient-face %) faces-w-points)
        leftover-points (excluded-points points init-points)]
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

(defn shared-edge
  "Returns the edge shared by two faces, or nil if they don't share an edge"
  [face1 face2]
  (let [edge-set (intersection (set face1) (set face2))]
    (if (not= 2 (count edge-set)) nil (vec edge-set))))

(defn shared-edge-data
  "Given a seq of faces and a face, returns a list with an item representing
  each edge shared by the face with one of the faces. Each item is a vector
  containing the shared edge and the non-shared point in the other face"
  [faces face]
  (filter-map
    #(when-let [edge (shared-edge face %)]
      [edge (first (excluded-points % edge))])
    faces))

(defn incorporate-point
  "Given all the current faces and a point, removes all faces the point has
  line-of-sight to and connects the torn edges to the point. This is done by
  looking at each edge, concating point to it (to create a face), and calling
  orient-face on this new face, with the old point being the reference for which
  way to orient the new face."
  [faces point]
  (let [[removed-faces remaining-faces] (remove-los-faces faces point)]
    (reduce
      (fn [rem-faces rmd-face]
        (let [edge-data (shared-edge-data
                          (remove #(= % rmd-face) faces) rmd-face)]
          (concat rem-faces
            (map
              (fn [[edge oldpoint]]
                (orient-face (cons point edge) oldpoint))
              edge-data))))
      remaining-faces
      removed-faces)))

(defn conv-hull
  "Given a seq of points, returns a seq of all faces making up the convex hull
  (outermost shell) of those points"
  [points]
  (let [[faces lpoints] (init-tetra points)]
    (reduce #(do (incorporate-point %1 %2)) faces lpoints)))

(defn conv-hull-fill
  [img-space]
  (conv-hull (img-space :grid-points)))
