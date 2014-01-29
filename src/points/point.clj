(ns points.point
  (:require [clojure.core.matrix :as matrix]))

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

