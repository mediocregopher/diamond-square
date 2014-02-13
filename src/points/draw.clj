(ns points.draw
  (:require [points.point :as point])
  (:import java.awt.Color
           java.awt.image.BufferedImage
           java.awt.BasicStroke
           javax.imageio.ImageIO
           java.io.File))

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

(defn- norm-point
  "Normalizes a point from the original cartesian coordinate system into the
  coordinate system used for image creation"
  [imgw imgh gridw gridh [x y z]]
  [ (point/norm (- gridw) gridw 0 imgw x)
    (point/norm (- gridh) gridh 1 (int (* imgw 0.01)) y)
    (- imgh (point/norm (- gridh) gridh 0 imgh z)) ])

(defn- norm-point-center
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

(defn- norm-points
  "Normalizes the given points from the 3d grid-space to the 2d image space"
  [img-space points]
  (let [[imgw imgh] (img-space :image-dims)]
    (map (partial norm-point-center imgw imgh imgw imgh)
      points)))

(defn- norm-polys
  "Normalizes the given set of polys from the 3d grid-space to the 2d image
  space"
  [img-space polys]
  (map (partial norm-points img-space) polys))

(defn blank!
  "Given an image space, colors the whole thing image buffer white, creating and
  setting a new image-buffer if one wasn't there"
  [img-space]
  (let [[imgw imgh] (img-space :image-dims)
        buf (or (img-space :image-buffer)
                (BufferedImage. imgw imgh BufferedImage/TYPE_INT_RGB))
        gfx (or (img-space :image-graphic)
                (.createGraphics buf))]
    (.setPaint gfx Color/WHITE)
    (.fillRect gfx 0 0 (dec imgw) (dec imgh))
    (assoc img-space
      :image-buffer buf
      :image-graphic gfx)))

(defn blot!
  "Given an image spaces draws all of its polys such that they are drawn
  back-to-front"
  [img-space blot-fn & blot-fn-args]
  (reduce
    #(apply blot-fn %1 %2 blot-fn-args)
    img-space
    (norm-polys img-space
      (sort-by #((point/center-point %) 1)
        (img-space :grid-polys))))
  img-space)

(defn point!
  "Draws the given poly's points to the img-space using the given color
  function"
  [img-space poly color-fn]
  (let [gfx (img-space :image-graphic)
        color (color-fn img-space poly)
        points (set (flatten poly))]
  (.setPaint gfx color)
  (reduce
    (fn [gfx [normx normy normz]]
      (.fillOval gfx
        (- normx normy)
        (- normz normy)
        (* 2 normy)
        (* 2 normy)) gfx)
    (img-space :image-graphic)
    poly))
  img-space)

(defn line!
  "Draws the given poly's edges to the img-space using the given color function"
  ([img-space poly color-fn]
    (line! img-space poly color-fn 1))
  ([img-space poly color-fn thickness]
    (let [gfx (img-space :image-graphic)
          xs (int-array (map #(% 0) poly))
          zs (int-array (map #(% 2) poly))
          color (color-fn img-space poly)
          origStroke (.getStroke gfx)]
      (.setPaint gfx color)
      (.setStroke gfx (BasicStroke. thickness))
      (.drawPolyline gfx xs zs (count poly))
      (.setStroke gfx origStroke))
    img-space))

(defn poly!
  "Draws the given poly to the img-space using the given color function"
  [img-space poly color-fn]
  (let [gfx (img-space :image-graphic)
        xs (int-array (map #(% 0) poly))
        zs (int-array (map #(% 2) poly))
        color (color-fn img-space poly)]
    (.setPaint gfx color)
    (.fillPolygon gfx xs zs (count poly)))
  img-space)

(defn rand-color
  "Returns a random RGB color. Can be used as a color-fn for poly!"
  ([_ _] (rand-color))
  ([] (Color. (rand-int 0x1000000))))

(def gray "Always returns gray" (constantly Color/GRAY))
(def black "Always returns black" (constantly Color/BLACK))

(defn compose
  "Compose sets of draw-fn, color-fn, and any additional args into a single
  function which can be applied by blot! to every poly. Example:

  (compose
    [poly!  rand-color]
    [line!  black 3]
    [point! black])
  "
  [& args]
  (fn [img-space poly]
    (reduce
      (fn [img-space arg-set]
        (let [draw-fn (first arg-set)
              color-fn (second arg-set)
              extra-args (drop 2 arg-set)]
          (apply draw-fn img-space poly color-fn extra-args)))
      img-space
      args)))

(defn write!
  "Draws the image to the given file as a png"
  [img-space filename]
  (ImageIO/write (img-space :image-buffer) "png" (File. filename))
  img-space)

