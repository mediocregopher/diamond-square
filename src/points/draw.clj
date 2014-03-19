(ns points.draw
  (:require [points.point :as point])
  (:import java.awt.Color
           java.awt.image.BufferedImage
           java.awt.RenderingHints
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

; While keeping the same relative shape and distances, but eliminating the y
; axis

(defn- norm-point
  "Normalizes a point from the original cartesian coordinate system into the
  coordinate system used for image creation"
  [imgw imgh gridw gridh [x y z]]
  [ (point/norm (- gridw) gridw 0 imgw x)
    (- imgh (point/norm (- gridh) gridh 0 imgh z)) ])

(defn- norm-point-center
  "Same as norm-point, but doesn't stretch the new points to fit the image
  dimensions. Instead, centers them along the longer axis"
  [imgw imgh gridw gridh [x y z]]
  (if (> imgw imgh)
    (let [cent-imgw (int (* gridw (/ imgh gridh)))
          pad (int (/ (- imgw cent-imgw) 2))
          [x z] (norm-point cent-imgw imgh gridw gridh [x y z])]
      [(+ pad x) z])
    (let [cent-imgh (int (* gridh (/ imgw gridw)))
          pad (int (/ (- imgh cent-imgh) 2))
          [x z] (norm-point imgw cent-imgh gridw gridh [x y z])]
      [x (+ pad z)])))

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

(defn add-light-source
  "Adds a light source at the given position to the img-space. The position of
  the light source, along with the closest and farthest distanced points in the
  img-space, are stored"
  [img-space pos]
  (let [points (img-space :grid-points)
        [closest farthest] (point/min-max-from pos points)
        prev-sources (img-space :light-sources '())]
    (assoc img-space
      :light-sources (cons {:pos pos
                            :min (point/point-distance pos closest)
                            :max (point/point-distance pos farthest)}
                           prev-sources))))

(def hints
  (list [RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON]
        [RenderingHints/KEY_STROKE_CONTROL RenderingHints/VALUE_STROKE_PURE]
        [RenderingHints/KEY_COLOR_RENDERING RenderingHints/VALUE_COLOR_RENDER_QUALITY]))

(defn blank!
  "Given an image space, colors the whole thing image buffer white, creating and
  setting a new image-buffer if one wasn't there. It also fills in some stats
  that will be useful in other functions"
  [img-space]
  (let [[imgw imgh] (img-space :image-dims)
        buf (or (img-space :image-buffer)
                (BufferedImage. imgw imgh BufferedImage/TYPE_INT_RGB))
        gfx (or (img-space :image-graphic)
                (.createGraphics buf))
        ys (mapcat (partial map #(% 1)) (img-space :grid-polys))]
    (.setPaint gfx Color/WHITE)
    (doseq [[k v] hints]
      (.setRenderingHint gfx k v))
    (.fillRect gfx 0 0 (dec imgw) (dec imgh))
    (assoc img-space
      :image-buffer buf
      :image-graphic gfx)))

(defn blot!
  "Given an image spaces draws all of its polys such that they are drawn
  back-to-front"
  [img-space blot-fn & blot-fn-args]
  (let [sorted-polys (sort-by #((point/center-point %) 1)
                      (img-space :grid-polys))]
    (reduce
      (fn [img-space [poly norm-poly]]
        (apply blot-fn img-space poly norm-poly blot-fn-args))
      img-space
      (map vector
        sorted-polys (norm-polys img-space sorted-polys)))
    img-space))

(defn point!
  "Draws the given poly's points to the img-space using the given color
  function"
  ([img-space poly norm-poly color-fn]
    (point! img-space poly norm-poly color-fn 1))
  ([img-space _ norm-poly color-fn size]
    (let [gfx (img-space :image-graphic)
          color (color-fn img-space norm-poly)
          points (set (flatten norm-poly))]
    (.setPaint gfx color)
    (reduce
      (fn [gfx [normx normz]]
        (.fillOval gfx
          normx normz size size)
          gfx)
      (img-space :image-graphic)
      norm-poly))
    img-space))

(defn line!
  "Draws the given poly's edges to the img-space using the given color function"
  ([img-space poly norm-poly color-fn]
    (line! img-space poly norm-poly color-fn 1))
  ([img-space _ norm-poly color-fn thickness]
    (let [gfx (img-space :image-graphic)
          xs (int-array (map first norm-poly))
          zs (int-array (map second norm-poly))
          color (color-fn img-space norm-poly)
          origStroke (.getStroke gfx)]
      (.setPaint gfx color)
      (.setStroke gfx (BasicStroke. thickness
                        BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
      (.drawPolyline gfx xs zs (count norm-poly))
      (.setStroke gfx origStroke))
    img-space))

(defn poly!
  "Draws the given poly to the img-space using the given color function"
  [img-space poly norm-poly color-fn]
  (let [gfx (img-space :image-graphic)
        xs (int-array (map first norm-poly))
        zs (int-array (map second norm-poly))
        color (color-fn img-space poly norm-poly)]
    (.setPaint gfx color)
    (.fillPolygon gfx xs zs (count norm-poly)))
  img-space)

(defn rand-color
  "Returns a random RGB color. Can be used as a color-fn for poly!"
  ([_ _] (rand-color))
  ([] (Color. (rand-int 0x1000000))))

(def gray "Always returns gray" (constantly Color/GRAY))
(def black "Always returns black" (constantly Color/BLACK))
(def blue "Always returns blue" (constantly Color/BLUE))
(def pink "Always returns ping" (constantly Color/PINK))

(defn- squash
  [minv maxv v]
  (if (< v minv) minv
    (if (> v maxv) maxv
      v)))

(defn- color-offset
  "Offsets all components of an RGB Color object by the given amount (can be
  positive or negative"
  [color offset]
  (Color.
    (squash 0 255 (+ offset (.getRed color)))
    (squash 0 255 (+ offset (.getGreen color)))
    (squash 0 255 (+ offset (.getBlue color)))))

(defn shaded
  "returns a color-fn which takes into account light sources to determine the
  brightness of the surface. Takes in a base color-fn"
  ([color-fn]
    (shaded color-fn 1))
  ([color-fn shading-scaler]
    (fn [img-space poly _]
      (let [base-color (color-fn img-space poly)
            center (point/center-point poly)
            light (first (img-space :light-sources))
            [pos closest farthest] [(light :pos) (light :min) (light :max)]
            dis (point/point-distance center pos)
            shade-ceil (* 127 shading-scaler)
            offset (int (point/norm-sqr closest farthest shade-ceil -128 dis))]
        (color-offset base-color offset)))))

(defn compose
  "Compose sets of draw-fn, color-fn, and any additional args into a single
  function which can be applied by blot! to every poly. Example:

  (compose
    [poly!  rand-color]
    [line!  black 3]
    [point! black])
  "
  [& args]
  (fn [img-space poly norm-poly]
    (reduce
      (fn [img-space arg-set]
        (let [draw-fn (first arg-set)
              color-fn (second arg-set)
              extra-args (drop 2 arg-set)]
          (apply draw-fn img-space poly norm-poly color-fn extra-args)))
      img-space
      args)))

(defn write!
  "Draws the image to the given file as a png"
  [img-space filename]
  (ImageIO/write (img-space :image-buffer) "png" (File. filename))
  img-space)

