(ns points.core
  (:require [points.point :as point]
            [points.shape :as shape])
  (:import java.awt.image.BufferedImage
           java.awt.Color
           javax.imageio.ImageIO
           java.io.File))

; Both of these are totally arbitrary. Their relative values matter, their
; absolute values really don't
(def grid-size
  "The size of the grid on the smaller axis of the image"
  20)

(def grid-buffer
  "Amount of grid space to leave open on the sides"
  (int (* grid-size 0.3)))

(defn init-img-space
  "Creates an img-space map for the given pixel heights and widths. The
  img-space holds all information about this particular image, including points
  locations and java objects, and is what is piped through all other functions"
  [imgw imgh]
  (let [buf (BufferedImage. imgw imgh BufferedImage/TYPE_INT_RGB)
        gfx (.createGraphics buf)]
    (.setPaint gfx Color/WHITE)
    (.fillRect gfx 0 0 (dec imgw) (dec imgh))
    (.setPaint gfx Color/BLACK)
    { :grid-dims [grid-size grid-size]
      :grid-padding grid-buffer
      :grid-points #{}
      :gird-polys #{}
      :image-dims [imgw imgh]
      :image-buffer buf
      :image-graphic gfx }))

(defn fill-points
  "Given an image space and a function uses the return of that function as the
  new list of points in the image space. The function takes in the image space
  object in case it needs it"
  [img-space points-fn & args]
  (assoc img-space :grid-points (apply points-fn img-space args)))

(defn scale-points
  "Scales the points in an image space by the given multiplier"
  [img-space amnt]
  (set (map #(vec (map (partial * amnt) %)) (img-space :grid-points))))

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
  (let [[rx ry rz] [(rand) (rand) (rand)]
        points (img-space :grid-points)]
    (set (map #(point/tri-rotate % rx ry rz) points))))

(defn conv-hull
  "Returns the set of faces making up the convex hull of the points in the given
  image space"
  [img-space]
  (point/conv-hull (img-space :grid-points)))

(defn fill-polys
  "Like fill-points, but fills in the :grid-polys key of the image space"
  [img-space polys-fn]
  (assoc img-space :grid-polys (polys-fn img-space)))

(defn dot
  "Draws a dot on the graphic, given the center x/y coordinates and a radius"
  [graphic radius middlex middley]
  (.fillOval graphic
    (- middlex radius)
    (- middley radius)
    (* 2 radius)
    (* 2 radius)))

(defn blot-points
  "Given an image space takes all of its points and draws them to the graphic
  object. It draws the points with different radius' depending on their y value"
  [img-space]
  (.setPaint (img-space :image-graphic) Color/BLACK)
  (let [points (img-space :grid-points)
        [imgw imgh] (img-space :image-dims)
        [gridw gridh] (img-space :grid-dims)
        gfx (img-space :image-graphic)
        norm-points-fn (partial point/norm-point-center imgw imgh gridw gridh)
        norm-points (map norm-points-fn points)]
    (reduce (fn [gfx [normx normy normz]]
              (dot gfx normy normx normz)
              gfx) gfx norm-points))
  img-space)

(defn blot-lines
  "Given an image space takes all of its polys and draws their outlines to the
  graphic object"
  [img-space]
  (.setPaint (img-space :image-graphic) Color/GRAY)
  (let [polys (img-space :grid-polys)
        [imgw imgh] (img-space :image-dims)
        [gridw gridh] (img-space :grid-dims)
        gfx (img-space :image-graphic)
        norm-points-fn (partial point/norm-point-center imgw imgh gridw gridh)
        norm-polys (map #(map norm-points-fn %) polys)]
    (reduce (fn [gfx poly]
              (let [xs (int-array (map #(% 0) poly))
                    zs (int-array (map #(% 2) poly))]
                (.drawPolygon gfx xs zs (count poly))
                gfx)) gfx norm-polys))
  img-space)

(defn blot-polys
  [img-space]
  (let [polys (img-space :grid-polys)
        [imgw imgh] (img-space :image-dims)
        [gridw gridh] (img-space :grid-dims)
        gfx (img-space :image-graphic)
        norm-points-fn (partial point/norm-point-center imgw imgh gridw gridh)
        sorted-polys (sort-by #((point/center-point %) 1) polys)
        norm-polys (map #(map norm-points-fn %) sorted-polys)]
    (reduce (fn [gfx poly]
              (let [xs (int-array (map #(% 0) poly))
                    zs (int-array (map #(% 2) poly))
                    color (Color. (rand-int 0x1000000))]
                (.setPaint (img-space :image-graphic) color)
                (.fillPolygon gfx xs zs (count poly))
                gfx)) gfx norm-polys)
  img-space))

(defn draw
  "Draws the image to the given file as a png"
  [img-space filename]
  (ImageIO/write (img-space :image-buffer) "png" (File. filename))
  img-space)

(comment

(require '[clojure.stacktrace :refer [e print-stack-trace]])
(try
(-> (init-img-space 1000 1000)
    (fill-points shape/blob 50)
    (fill-points scale-points 15)
    (fill-points random-rotate-points)
    (fill-polys conv-hull)
    (blot-polys)
    (draw "/tmp/img.png")
    ;((constantly nil))
    (#(def last-img-space %))
    )
(catch Exception e (print-stack-trace e)))

)
