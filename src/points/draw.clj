(ns points.draw
  (:require [points.point :as point])
  (:import java.awt.Color
           javax.imageio.ImageIO
           java.io.File))

(defn dot
  "Draws a dot on the graphic, given the center x/y coordinates and a radius"
  [graphic radius middlex middley]
  (.fillOval graphic
    (- middlex radius)
    (- middley radius)
    (* 2 radius)
    (* 2 radius)))

(defn blot-points!
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

(defn blot-lines!
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

(defn blot-polys!
  "Given an image spaces draws all of its polys such that they are drawn
  back-to-front"
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

(defn write!
  "Draws the image to the given file as a png"
  [img-space filename]
  (ImageIO/write (img-space :image-buffer) "png" (File. filename))
  img-space)

