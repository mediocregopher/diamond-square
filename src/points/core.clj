(ns points.core
  (:require [points.point :as point]
            [points.cube :as cube])
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
  [imgw imgh]
  (let [buf (BufferedImage. imgw imgh BufferedImage/TYPE_INT_RGB)
        gfx (.createGraphics buf)]
    (.setPaint gfx Color/WHITE)
    (.fillRect gfx 0 0 (dec imgw) (dec imgh))
    { :grid-dims [grid-size grid-size]
      :grid-padding grid-buffer
      :grid-points []
      :image-dims [imgw imgh]
      :image-buffer buf
      :image-graphic gfx }))

; TODO Break out random-size-cube into random-scale, random-rotate, and
;      unit-cube
(defn random-size-cube
  [img-space]
  (let [pad (img-space :grid-padding)
        mdim (apply min (img-space :grid-dims))
        r (+ pad (rand-int (- mdim (* 2 pad))))
        points (cube/random-cube)]
    (map #(vec (map (partial * r) %)) points)))

(defn fill-points
  [img-space points-fn]
  (assoc img-space :grid-points (points-fn img-space)))

(defn dot
  [graphic radius middlex middley]
  (.fillOval graphic
    (- middlex radius)
    (- middley radius)
    (* 2 radius)
    (* 2 radius)))

(defn blot-points
  [img-space]
  (.setPaint (img-space :image-graphic) Color/BLACK)
  (let [points (img-space :points)
        [imgw imgh] (img-space :image-dims)
        [gridw gridh] (img-space :grid-dims)
        gfx (img-space :image-graphic)
        norm-points-fn (partial point/norm-point-center imgw imgh gridw gridh)
        norm-points (map norm-points-fn points)]
    (reduce (fn [gfx [normx normy normz]]
              (dot gfx normy normx normz)
              gfx) gfx norm-points))
  img-space)

(defn draw
  [img-space filename]
  (ImageIO/write (img-space :image-buffer) "png" (File. filename)))

(comment

(-> (init-img-space 1000 1000)
    (fill-points random-size-cube)
    (blot-points)
    (draw "/tmp/img.png"))

)
