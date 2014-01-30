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
      :grid-points []
      :image-dims [imgw imgh]
      :image-buffer buf
      :image-graphic gfx }))

(defn fill-points
  "Given an image space and a function uses the return of that function as the
  new list of points in the image space. The function takes in the image space
  object in case it needs it"
  [img-space points-fn]
  (assoc img-space :grid-points (points-fn img-space)))

(defn random-scale-points
  "Scales the points in an image space a random amount relative to the origin"
  [img-space]
  (let [pad (img-space :grid-padding)
        mdim (apply min (img-space :grid-dims))
        r (+ pad (rand-int (- mdim (* 2 pad))))
        points (img-space :grid-points)]
    (map #(vec (map (partial * r) %)) points)))

(defn random-rotate-points
  "Rotates the points in an image space a random amount around the origin"
  [img-space]
  (let [[rx ry rz] [(rand) (rand) (rand)]
        points (img-space :grid-points)]
    (map #(point/tri-rotate % rx ry rz) points)))

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

(defn draw
  "Draws the image to the given file as a png"
  [img-space filename]
  (ImageIO/write (img-space :image-buffer) "png" (File. filename)))

(comment

(-> (init-img-space 1000 1000)
    (fill-points shape/unit-cube)
    (fill-points random-scale-points)
    (fill-points random-rotate-points)
    (blot-points)
    (draw "/tmp/img.png"))

)
