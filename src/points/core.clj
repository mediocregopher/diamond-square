(ns points.core
  (:require [points.point :as point]
            [points.shape :as shape]
            [points.transform :as transform]
            [points.draw :as draw])
  (:import java.awt.image.BufferedImage
           java.awt.Color))

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
    { :grid-dims [grid-size grid-size]
      :grid-padding grid-buffer
      :grid-points #{}
      :gird-polys #{}
      :image-dims [imgw imgh]
      :image-buffer buf
      :image-graphic gfx }))

(defn conv-hull
  "Returns the set of faces making up the convex hull of the points in the given
  image space"
  [img-space]
  (point/conv-hull (img-space :grid-points)))

(defn fill-polys
  "Like fill-points, but fills in the :grid-polys key of the image space"
  [img-space polys-fn]
  (assoc img-space :grid-polys (polys-fn img-space)))

(comment

(require '[clojure.stacktrace :refer [e print-stack-trace]])
(try
(-> (init-img-space 2000 2000)
    (shape/fill-shape shape/blob 100)
    (transform/random-scale-points)
    (transform/random-rotate-points)
    (point/conv-hull)
    ;(draw/blot-points!)
    (draw/blot-polys!)
    (draw/write! "/tmp/img.png")
    ;((constantly nil))
    (#(def last-img-space %))
    )
(catch Exception e (print-stack-trace e)))

)
