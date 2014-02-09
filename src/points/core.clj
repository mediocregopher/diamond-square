(ns points.core
  (:require [points.point :as point]
            [points.shape :as shape]
            [points.transform :as transform]
            [points.draw :as draw]))

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
  { :grid-dims [grid-size grid-size]
    :grid-padding grid-buffer
    :grid-points #{}
    :gird-polys #{}
    :image-dims [imgw imgh] })

(comment

(require '[clojure.stacktrace :refer [e print-stack-trace]])
(try
(-> (init-img-space 2000 2000)
    (shape/fill-shape shape/blob-sphere 300)
    (transform/random-scale-points)
    (transform/random-rotate-points)
    (point/conv-hull)

    (draw/blank!)
    (draw/blot-lines!)
    (draw/blot-points!)
    (draw/write! "/tmp/img-lines.png")

    (draw/blank!)
    (draw/blot-polys!)
    (draw/write! "/tmp/img.png")
    ;((constantly nil))
    (#(def last-img-space %))
    )
(catch Exception e (print-stack-trace e)))

)
