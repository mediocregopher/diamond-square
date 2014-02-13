(ns points.core
  (:require [points.point :as point]
            [points.shape :as shape]
            [points.transform :as transform]
            [points.draw :as draw]))

(defn init-img-space
  "Creates an img-space map for the given pixel heights and widths. The
  img-space holds all information about this particular image, including points
  locations and java objects, and is what is piped through all other functions"
  [imgw imgh]
  { :grid-points #{}
    :gird-polys #{}
    :image-dims [imgw imgh] })

(comment

(require '[clojure.stacktrace :refer [e print-stack-trace]])
(try
(-> (init-img-space 2000 2000)
    (shape/fill-shape shape/sphere 100)
    (transform/scale-points 1500)
    (transform/random-rotate-points)
    (point/conv-hull)

    (draw/blank!)
    (draw/blot!
      (draw/compose
        [draw/poly! draw/rand-color]
        [draw/line! draw/black 3]))

    (draw/write! "/tmp/img.png")
    ;((constantly nil))
    (#(def last-img-space %))
    )
(catch Exception e (print-stack-trace e)))

)
