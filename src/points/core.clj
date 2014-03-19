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
(require '[clojure.pprint :refer [pprint]])

(defn point-in-orbit
  [i points radius]
  (let [radians (* i (/ (* 2 Math/PI) points))]
    [(* radius (Math/cos radians)) (* radius (Math/sin radians)) 0]))

(defn draw-orbit
  [img-space points radius]
  (doseq [i (range points)]
    (let [light-source (point-in-orbit i points radius)]
      (-> img-space
        (draw/add-light-source light-source)
        (draw/blank!)
        (draw/blot! draw/poly! (draw/shaded draw/pink 0.1))
        (draw/write! (str "/tmp/img" i ".png"))))))

(try
(-> (init-img-space 2000 2000)
    (shape/fill-shape shape/sphere 100)
    (transform/scale-points 1500)
    (transform/random-rotate-points)
    (point/conv-hull)

    (draw/add-light-source [4000 2000 4000])
    (draw/blank!)
    (draw/blot! draw/poly! (draw/shaded draw/blue 0.5))
    (draw/write! "/tmp/img.png")

    (#(def last-img-space %))
    )
(catch Exception e (print-stack-trace e)))

)
