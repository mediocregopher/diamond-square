(ns points.core
  (:gen-class)
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

(defn- rand-range
  [min max]
  (+ min (rand-int (inc (- max min)))))

(defn -main [& args]
  (let [filename (nth args 0)
        width (Integer/parseInt (nth args 1))
        height (Integer/parseInt (nth args 2))]

    (-> (init-img-space width height)
        (shape/fill-shape shape/sphere (rand-range 50 250))
        (transform/scale-points (int (* 0.75 (min width height))))
        (transform/random-rotate-points)
        (point/conv-hull)

        (draw/add-light-source [(rand-range (- width) width)
                                (max width height)
                                (rand-range (- height) height)])
        (draw/blank!)
        (draw/blot!
          (draw/compose
            [draw/poly! (draw/shaded (draw/choose-rand-color) 0.1)]))
        (draw/write! filename)
        ((constantly nil)))))

(comment
  (-main "/tmp/img.png" "2000" "2000")
)
