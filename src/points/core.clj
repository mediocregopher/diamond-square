(ns points.core
  (:require [clojure.core.matrix :as matrix]))

(import
  'java.awt.image.BufferedImage
  'java.awt.Color
  'javax.imageio.ImageIO
  'java.io.File)

(def width 500)
(def height 500)

(defn dot
  [graphic radius middlex middley]
  (.fillOval graphic
    (- middlex radius)
    (- middley radius)
    (* 2 radius)
    (* 2 radius)))

(defn cube
  [x1 y1 z1 x2 y2 z2]
  (for [x [x1 x2]
        y [y1 y2]
        z [z1 z2]]
    [x y z]))

(defn x-rot-matrix
  [rad]
  [[1 0 0]
    [0 (Math/cos rad) (- (Math/sin rad))]
    [0 (Math/sin rad) (Math/cos rad)]])

(defn y-rot-matrix
  [rad]
  [[(Math/cos rad) 0 (Math/sin rad)]
   [0 1 0]
   [(- (Math/sin rad)) 0 (Math/cos rad)]])

(defn z-rot-matrix
  [rad]
  [[(Math/cos rad) (- (Math/sin rad)) 0]
   [(Math/sin rad) (Math/cos rad) 0]
   [0 0 1]])

(defn norm
  [A B nA nB x]
  (+ nA (/ (* (- x A) (- nB nA)) (- B A))))

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

; While keeping the same relative shape and distances

(defn norm-point
  [imgw imgh gridw gridh [x y z]]
  [ (norm (- gridw) gridw 0 imgw x)
    (norm (- gridh) gridh 1 (int (* imgw 0.01)) y)
    (- imgh (norm (- gridh) gridh 0 imgh z)) ])

; Both of these are totally arbitrary. Their relative values matter, their
; absolute values really don't
(def grid-size
  "The size of the grid on the smaller axis of the image"
  20)

(def grid-buffer
  "Amount of grid space to leave open on the sides"
  (int (* grid-size 0.2)))

(defn init-lattice
  [imgw imgh]
  (let [min-dim (min imgw imgh)
        norm-fn #(norm 0 min-dim 0 grid-size %)
        buf (BufferedImage. imgw imgh BufferedImage/TYPE_INT_RGB)
        gfx (.createGraphics buf)]
    (.setPaint gfx Color/WHITE)
    (.fillRect gfx 0 0 (dec imgw) (dec imgh))
    (.setPaint gfx Color/BLACK)
    { :grid-dims [(norm-fn imgw) (norm-fn imgh)]
      :grid-padding grid-buffer
      :image-dims [imgw imgh]
      :image-buffer buf
      :image-graphic gfx}))

(defn random-rotate
  [point rx ry rz]
  (->> point
    (matrix/mmul (x-rot-matrix (* Math/PI rx)))
    (matrix/mmul (y-rot-matrix (* Math/PI ry)))
    (matrix/mmul (z-rot-matrix (* Math/PI rz)))))

(defn random-cube
  [lattice]
  (let [pad (lattice :grid-padding)
        gdim (apply min (lattice :grid-dims))
        r (+ pad (rand-int (- gdim (* 2 pad))))
        t (- r)
        [rx ry rz] [(rand) (rand) (rand)]]
    (map #(random-rotate % rx ry rz) (cube r r r t t t))))

(defn draw-lattice
  [lattice points]
  (let [[imgw imgh] (lattice :image-dims)
        [gridw gridh] (lattice :grid-dims)
        gfx (lattice :image-graphic)
        norm-points (map (partial norm-point imgw imgh gridw gridh) points)]
    (reduce (fn [gfx [normx normy normz]]
              (dot gfx normy normx normz)
              gfx) gfx norm-points)))

(defn draw-random-cube
  [w h filename]
  (let [lattice (init-lattice w h)
        points (random-cube lattice)
        img (lattice :image-buffer)]
    (draw-lattice lattice points)
    (ImageIO/write img "png" (File. filename))))

(comment

(draw-random-cube 1000 1000 "/tmp/img.png")

)
