(ns diamond-square.core
  (:require [clojure.core.reducers :as r]))

; == The Goal ==
; Create a fractal terrain generator using clojure

; == The Algorithm ==
; Diamond-Square. We start with a grid of points, each with a height of 0.
;
; 1. Take each corner point of the square, average the heights, and assign that
;    to be the height of the midpoint of the square. Apply some random error to
;    the midpoint.
;
; 2. Creating a line from the midpoint to each corner we get four half-diamonds.
;    Average the heights of the points (with some random error) and assign the
;    heights to the midpoints of the diamonds.
;
; 3. We now have four square sections, start at 1 for each of them (with
;    decreasing amount of error for each iteration).
;
; This picture explains it better than I can:
; https://raw2.github.com/mediocregopher/diamond-square/master/resources/dsalg.png
; (http://nbickford.wordpress.com/2012/12/21/creating-fake-landscapes/dsalg/)
;
; == The Strategy ==
; We begin with a vector of vectors of numbers, and iterate over it, filling in
; spots as they become available. Our grid will have the top-left being (0,0),
; y being pointing down and x going to the right. The outermost vector
; indicating row number (y) and the inner vectors indicate the column number (x)
;
; = Utility =
; First we create some utility functions for dealing with vectors of vectors.

(defn print-m
  "Prints a grid in a nice way"
  [m]
  (doseq [n m]
    (println n)))

(defn get-m
  "Gets a value at the given x,y coordinate of the grid, with [0,0] being in the
  top left"
  [m x y]
  ((m y) x))

(defn set-m
  "Sets a value at the given x,y coordinat of the grid, with [0,0] being in the
  top left"
  [m x y v]
  (assoc m y
    (assoc (m y) x v)))

(defn add-m
  "Like set-m, but adds the given value to the current on instead of overwriting
  it"
  [m x y v]
  (set-m m x y
   (+ (get-m m x y) v)))

(defn avg
  "Returns the truncated average of the given list"
  [l]
  (int (/ (reduce + l) (count l))))

(defmacro tc
  "Measures the time taken to run the given body. Returns a vector of the number
  of milliseconds and the return from the body"
  [& body]
  `(let [t1#  (System/currentTimeMillis)
         res# (do ~@body)
         t2#  (System/currentTimeMillis)]
    [(- t2# t1#) res#]))

; = Grid size =
; Since we're starting with a blank grid we need to find out what sizes the
; grids can be. For convenience the size (height and width) should be odd, so we
; easily get a midpoint. And on each iteration we'll be halfing the grid, so
; whenever we do that the two resultrant grids should be odd and halfable as
; well, and so on.
;
; The algorithm that fits this is size = 2^n + 1, where 1 <= n. For the rest of
; this guide I'll be referring to n as the "degree" of the grid.

(def exp2-pre-compute
  (vec (map #(int (Math/pow 2 %)) (range 31))))

(defn exp2
  "Returns 2^n as an integer. Uses pre-computed values since we end up doing
  this so much"
  [n]
  (exp2-pre-compute n))

(def grid-sizes
  (vec (map #(inc (exp2 %)) (range 1 31))))

(defn grid-size [degree]
  (inc (exp2 degree)))

; Available grid heights/widths are as follows:
;[3 5 9 17 33 65 129 257 513 1025 2049 4097 8193 16385 32769 65537 131073
;262145 524289 1048577 2097153 4194305 8388609 16777217 33554433 67108865
;134217729 268435457 536870913 1073741825])

(defn blank-grid
  "Generates a grid of the given degree, filled in with zeros"
  [degree]
  (let [gsize (grid-size degree)]
    (vec (repeat gsize
      (vec (repeat gsize 0))))))

(comment
  (print-m (blank-grid 3))
)

; = Coordinate Pattern (The Tricky Part) =
; We now have to figure out which coordinates need to be filled in on each pass.
; A pass is defined as a square step followed by a diamond step. The next pass
; will be the square/dimaond steps on all the smaller squares generated in the
; pass. It works out that the number of passes required to fill in the grid is
; the same as the degree of the grid, where the first pass is 1.
;
; So we can easily find patterns in the coordinates for a given degree/pass,
; I've laid out below all the coordinates for each pass for a 3rd degree grid
; (which is 9x9).

; Degree 3 Pass 1 Square
; [. . . . . . . . .]
; [. . . . . . . . .]
; [. . . . . . . . .]
; [. . . . . . . . .]
; [. . . . 1 . . . .] (4,4)
; [. . . . . . . . .]
; [. . . . . . . . .]
; [. . . . . . . . .]
; [. . . . . . . . .]

; Degree 3 Pass 1 Diamond
; [. . . . 2 . . . .] (4,0)
; [. . . . . . . . .]
; [. . . . . . . . .]
; [. . . . . . . . .]
; [2 . . . . . . . 2] (0,4) (8,4)
; [. . . . . . . . .]
; [. . . . . . . . .]
; [. . . . . . . . .]
; [. . . . 2 . . . .] (4,8)

; Degree 3 Pass 2 Square
; [. . . . . . . . .]
; [. . . . . . . . .]
; [. . 3 . . . 3 . .] (2,2) (6,2)
; [. . . . . . . . .]
; [. . . . . . . . .]
; [. . . . . . . . .]
; [. . 3 . . . 3 . .] (2,6) (6,6)
; [. . . . . . . . .]
; [. . . . . . . . .]

; Degree 3 Pass 2 Diamond
; [. . 4 . . . 4 . .] (2,0) (6,0)
; [. . . . . . . . .]
; [4 . . . 4 . . . 4] (0,2) (4,2) (8,2)
; [. . . . . . . . .]
; [. . 4 . . . 4 . .] (2,4) (6,4)
; [. . . . . . . . .]
; [4 . . . 4 . . . 4] (0,6) (4,6) (8,6)
; [. . . . . . . . .]
; [. . 4 . . . 4 . .] (2,8) (6,8)

; Degree 3 Pass 3 Square
; [. . . . . . . . .]
; [. 5 . 5 . 5 . 5 .] (1,1) (3,1) (5,1) (7,1)
; [. . . . . . . . .]
; [. 5 . 5 . 5 . 5 .] (1,3) (3,3) (5,3) (7,3)
; [. . . . . . . . .]
; [. 5 . 5 . 5 . 5 .] (1,5) (3,5) (5,5) (7,5)
; [. . . . . . . . .]
; [. 5 . 5 . 5 . 5 .] (1,7) (3,7) (5,7) (7,7)
; [. . . . . . . . .]

; Degree 3 Pass 3 Diamond
; [. 6 . 6 . 6 . 6 .] (1,0) (3,0) (5,0) (7,0)
; [6 . 6 . 6 . 6 . 6] (0,1) (2,1) (4,1) (6,1) (8,1)
; [. 6 . 6 . 6 . 6 .] (1,2) (3,2) (5,2) (7,2)
; [6 . 6 . 6 . 6 . 6] (0,3) (2,3) (4,3) (6,3) (8,3)
; [. 6 . 6 . 6 . 6 .] (1,4) (3,4) (5,4) (7,4)
; [6 . 6 . 6 . 6 . 6] (0,5) (2,5) (4,5) (6,5) (8,5)
; [. 6 . 6 . 6 . 6 .] (1,6) (3,6) (5,6) (7,6)
; [6 . 6 . 6 . 6 . 6] (0,7) (2,7) (4,7) (6,7) (8,7)
; [. 6 . 6 . 6 . 6 .] (1,8) (3,8) (5,8) (7,8)
;
; I make two different functions, one to give the coordinates for the square
; portion of each pass and one for the diamond portion of each pass. To find the
; actual patterns it was useful to first look only at the pattern in the
; y-coordinates, and figure out how that translated into the pattern for the
; x-coordinates.

(defn grid-square-coords
  "Given a grid degree and pass number, returns all the coordinates which need
  to be computed for the square step of that pass"
  [degree pass]
  (let [gsize (grid-size degree)
        start (exp2 (- degree pass))
        interval (* 2 start)
        coords (map #(+ start (* interval %))
                (range (exp2 (dec pass))))]
    (r/mapcat (fn [y]
      (r/map #(vector % y) coords))
      coords)))
;
; (grid-square-coords 3 2)
; => ([2 2] [6 2] [2 6] [6 6])

(defn grid-diamond-coords
  "Given a grid degree and a pass number, returns all the coordinates which need
  to be computed for the diamond step of that pass"
  [degree pass]
  (let [gsize (grid-size degree)
        interval (exp2 (- degree pass))
        num-coords (grid-size pass)
        coords (map #(* interval %) (range 0 num-coords))]
    (r/mapcat (fn [y]
      (if (even? (/ y interval))
        (r/map #(vector % y) (take-nth 2 (drop 1 coords)))
        (r/map #(vector % y) (take-nth 2 coords))))
      coords)))

; (grid-diamond-coords 3 2)
; => ([2 0] [6 0] [0 2] [4 2] [8 2] [2 4] [6 4] [0 6] [4 6] [8 6] [2 8] [6 8])

; = Height Generation =
; We now work on functions which, given a coordinate, will return what value
; coordinate will have.

(defn avg-points
  "Given a grid and an arbitrary number of points (of the form [x y]) returns
  the average of all the given points that are on the map. Any points which are
  off the map are ignored"
  [m coords]
  (let [grid-size (count m)]
    (avg
      (map #(get-m m (first %) (second %))
        (filter
          (fn [[x y]]
            (and (< -1 x) (> grid-size x)
                 (< -1 y) (> grid-size y)))
          coords)))))

(defn error
  "Returns a number between -e and e, inclusive"
  [e]
  (- (rand-int (inc (* 2 e))) e))

; The next function is a little weird. It primarily takes in a point, then
; figures out the distance from that point to the points we'll take the average
; of. The locf (locator function) is used to return back the actual points to
; use. For the square portion it'll be the points diagonal from the given one,
; for the diamond portion it'll be the points to the top/bottom/left/right from
; the given one.
;
; Once it has those points, it finds the average and applies the error. The
; error function is nothing more than a number between -interval and +interval,
; where interval is the distance between the given point and one of the averaged
; points. It is important that the error decreases the more passes you do, which
; is why the interval is used.
;
; The error function is what should be messed with primarily if you want to
; change what kind of terrain you generate (a giant mountain instead of
; hills/valleys, for example). The one we use is uniform for all intervals, so
; it generates a uniform terrain.

(defn- grid-fill-point
  [locf m degree pass x y]
  (let [interval (exp2 (- degree pass))
        leftx  (- x interval)
        rightx (+ x interval)
        upy    (- y interval)
        downy  (+ y interval)
        v      (avg-points m
                (locf x y leftx rightx upy downy))]
    (add-m m x y (+ v (error interval)))))

(def grid-fill-point-square
  "Given a grid, the grid's degree, the current pass number, and a point on the
  grid, fills in that point with the average (plus some error) of the
  appropriate corner points, and returns the resultant grid"
  (partial grid-fill-point
    (fn [_ _ leftx rightx upy downy]
      [[leftx upy]
       [rightx upy]
       [leftx downy]
       [rightx downy]])))

(def grid-fill-point-diamond
  "Given a grid, the grid's degree, the current pass number, and a point on the
  grid, fills in that point with the average (plus some error) of the
  appropriate edge points, and returns the resultant grid"
  (partial grid-fill-point
    (fn [x y leftx rightx upy downy]
      [[leftx y]
       [rightx y]
       [x upy]
       [x downy]])))

; = Filling in the Grid =
; We finally compose the functions we've been creating to fill in the entire
; grid

(defn- grid-fill-point-passes
  "Given a grid, a function to fill in coordinates, and a function to generate
  those coordinates, fills in all coordinates for a given pass, returning the
  resultant grid"
  [m fill-f coord-f degree pass]
  (r/fold
    (fn ([] m)
        ([macc [x y]] (fill-f macc degree pass x y)))
    (coord-f degree pass)))

(defn grid-pass
  "Given a grid and a pass number, does the square then the diamond portion of
  the pass"
  [m degree pass]
  (-> m
    (grid-fill-point-passes
      grid-fill-point-square grid-square-coords degree pass)
    (grid-fill-point-passes
      grid-fill-point-diamond grid-diamond-coords degree pass)))

; The most important function in this guide, does all the work
(defn terrain
  "Given a grid degree, generates a uniformly random terrain on a grid of that
  degree"
  ([degree]
    (terrain (blank-grid degree) degree))
  ([m degree]
    (reduce
      #(grid-pass %1 degree %2)
      m
      (range 1 (inc degree)))))

(comment
  (print-m
    (terrain 5))

  (first (tc
    (terrain 9)))
)

; == The Results ==
; We now have a generated terrain, probably. We should check it. First we'll
; create an ASCII representation. But to do that we'll need some utility
; functions.

(defn max-terrain-height
  "Returns the maximum height found in the given terrain grid"
  [m]
  (reduce max
    (map #(reduce max %) m)))

(defn min-terrain-height
  "Returns the minimum height found in the given terrain grid"
  [m]
  (reduce min
    (map #(reduce min %) m)))

(defn norm
  "Given x in the range (A,B), normalizes it into the range (0,new-height)"
  [A B new-height x]
  (int (/ (* (- x A) new-height) (- B A))))

(defn normalize-terrain
  "Given a terrain map and a number of \"steps\", normalizes the terrain so all
  heights in it are in the range (0,steps)"
  [m steps]
  (let [max-height (max-terrain-height m)
        min-height (min-terrain-height m)
        norm-f (partial norm min-height max-height steps)]
    (vec (map #(vec (map norm-f %)) m))))

; We now define which ASCII characters we want to use for which heights. The
; vector starts with the character for the lowest height and ends with the
; character for the heighest height.

(def tiles
  [\~ \~ \" \" \x \x \X \$ \% \# \@])

(defn tile-terrain
  "Given a terrain map, converts it into an ASCII tile map"
  [m]
  (vec (map #(vec (map tiles %))
    (normalize-terrain m (dec (count tiles))))))

(comment
  (print-m
    (tile-terrain
      (terrain 5)))

; [~ ~ " " x x x X % $ $ $ X X X X X X $ x x x X X X x x x x " " " ~]
; [" ~ " " x x X X $ $ $ X X X X X X X X X X X X X X x x x x " " " "]
; [" " " x x x X X % $ % $ % $ $ X X X X $ $ $ X X X X x x x x " " "]
; [" " " x x X $ % % % % % $ % $ $ X X $ $ $ $ X X x x x x x x " " x]
; [" x x x x X $ $ # % % % % % % $ X $ X X % $ % X X x x x x x x x x]
; [x x x X $ $ $ % % % % % $ % $ $ $ % % $ $ $ $ X X x x x x x x x x]
; [X X X $ % $ % % # % % $ $ % % % % $ % $ $ X $ X $ X X x x x X x x]
; [$ $ X $ $ % $ % % % % $ $ $ % # % % % X X X $ $ $ X X X x x x x x]
; [% X X % % $ % % % $ % $ % % % # @ % $ $ X $ X X $ X x X X x x x x]
; [$ $ % % $ $ % % $ $ X $ $ % % % % $ $ X $ $ X X X X X X x x x x x]
; [% % % X $ $ % $ $ X X $ $ $ $ % % $ $ X X X $ X X X x x X x x X X]
; [$ $ $ X $ $ X $ X X X $ $ $ $ % $ $ $ $ $ X $ X x X X X X X x X X]
; [$ $ $ $ X X $ X X X X X $ % % % % % $ X $ $ $ X x X X X $ X X $ $]
; [X $ $ $ $ $ X X X X X X X % $ % $ $ $ X X X X X x x X X x X X $ $]
; [$ $ X X $ X X x X $ $ X X $ % X X X X X X X X X x X X x x X X X X]
; [$ $ X X X X X X X $ $ $ $ $ X $ X X X X X X X x x x x x x x X X X]
; [% % % $ $ X $ X % X X X % $ $ X X X X X X x x x x x x x x x X X $]
; [$ % % $ $ $ X X $ $ $ $ $ $ X X X X x X x x x x " x x x " x x x x]
; [$ X % $ $ $ $ $ X X X X X $ $ X X X X X X x x " " " " " " " " x x]
; [$ X $ $ % % $ X X X $ X X X x x X X x x x x x " " " " " ~ " " " "]
; [$ $ X X % $ % X X X X X X X X x x X X X x x x " " " " " " ~ " " "]
; [$ $ X $ % $ $ X X X X X X x x x x x x x x x " " " " " " " " " ~ ~]
; [$ $ $ $ $ X X $ X X X X X x x x x x x x x " " " " " " " ~ " " " ~]
; [$ % X X $ $ $ $ X X X X x x x x x x x x x x " " " " ~ " " ~ " " ~]
; [% $ $ X $ X $ X $ X $ X x x x x x x x x x x " " " " ~ ~ ~ " ~ " ~]
; [$ X X X X $ $ $ $ $ X x x x x x x x x x x " " " " ~ ~ ~ ~ ~ ~ ~ ~]
; [X x X X x X X X X X X X X x x x x x x x x x " " " ~ ~ " " ~ ~ ~ ~]
; [x x x x x x X x X X x X X X x x x x x x x " x " " " " " ~ ~ ~ ~ ~]
; [x x x x x x x x X X X X $ X X x X x x x x x x x x " ~ ~ ~ ~ ~ ~ ~]
; [" x x x x x X x X X X X X X X X X x x x x x x " " " " ~ ~ ~ ~ ~ ~]
; [" " " x x x X X X X $ $ $ X X X X X X x x x x x x x x " " ~ ~ ~ ~]
; [" " " " x x x X X X X X $ $ X X x X X x x x x x x x " " " " " ~ ~]
; [~ " " x x x x X $ X $ X $ $ X x X x x x x x x x x x x x x " " " ~]
)

; = Pictures! =
; ASCII is cool, but pictures are better. First we import some java libraries
; that we'll need, then define the colors for each level just like we did tiles
; for the ascii representation.

(import
  'java.awt.image.BufferedImage
  'javax.imageio.ImageIO
  'java.io.File)

(def colors
  [0x1437AD 0x04859D 0x007D1C 0x007D1C 0x24913C
   0x00C12B 0x38E05D 0xA3A3A4 0x757575 0xFFFFFF])

; Finally we reduce over a BufferedImage instance to output every tile as a
; single pixel on it.

(defn img-terrain
  "Given a terrain map and a file name, outputs a png representation of the
  terrain map to that file"
  [m file]
  (let [img (BufferedImage. (count m) (count m) BufferedImage/TYPE_INT_RGB)]
    (reduce
      (fn [rown row]
        (reduce
          (fn [coln tile]
            (.setRGB img coln rown (colors tile))
            (inc coln))
          0 row)
        (inc rown))
      0 (normalize-terrain m (dec (count colors))))
    (ImageIO/write img "png" (File. file))))

(comment
  (img-terrain
    (terrain 10)
    "resources/terrain.png")

  ; https://raw2.github.com/mediocregopher/diamond-square/master/resources/terrain.png
)

; == Conclusion ==
; There's still a lot of work to be done. The algorithm starts taking a
; non-trivial amount of time around the 10th degree, which is only a 1025x1025px
; image. I need to profile the code and find out where the bottlenecks are. It's
; possible re-organizing the code to use pmaps instead of reduces in some places
; could help.
