(var entry @[])

(let [fl (file/open "./day4/input")]
  (loop [line :in (file/lines fl)]
    (array/push entry (string/trim line))))

(set entry (map string-to-chars entry))

(def xmas (string/bytes "XMAS"))

(def width (length (get entry 0)))
(def height (length entry))

# part 1
(defn get-h-check [x y]
  [(get-in entry [y       x] 0)
   (get-in entry [y (+ x 1)] 0)
   (get-in entry [y (+ x 2)] 0)
   (get-in entry [y (+ x 3)] 0)])

(defn get-y-check [x y]
  [(get-in entry [y       x] 0)
   (get-in entry [(+ y 1) x] 0)
   (get-in entry [(+ y 2) x] 0)
   (get-in entry [(+ y 3) x] 0)])

(defn get-r-check [x y]
  [(get-in entry [(+ y 0) (+ x 0)] 0)
   (get-in entry [(+ y 1) (+ x 1)] 0)
   (get-in entry [(+ y 2) (+ x 2)] 0)
   (get-in entry [(+ y 3) (+ x 3)] 0)])

(defn get-l-check [x y]
  [(get-in entry [(- y 0) (+ x 0)] 0)
   (get-in entry [(- y 1) (+ x 1)] 0)
   (get-in entry [(- y 2) (+ x 2)] 0)
   (get-in entry [(- y 3) (+ x 3)] 0)])

(defn is-xmas [ch]
  (let [s (string/from-bytes (splice ch))]
    (or (= s "XMAS")
        (= s "SAMX"))))

# count
(var count 0)
(loop [x :in (range width)]
  (loop [y :in (range height)]
    (let [h-check (get-h-check x y)
          y-check (get-y-check x y)
          r-check (get-r-check x y)
          l-check (get-l-check x y)]
      (if (is-xmas h-check) (set count (+ count 1)))
      (if (is-xmas y-check) (set count (+ count 1)))
      (if (is-xmas r-check) (set count (+ count 1)))
      (if (is-xmas l-check) (set count (+ count 1))))))

(pp count)

# part 2
(defn get-cross [x y]
  [[(get-in entry [(- y 1) (- x 1)] 0) (get-in entry [(+ y 1) (+ x 1)] 0)]
   [(get-in entry [(- y 1) (+ x 1)] 0) (get-in entry [(+ y 1) (- x 1)] 0)]])

(defn cross-as-str [cs]
  (map (fn [c] (string/from-bytes (splice c))) cs))

(defn check-cross [x y]
 (every? (map (fn [s] (or (= s "MS") (= s "SM"))) (cross-as-str (get-cross x y)))))

(set count 0)
(loop [x :in (range width)]
  (loop [y :in (range height)]
    (if (= "A" (string/from-bytes (get-in entry [y x] 0)))
      (if (check-cross x y)
        (set count (+ count 1))))))
(pp count)
