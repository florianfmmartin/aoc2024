(var input @"")
(let [fl (file/open "./day2/input" :r)]
  (file/read fl :all input))

(var reports
  (->> input
    (string/split "\n")
    (map (fn [l] 
      (map scan-number (string/split " " l))))))

(defn get-steps [r]
  (var steps @[])
  (loop [i :in (range (- (length r) 1))]
    (array/push steps
      (- (get r (+ i 1))
        (get r i))))
  steps)

(var report-steps (filter (fn [l] (not= 0 (length l))) (map get-steps reports)))

(defn is-gradual [s]
  (let [dir   (if (> (get s 0) 0) :up :down)
        allow (if (= dir :up) [1 2 3] [-1 -2 -3])]
    (reduce
      (fn [acc x] (and acc (or (= x (get allow 0))
                               (= x (get allow 1))
                               (= x (get allow 2)))))
      true
      s)))

# solution 1
(reduce
  (fn [acc el]
    (if (is-gradual el)
      (+ acc 1)
      acc))
  0
  report-steps)

(defn get-holes [r]
  (var holes @[r])
  (loop [i :keys r]
    (var hole @[])
    (loop [j :keys r]
      (if (not= i j)
        (array/push hole (get r j))))
    (array/push holes hole))
  holes)

(var report-holes (map get-holes reports))
(var holes-steps (map (fn [hs] (map get-steps hs)) report-holes))
(array/pop holes-steps)
(var holes-gradual (map (fn [hs] (map is-gradual hs)) holes-steps))

(defn l-or [l]
  (reduce (fn [a e] (or a e)) false l))

(var holes-okay (map l-or holes-gradual))
(reduce
  (fn [acc el]
    (if el
      (+ acc 1)
      acc))
  0
  holes-okay)

(doc reduce)
