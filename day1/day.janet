# part 1
(var left @[])
(var right @[])

(let [fl (file/open "./day1/input")]
  (loop [line :in (file/lines fl)]
    (let [l (string/split " " line)]
      (array/push left (scan-number (first l)))
      (array/push right (scan-number (string/trim (last l)))))))

(sort left)
(sort right)

(var count 0)

(loop [i :keys left]
  (let [l (get left  i)
        r (get right i)]
    (set count (+ count
      (math/abs (- l r))))))

(pp count)

# part 2
(defn count-in-sorted-right [n]
  (var c 0)
  (label result
    (loop [x :in right]
      (cond
        (= x n)
          (set c (+ c 1))
        (> x n)
          (return result))))
  c)

(var similar 0)

(loop [v :in left]
  (let [n (count-in-sorted-right v)]
    (set similar
      (+ similar
        (* v n)))))

(pp similar)
