(var content @"")
(let [fl (file/open "./day3/input" :r)]
  (file/read fl :all content)
  (set content (peg/replace-all :s+ " " content))
  (file/close fl))

(defn split-by-and-join-with [s k i]
  (slice
    (flatten
      (map (fn [x] [k x])
        (string/split s i))) 1))

(var input (split-by-and-join-with "do()" :do content))
(set input
  (flatten (map
    (fn [x]
      (cond
        (= x :do) x
        (split-by-and-join-with "don't()" :dont x)))
    input)))

(var final @"")
(var is-do true)
(loop [x :in input]
  (cond
    (= x :do)   (set is-do true)
    (= x :dont) (set is-do false)
    is-do
      (set final (string final x)))))
      (pp final)

(def mul-grammar
  '{:num  (number (between 1 3 :d) 10 :my-num)
    :tmul (thru "mul(")
    :mul  (+ (* :tmul :num "," :num ")") :tmul)
    :main (any (+ :mul))})

(defn zip-mul-plus [nums]
  (var total 0)
  (loop [i :in (range 1 (+ 1 (/ (length nums) 2)))]
    (let [x (- (* i 2) 2)
          y (+ x 1)]
      (set total
        (+ total
          (* (get nums x)
             (get nums y))))))
  total)

(defn part-1 [i]
  (zip-mul-plus
    (peg/match mul-grammar i)))

(loop [x :in 
(peg/match mul-grammar input)
] (pp x))

(part-1 input)
(part-1 final)

