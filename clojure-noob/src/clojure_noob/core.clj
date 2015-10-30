;; Hobbit example

;; Hobbit model
(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn matching-part
  ;; Use a regular expression to replace the left side with the right
  ;; and get the corresponding size
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  ;; loop over the body parts. tail is bound to `remaining-asym-parts`
  ;; (initially `remaining-asym-parts` == `asym-body-parts`)
  ;; result sequence, `final-body-pars` initial value is `[]`
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    ;; If `remaining-asym-parts` is empty, we're done and return result vector
    (if (empty? remaining-asym-parts)
      final-body-parts
      ;; otherwise, associate `part` with the first element of
      ;; `remaining-asym-parts` (head), associate `remaining` with the rest of
      ;; the elements (tail)
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               ;; add elements from below into the vector `final-body-parts`
               ;; simplified: (into [] (set [:a :a])) => [:a]
               (into final-body-parts
                     ;; create a set consisiting of `part` and it's match
                     ;; (bc `part` and `matching-part` may be the same)
                     (set [part (matching-part part)])))))))

;; with a reducer - process elements of a collection to build result
(defn symmetrize-body-parts-reducer
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            ;; anonymous function focuses on processing elem & building result
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))

(defn hit
  "Determine which body part is hit based on its accumulated size"
  [asym-body-parts]
  (let [sym-parts (symmetrize-body-parts-reducer asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts
           accumuated-size (:size part)]
      (if (> accumuated-size target)
        part
        (recur remaining (+ accumuated-size (:size (first remaining))))))))


;; Chapter exercises
(defn inc-100
  "Take a number and add 100 to it"
  [num]
  (+ num 100))

(defn dec-maker
  "Return a function that decreases its input by `dec-by`"
  [dec-by]
  #(- % dec-by))

(defn mapset-maker
  "Function that works like map but returns a set of mapped values"
  [func values]
  (set (map func values)))
