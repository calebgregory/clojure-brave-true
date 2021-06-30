;; chapter reading
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
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))

(defn better-symmetrize-body-parts
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))

(defn hit
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts
           accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

;; exercise 1
[:string (= (str "this" " string " "will" " be " "concatenated")
            "this string will be concatenated")
 :vector (= (vector "this" "is" 4 "vector...")
            ["this" "is" 4 "vector..."])
 :list (= (list "this" "is" :a "list")
          '("this" "is" :a "list"))
 :hash-map (= (hash-map :this "is" :a "hash-map")
              {:this "is" :a "hash-map"})
 :hast-set (= (hash-set :this :is :a :hash-set)
              #{:this :is :a :hash-set})]

;; exercise 2
(def add-100 (partial + 100))
(add-100 1)

;; exercise 3
(defn dec-maker [x] #(- % x))
((dec-maker 1) 100)

;; exercise 4
(defn mapset
  [f xs]
  (into #{} (map f (set xs))))
(mapset inc [1 1 2 2])

;; exercise 5
(defn symmetrize-body-parts
  [asym-body-parts symmetrizer]
  (reduce (fn [acc part]
            (into acc (symmetrizer part)))
          []
          asym-body-parts))

(defn matching-parts
  [{:keys [name size] :as part}]
  (if (re-find #"^left-" name)
    (map (fn [i] {:name (clojure.string/replace name #"^left-" (str i "-"))
                  :size size})
         (range 5))
    [part]))

(def radially-symmetric-body-parts
  (symmetrize-body-parts asym-hobbit-body-parts #(matching-parts %)))
radially-symmetric-body-parts
