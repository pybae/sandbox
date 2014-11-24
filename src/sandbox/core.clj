(ns sandbox.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "I'm a little teapot!"))

(defn train
  []
  (println "Choo choo!"))

(defn codger-communication
  [whippersnapper]
  (str "Get off my lawn, " whippersnapper "!!!"))

(defn codger
  [& whippersnappers]
  (map codger-communication whippersnappers))

(defn inc-maker
  "Create a custom incrementor"
  [inc-by]
  #(+ % inc-by))

(def inc3 (inc-maker 3))

;; an example of using a closure
;; inc-maker 3 has access to inc-by, even though it is called outside of inc-maker

;; hobbit code

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

(defn needs-matching-part?
  [part]
  (re-find #"^left-" (:name part)))

(defn make-matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts
            final-body-parts (conj final-body-parts part)]
        (if (needs-matching-part? part)
          (recur remaining (conj final-body-parts (make-matching-part part)))
          (recur remaining final-body-parts))))))

(defn better-symmetrize-body-parts
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (let [final-body-parts (conj final-body-parts part)]
              (if (needs-matching-part? part)
                (conj final-body-parts (make-matching-part part))
                final-body-parts)))
          []
          asym-body-parts))

(defn hit
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (inc (rand body-part-size-sum))]
    (println target)
    (loop [[part & rest] sym-parts
           accumulated-size (:size part)]
      (println accumulated-size)
      (if (> accumulated-size target)
        part
        (recur rest (+ accumulated-size (:size part)))))))


(defn my-last-func
  [my-list]
  (first (reverse my-list)))

(defn my-last-func-better
  [my-list]
  (nth my-list (dec (count my-list))))

;; (my-last-func [1 2 3 4 5])
;; (last [1 2 3 4 5])

;; The first problem of Project Euler
;; Sum of all the numbers from 0 to 1000, multiples of 3 and 5

;; slow implementation:

(defn project-euler-first-problem
  []
  (- (+ (reduce + (range 0 1000 3))
        (reduce + (range 0 1000 5)))
     (reduce + (range 0 1000 15))))

(project-euler-first-problem)

;; Fast implementation:

;; Uses the n(n+1)/2 statement
(defn calculate-sum
  [start end step]
  (let [difference (quot (- end (inc start)) step)]
    (quot (* step (* difference (inc difference))) 2)))
  
(def project-euler-fast-first-problem
  (- (+ (calculate-sum 0 1000 3)
        (calculate-sum 0 1000 5))
     (calculate-sum 0 1000 15)))

;; The second problem of Project Euler
;; This is a lazy sequence containing all fibonacci numbers
;; This is amazing :)

(def fibs
  (letfn
      [(fibsrec [a b]
         (lazy-seq (cons a (fibsrec b (+ a b)))))]
    (fibsrec 1 2)))

(defn fibs-until
  [max]
  (take-while #(<= % max) fibs))

(def project-euler-second-problem
  (reduce + (filter even? (fibs-until 4000000))))

;; The third problem of Project Euler

(defn prime-factors
  [num]
  (loop [current 2
         factors []
         n num]
    (if (zero? (mod n current))
      (recur current (conj factors current) (/ n current))
      (if (> (* current current) n)
        (conj factors n)
        (recur (inc current) factors n)))))

(def project-euler-third-problem
  (apply max (prime-factors 600851475143)))

