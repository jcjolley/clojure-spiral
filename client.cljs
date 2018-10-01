(ns spiral.client
  (:require [reagent.core :as r]))

(defn get-width [n] (-> n Math/sqrt Math/ceil))
(defn get-height [n] (-> n Math/sqrt Math/round))

;(get-height 5)

(defn create-n-vectors [n init]
  (if (= n 0)
    init
    (recur (dec n) (conj init []))))

;(create-n-vectors 2 [])


(defn prepend [x v] 
  (into (if (vector? x) x [x]) v))
(prepend 1 [2 3 4 5])
(prepend [1 2] [3 4 5])

(defn append [x v]
  (into v (if (vector? x) x [x])))

;(append 5 [1 2 3 4])
;(append [3 4 5] [1 2])

(defn transition-spiral [isIncreasing val row spiral]
  (if ((not nil?) val)
    (let [update-fn (if isIncreasing append prepend)
          oldRow (get spiral row)
          newRow (update-fn val oldRow)]
      (assoc spiral row newRow))
    spiral))
  

;(transition-spiral true nil 0 [[] [1 2]])
;(transition-spiral false 6 1 [[5 4 3] [1 2]])

(defn transition 
  [isIncreasing row values spiral]
  (print "transition" "incr" isIncreasing "row" row "vals" values "spiral" spiral)
  (if (empty? values)
    spiral
    (let [newRow ((if isIncreasing + -) row 1)
          val (first values)
          newValues (rest values)
          newSpiral (transition-spiral isIncreasing val row spiral)]
      (recur isIncreasing newRow newValues newSpiral)))) 

;(transition true 0 [] [[] [1 2]])
;(transition false 1 [6] [[5 4 3] [1 2]])

(defn write-spiral-row [isIncreasing rowNum val spiral]
  (if ((not nil?) val)
    (let [update-fn (if isIncreasing append prepend)
          oldRow (get spiral rowNum)
          newRow (update-fn val oldRow)
          newSpiral (assoc spiral rowNum newRow)]
      newSpiral)
    spiral))

(defn write-spiral [isIncreasing rowNum values spiral]
  (print "writing" "incr" isIncreasing "row" rowNum "vals" values "spiral" spiral)
  (if (empty? values)
    spiral
    (let [val (first values)
          newValues (rest values)
          newSpiral (write-spiral-row isIncreasing rowNum val spiral)]
      (recur isIncreasing rowNum newValues newSpiral))))

;(write-spiral true 0 [1 2] [[]])
;(write-spiral false 0 [3 4 5] [[] [1 2]])
    
(defn build-spiral 
  "the workhorse"
  [my-seq n isTransition isIncreasing startRow numWrite numTransition i spiral]
  (if (> i n)    
    (reverse spiral)
    (if isTransition
      (let [values (take numTransition (drop i my-seq))
            newIsIncreasing (not isIncreasing)
            newStartRow ((if isIncreasing + -) startRow numTransition)
            newI (+ i (count values))
            newSpiral (transition isIncreasing startRow values spiral)
            newNumTransition (inc numTransition)]
        (recur my-seq n false newIsIncreasing newStartRow numWrite newNumTransition newI newSpiral))
      (let [values (take numWrite (drop i my-seq))
            newI (+ i (count values))
            newStartRow ((if isIncreasing + -) startRow 1)
            newSpiral (write-spiral isIncreasing startRow values spiral)
            newNumWrite (inc numWrite)]
        (recur my-seq n true isIncreasing newStartRow newNumWrite numTransition newI newSpiral))))) 

      
(defn build-spiral-2
  "the workhorse"
  [my-seq n isTransition isIncreasing startRow numWrite numTransition i spiral]
  (print (remove empty? (reverse spiral)))
  (if (> i n)    
    (remove empty? (reverse spiral))
    (let [values (take (if isTransition numTransition numWrite) (drop i my-seq))
          newIsIncreasing (if isTransition (not isIncreasing) isIncreasing)
          newStartRow ((if isIncreasing + -) startRow (if isTransition numTransition 1))
          newI (+ i (count values))
          newSpiral ((if isTransition transition write-spiral) isIncreasing startRow values spiral)
          newNumTransition (min (- (inc n) i) (if isTransition (inc numTransition) numTransition))
          newNumWrite (min (- (inc n) i) (if isTransition numWrite (inc numWrite)))]
      (recur my-seq n (not isTransition) newIsIncreasing newStartRow newNumWrite newNumTransition newI newSpiral))))

      
(defn generate-spiral
  "A function that generates a spiral with the provided sequence"
  [my-seq n]
  (let [height (get-height n)
        spiral (create-n-vectors height [])
        startRow (Math.floor (/ height 2))]
    (print "starting spiral is" spiral)
    (build-spiral-2 my-seq n false true startRow 2 0 1 spiral)))

(def odd-numbers (iterate (partial + 2) 1))
(def top-right-corner (map first (iterate (fn [[val delta]] [(+ delta val) (+ delta 8)]) [1 2])))
(def bottom-left-corner (map first (iterate (fn [[val delta]] [(+ delta val) (+ delta 8)]) [1 6])))

(defn get-sorta-ring [n]
  (let [height (get-height n)]
    (count (take-while #(> height %1) odd-numbers)))) 

(defn get-pad-fn [n]
  (let [ring (get-sorta-ring n)
        bottom-left (nth bottom-left-corner ring)
        top-right (nth top-right-corner (inc ring))]
    (if (and (>= n bottom-left) (< n top-right))
      append
      prepend)))

(defn pad-spiral [width spiral]
  (let [update-fn (get-pad-fn (count (flatten spiral)))]
     (map 
       (fn [row] 
         (if (< (count row) width) 
           (update-fn (take (- width (count row)) (repeat nil)) row) 
           row)) 
       spiral)))

(defn print-spiral [spiral]
  [:div
    (map (fn [row] [:div (map #([:span (str % )] row))]))])
(defn get-max-width [spiral]
  (reduce #(max %1 (count %2)) 0 spiral))

; stores the click count
(def spiral (r/atom (generate-spiral (range) 21)))

; reagent component to be rendered
(defn content []
  [:div "Spiral: "
    [:div (for [row (pad-spiral (get-max-width @spiral) @spiral)] ^{:key row} [:div {:style {:height "55px"}} (for [item row] ^{:key item} [:div {:style {:margin "2px" :width "50px" :height "50px" :background-color "blue" :float "left" :color "white" :text-align "center" :line-height "50px"}} item])])]])


; tells reagent to begin rendering
(r/render-component [content]
  (.querySelector js/document "#app"))




