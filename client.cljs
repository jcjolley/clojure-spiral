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
;(prepend 1 [2 3 4 5])
;(prepend [1 2] [3 4 5])

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
  (if (empty? values)
    (do
      ;(print "Spiral after transition" spiral)
      spiral)
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
        (print "values" values, "i" i "n" n)
        (recur my-seq n true isIncreasing newStartRow newNumWrite numTransition newI newSpiral)))))  
      

(defn generate-spiral
  "A function that generates a spiral with the provided sequence"
  [my-seq n]
  (let [height (get-height n)
        spiral (create-n-vectors height [])
        startRow (Math.floor (/ height 2))]
    (build-spiral my-seq n false true startRow 2 0 1 spiral)))

(defn pad-spiral [width spiral]
  (map 
    (fn [row] 
       (if (< (count row) width) 
         (append (take (- width (count row)) (repeat nil)) row) 
         row)) 
    spiral))

(print (generate-spiral (range) 7))

(defn print-spiral [spiral]
  [:div
    (map (fn [row] [:div (map #([:span (str % )] row))]))])
(defn get-max-width [spiral]
  (reduce #(max %1 (count %2)) 0 spiral))

; stores the click count
(def spiral (r/atom (generate-spiral (range) 8)))
; reagent component to be rendered
(defn content []
  [:div "Spiral: "
   [:div (for [row (pad-spiral (get-max-width @spiral) @spiral)] ^{:key row} [:div {:style {:height "55px"}} (for [item row] ^{:key item} [:div {:style {:margin "2px" :width "50px" :height "50px" :background-color "blue" :float "left" :color "white" :text-align "center" :line-height "50px"}} item])])]])
  


; tells reagent to begin rendering
(r/render-component [content]
  (.querySelector js/document "#app"))

