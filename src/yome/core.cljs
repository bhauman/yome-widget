(ns yome.core
  (:require
     [om.core :as om :include-macros true]
     [sablono.core :as sab]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defn prevent-> [f]
  (fn [e]
    (.preventDefault e)
    (f)))

(def yome-struct {:num-sides 6
                  :sides [{:corner nil
                           :face nil}
                          {:corner nil
                           :face nil}
                          {:corner nil
                           :face nil}
                          {:corner nil
                           :face nil}
                          {:corner nil
                           :face nil}
                          {:corner nil
                           :face nil}]})

(defonce app-state (atom yome-struct))

(def round js/Math.round)

(defn yome-theta [yome]
  (/ (* 2 js/Math.PI) (:num-sides yome)))

(defn yome-deg [yome]
  (/ 360 (:num-sides yome)))

(defn rotate [theta {:keys [x y] :as point} ]
  (let [sint (js/Math.sin theta)
        cost (js/Math.cos theta)]
    (assoc point
           :x (round (- (* x cost) (* y sint)))
           :y (round (+ (* x sint) (* y cost))))))

(defn radial-point [radius theta]
  (rotate theta {:x 0 :y radius}))

(defn side-line [radius start-theta end-theta]
  {:start (radial-point radius start-theta)
   :end (radial-point radius end-theta)
   :type :line})

(defn yome-side-line [radius num-sides]
  (let [theta (/ (* 2 js/Math.PI) num-sides)]
    (side-line radius 0 theta)))

(defmulti draw :type)

(defmethod draw :default [x] (sab/html [:g]))

(defmethod draw :line [{:keys [start end]}]
  (sab/html [:line {:x1 (:x start)
                    :y1 (:y start)
                    :x2 (:x end)
                    :y2 (:y end)
                    #_:style
                    #_{:stroke "rgb(0,130,240)"
                       :stroke-width "2"} }]))

(defn draw-window [yome]
  (let [theta (yome-theta yome)
        indent (yome-theta {:num-sides (* 6 (:num-sides yome))})
        {:keys [start end] :as side} (side-line 160 
                                                indent
                                                (- theta indent))
        mid (radial-point 100 (* 3 indent))]
    [:g {:class "yome-window"}
     (draw side)
     (draw {:type :line
            :start start
            :end mid})
     (draw {:type :line
            :start mid
            :end end})]))

(defn draw-door-frame [yome]
  (let [theta (yome-theta yome)
        indent (yome-theta {:num-sides (* 6 (:num-sides yome))})
        door-top (side-line 165
                            (* 2.2 indent)
                            (- theta (* 2.2 indent)))
        door-bottom (side-line 90 
                               (* 2.2 indent)
                               (- theta (* 2.2 indent)))]
    [:g {:class "yome-door" :transform (str "rotate(-" (round (/ (yome-deg yome) 2)) ", 0, 0)")}
     (draw door-top)
     (draw door-bottom)
     (draw {:type :line
            :start (:start door-top)
            :end (:start door-bottom)})
     (draw {:type :line
            :start (:end door-top)
            :end (:end door-bottom)})]))

(defn draw-zip-door [yome]
  (let [indent (yome-theta {:num-sides (* 6 (:num-sides yome))})
        zips (map (fn [x]
                    (side-line (- 170 (* 10 x))
                               (* 0.15 indent)
                               (+ 0 (* -0.15 indent))))
                  (range 9))]
    [:g {:class "yome-zip-door"}
     (map draw
          (cons {:type :line
                 :start (radial-point 180 0)
                 :end   (radial-point 80 0)} zips))]))

(defn draw-stove-vent [yome]
  (let [theta (yome-theta yome)
        point (radial-point 155 0)]
    [:ellipse {:cx (:x point) :cy (:y point) :rx 14 :ry 8 :class "yome-stove-vent"}]))

(def draw-map {:window     #'draw-window
               :door-frame #'draw-door-frame
               :zip-door   #'draw-zip-door
               :stove-vent #'draw-stove-vent})

(defn yome-side [yome index]
  (let [num-sides (:num-sides yome)
        rot (round (* (yome-deg yome)
                      index) )
        side (get-in yome [:sides index])]
    (sab/html [:g {:transform (str "rotate(" rot ", 0, 0)")}
               (cons
                (draw (yome-side-line 180 (:num-sides yome)))
                (map (fn [p]
                         ((draw-map p) yome))
                     (keep identity [(:corner side) (:face side)])))])))

(defn draw-yome [yome]
  (sab/html
   [:g {:transform (str "rotate(" (round (/ (yome-deg yome) 2)) ", 0, 0)")}
    (map (partial yome-side yome) (range (:num-sides yome)))]))

(def base-corner-controls [:stove-vent :zip-door :door-frame])

(defn count-item [yome item]
  (count (filter #(= item %) (map :corner (:sides yome)))))

(defn addable-stove-vent? [yome]
  (zero? (count-item yome :stove-vent)))

(defn addable-door? [yome]
  (> 3 (+ (count-item yome :zip-door) (count-item yome :door-frame))))

(defn get-controls-to-add [yome]
  (keep identity
        (concat
         (when (addable-stove-vent? yome) [:stove-vent])
         (when (addable-door? yome)
           [:zip-door :door-frame]))))

(defn corner-controls-to-render [yome side index]
  (if-let [corner (:corner side)]
    [{:op :remove
      :item corner
      :type :corner
      :index index}]
    (map (fn [x]
           {:op :add
            :item x
            :type :corner
            :index index})
         (get-controls-to-add yome))))

(defn control-to-string [{:keys [op item]}]
  (str (if (= op :add) "+" "-") " "
       (name item))) ;; TODO

(defn corner-transition [{:keys [op item type index] :as ctl}]
  (swap! app-state
         (fn [state]
           (let [res (update-in state [:sides index]
                                (fn [side]
                                  (assoc side type
                                         (when (= op :add) item))))]
             (prn state)
             res))))

(defn corner-controls [yome side index]
  (let [theta (+ (* (yome-theta yome) index)
                 (/ (yome-theta yome) 2))
        pos (radial-point 220 theta)]
    (sab/html
     [:div.corner-controls { :style {:position "absolute"
                                     :top (:y pos)
                                     :left (:x pos)}}
      [:div.corner-controls-offset
       (map (fn [corner-control]
              [:a {:href "#"
                   :onClick (fn [] (corner-transition corner-control))}
               (control-to-string corner-control)])
            (corner-controls-to-render yome side index))]])))

(defn face-control-to-render [yome side index]
  {:op (if (:face side) :remove :add)
   :item :window
   :type :face
   :index index})

(defn face-controls [yome side index]
  (let [theta (* (yome-theta yome) (+ 1 index))
        pos (radial-point 200 theta)]
    (sab/html
     [:div.face-controls { :style {:position "absolute"
                                   :top (:y pos)
                                   :left (:x pos)}}
      [:div.face-controls-offset
       (let [ctl (face-control-to-render yome side index)]
         [:a {:href "#"
              :onClick (prevent-> (fn [] (corner-transition ctl)))}
          (control-to-string ctl)])]])))

(defn side-controls [yome index]
  (let [side (get-in yome [:sides index])]
    (sab/html [:div
               (corner-controls yome side index)
               (face-controls yome side index)])))

(defn draw-yome-controls [yome]
    (sab/html [:div.yome-controls (map (partial side-controls yome) (range (:num-sides yome)))]))

(defn yome [state]
  (sab/html
   [:div.yome-widget
    [:svg {:class "yome" :height 500 :width 500
           :viewBox "-250 -250 500 500"
           :preserveAspectRatio "xMidYMid meet" }
     (draw-yome state)]
    (draw-yome-controls state)
    ]
   ))

(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (sab/html [:div
                   (yome data)
                   [:h1 "Hi there!"]]))))
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  (swap! app-state update-in [:__figwheel_counter] inc)) 

