(ns yome.core
  (:require
     [om.core :as om :include-macros true]
     [sablono.core :as sab]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(def yome-struct {:num-sides 6
                  :sides [{:corner :door-frame
                           :face :window}
                          {:corner nil
                           :face nil}
                          {:corner nil
                           :face :window}
                          {:corner nil
                           :face nil}
                          {:corner :stove-vent
                           :face :window}
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

(reset! app-state yome-struct)

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
        point (radial-point 150 0)]
    [:circle {:cx (:x point) :cy (:y point) :r 14 :class "yome-stove-vent"}]))

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

(defn corner-controls [yome side index]
  (let [theta (+ (* (yome-theta yome) index)
                 (/ (yome-theta yome) 2))
        pos (radial-point 220 theta)]
    (sab/html
     [:div.corner-controls { :style {:position "absolute"
                                     :top (:y pos)
                                     :left (:x pos)
                                     :width "100px"}}
      [:div.corner-controls-offset
       [:a {:href "#"
            :onChange (fn [] )}
        "+ stove vent"]
       [:a {:href "#"
            :onChange (fn [] )}
        "+ zip door"]
       [:a {:href "#"
            :onChange (fn [] )}
        "+ door frame"]]])))

(defn face-controls [yome side index]
  (let [theta (+ (* (yome-theta yome) index)
                 (/ (yome-theta yome) 2))
        pos (radial-point 220 theta)]
    (sab/html
     [:div.face-controls { :style {:position "absolute"
                                     :top (:y pos)
                                     :left (:x pos)
                                     :width "100px"}}
      [:div.face-controls-offset
       [:a {:href "#"
            :onChange (fn [] )}
        "+ stove vent"]
       [:a {:href "#"
            :onChange (fn [] )}
        "+ zip door"]
       [:a {:href "#"
            :onChange (fn [] )}
        "+ door frame"]]])))

(defn side-controls [yome index]
  (let [side (get-in yome [:sides index])]
    (sab/html [:div (corner-controls yome side index)])))

(defn draw-yome-controls [yome]
    (sab/html [:div.yome-controls (side-controls yome 0)]))


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

