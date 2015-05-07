(ns yome.core
  (:require
   [om.core :as om :include-macros true]
   [clojure.string :as string]
   [clojure.set :refer [map-invert]]
   [sablono.core :as sab]))

(enable-console-print!)

(defn prevent-> [f]
  (fn [e]
    (.preventDefault e)
    (f)))

(defn prevent->value [f]
  (fn [e]
    (.preventDefault e)
    (let [v (.-value (.-target e))]
      (f v))))

(defn change-yome-sides [yome v]
  (assoc yome
         :num-sides v
         :sides (mapv (fn [i]
                       (if-let [s (get-in yome [:sides i])]
                         s
                         {:corner nil
                          :face nil})) (range v))))


(defonce app-state (atom (change-yome-sides {} 8)))

(def code-map {nil "r"
               :door-frame "d"
               :stove-vent "p"
               :zip-door   "z"})

(def decode-map (map-invert code-map))

(defn serialize-yome [yome]
  (str
   (apply str (map (comp #(if % "w" "r") :face) (:sides yome)))
   ":"
   (apply str (map (comp code-map :corner) (:sides yome)))))

(defn deserialize-yome [s]
  (let [[faces corners] (string/split s ":")
        sides (map #(if (= "w" %)
                      {:face :window}
                      {:face nil}) faces)
        sides (mapv (fn [s c]
                     (assoc s :corner (decode-map c)))
                   sides corners)]
    {:num-sides (count sides)
     :sides sides}))

(def round js/Math.round)

(defn side-count [yome]
  (count (:sides yome)))

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
   :end   (radial-point radius end-theta)
   :type :line})

(defn yome-side-line [radius num-sides]
  (let [theta (/ (* 2 js/Math.PI) num-sides)]
    (side-line radius 0 theta)))

(defn points [p-list]
  (string/join " "  (map (comp #(string/join "," %) (juxt :x :y) )
                         p-list)))

(defmulti draw identity)

(defmethod draw :line [_ {:keys [start end]}]
  (sab/html [:line {:x1 (:x start)
                    :y1 (:y start)
                    :x2 (:x end)
                    :y2 (:y end)}]))

(defmethod draw :side [_ yome]
  (sab/html (draw :line (yome-side-line 180 (:num-sides yome)))))

(defmethod draw :window [_ yome]
  (let [theta (yome-theta yome)
        indent (/ theta 6)
        {:keys [start end]}
        (side-line 160 indent (- theta indent))
        mid    (radial-point 100 (/ theta 2))]
    (sab/html [:polygon { :class "yome-window"
                         :key "yome-window"
                         :points (points (list start end mid))}])))

(defmethod draw :door-frame [_ yome]
  (let [theta (yome-theta yome)
        indent (* 2.2 (/ theta 6))
        door-top (side-line 165 indent (- theta indent))
        door-bottom (side-line 90 indent (- theta indent))]
    (sab/html [:polygon {:class "yome-door"
               :key "yome-door"
               :points (points (list (:start door-top) (:end door-top)
                                     (:end door-bottom) (:start door-bottom)))
               :transform (str "rotate(-" (round (/ (yome-deg yome) 2)) ", 0, 0)")}])))

(defmethod draw :zip-door [_ yome]
  (let [theta (yome-theta yome)
        indent (* 0.15 (/ theta 6))
        zips (map (fn [x]
                    (side-line (- 170 (* 10 x))
                               indent
                               (- indent)))
                  (range 9))]
    [:g {:class "yome-zip-door"
         :key "yome-zip-door"}
     (map (partial draw :line)
          (cons {:type :line
                 :start (radial-point 180 0)
                 :end   (radial-point 80 0)} zips))]))

(defmethod draw :stove-vent [_ yome]
  (let [theta (yome-theta yome)
        point (radial-point 155 0)]
    [:ellipse {:cx (:x point) :cy (:y point) :rx 14 :ry 8
               :class "yome-stove-vent"
               :key "yome-stove-vent"}]))

(defn yome-side [yome index]
  (let [num-sides (:num-sides yome)
        {:keys [corner face]} (get-in yome [:sides index])]
    (sab/html [:g {:transform (str "rotate("
                                   (round (* (yome-deg yome) index))
                                   ", 0, 0)")
                   :class "yome-side"
                   :key (str "yome-side-" index)}
               (cons
                (draw :side yome)
                (map #(draw % yome)
                     (keep identity [corner face])))])))

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

(defn control-visible? [yome type]
  (condp = type
    :zip-door   (addable-door? yome)
    :door-frame (addable-door? yome)
    :stove-vent (addable-stove-vent? yome)
    false))

(defn corner-controls-to-render [yome {:keys [corner] :as side} index]
  (map (fn [x]
         {:op (if corner
                (if (= corner x) :remove :hidden)
                (if (control-visible? yome x) :add :hidden))
          :item x
          :type :corner
          :index index})
       base-corner-controls))

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
                   :key (name (:item corner-control))
                   :className (name (:op corner-control))
                   :onClick (fn [] (corner-transition corner-control))}
               (control-to-string corner-control)])
            (corner-controls-to-render yome side index))]])))

(defn face-controls [yome side index]
  (let [theta (* (yome-theta yome) (+ 1 index))
        pos (radial-point 200 theta)]
    (sab/html
     [:div.face-controls { :style {:position "absolute"
                                   :top (:y pos)
                                   :left (:x pos)}}
      [:div.face-controls-offset
       (let [ctl {:op (if (:face side) :remove :add)
                  :item :window
                  :type :face
                  :index index}]
         [:a {:href "#"
              :class (str (name (:op ctl))
                          " "
                          (name (:item ctl)))
              :onClick (prevent-> (fn [] (corner-transition ctl)))}
          (control-to-string ctl)])]])))

(defn side-controls [yome index]
  (let [side (get-in yome [:sides index])]
    (sab/html [:div
               (corner-controls yome side index)
               (face-controls yome side index)])))

(defn draw-yome-controls [yome]
    (sab/html [:div.yome-controls (map (partial side-controls yome) (range (:num-sides yome)))]))

(defn select [n]
  (sab/html
   [:select.yome-type-select
    {:value    n
     :onChange (prevent->value (fn [v]
                                 (swap! app-state change-yome-sides v)))}
    (map-indexed
     (fn [i y]
       [:option {:value (+ 6 i)} y])
     ["HexaYome" "SeptaYome" "OctaYome"])]))

(defn yome [state]
  (sab/html
   [:div.yome-widget
    [:div
     (select (:num-sides state))]
    [:div {:style {:position "relative" :height 500 :width 500}}
     [:svg {:class "yome" :height 500 :width 500
            :viewBox "-250 -250 500 500"
            :preserveAspectRatio "xMidYMid meet" }
      (draw-yome state)]
     (draw-yome-controls state)]
    [:div [:input {:type "text"
                   :value (serialize-yome state)
                   :onChange (prevent->value (fn [v]
                                               (swap! app-state merge (deserialize-yome v))))}]]]))

(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_] (yome data))))
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  (swap! app-state update-in [:__figwheel_counter] inc)) 

