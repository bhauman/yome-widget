(ns yome.core
  (:require
   #_[cljs.pprint :as p]
   [om.core :as om :include-macros true]
   [clojure.string :as string]
   [clojure.set :refer [map-invert intersection]]
   [sablono.core :as sab]
   [cljs.core.async :as async :refer [<!]]   
   [cljs-http.client :as http]
   [goog.object :as gobj])
  (:require-macros [cljs.core.async.macros :as m :refer [go]]))

(enable-console-print!)

(defn l [x]
  (.log js/console x)
  x)

(defn prevent-> [f]
  (fn [e]
    (.preventDefault e)
    (f)))

(defn prevent->value [f]
  (fn [e]
    (.preventDefault e)
    (let [v (.-value (.-target e))]
      (f v))))

(defn prevent->checked [f]
  (fn [e]
    (.preventDefault e)
    (let [v (.-checked (.-target e))]
      (f v))))

(defn change-yome-sides [yome v]
  (assoc yome
         :sides (mapv (fn [i]
                       (if-let [s (get-in yome [:sides i])]
                         s
                         {:corner nil
                          :face nil})) (range v))))

(defonce app-state (atom (change-yome-sides {:form {}} 8)))

;;**************** Begin Yome Code Maker Functions *********************

(def code-map {nil "r"
              :door-frame "d"
              :stove-vent "p"
              :zip-door   "z"})

(def decode-map (map-invert code-map))

(declare convert-options unconvert-options)

(defn serialize-yome [yome]
  (str
   (apply str (map (comp #(if % "w" "r") :face) (:sides yome)))
   ":"
   (apply str (map (comp code-map :corner) (:sides yome)))
   ":"
   (convert-options yome)))

(defn deserialize-yome [s]
  (let [[faces corners binary-code] (string/split s ":")
        sides (map #(if (= "w" %)
                      {:face :window}
                      {:face nil}) faces)
        sides (mapv (fn [s c]
                     (assoc s :corner (decode-map c)))
                    sides corners)
        selected-options (unconvert-options (js/parseInt binary-code))
        selected-options (if (some #(= (:corner %) :stove-vent) sides)
                           (conj selected-options :stove-vent-hole)
                           selected-options)]
    (reduce (fn [a o] (assoc-in a [:form o] true))
            {:sides sides} selected-options)))

;;****************** End Yome Code Maker Functions *********************

;;********************* Begin Feature Count Functions ******************

(def round js/Math.round)

(defn side-count [yome]
  (count (:sides yome)))

(defn window-count [yome]
  (count (filter :face (:sides yome))))

(defn door-frame-count [yome]
  (count (filter #(= % :door-frame) (keep :corner (:sides yome)))))

(defn zip-door-count [yome]
  (count (filter #(= % :zip-door) (keep :corner (:sides yome)))))

(defn stove-vent? [yome]
  (> (count (filter #(= % :stove-vent) (keep :corner (:sides yome))))
     0))

#_(prn (stove-vent? @app-state))

;;********************* End Feature Count Functions ********************

;;**********************Begin Drawing Functions ************************
(defn yome-theta [yome]
  (/ (* 2 js/Math.PI) (side-count yome)))

(defn yome-deg [yome]
  (/ 360 (side-count yome)))

(defn rotate [theta {:keys [x y] :as point} ]
  (let [sint (js/Math.sin theta)
        cost (js/Math.cos theta)]
    (assoc point
           :x (- (* x cost) (* y sint))
           :y (+ (* x sint) (* y cost)))))

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
  (sab/html (draw :line (yome-side-line 180 (side-count yome)))))

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
               :transform (str "rotate(-" (/ (yome-deg yome) 2) ", 0, 0)")}])))

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
  (let [num-sides (side-count yome)
        {:keys [corner face]} (get-in yome [:sides index])]
    (sab/html [:g {:transform (str "rotate("
                                   (* (yome-deg yome) index)
                                   ", 0, 0)")
                   :class "yome-side"
                   :key (str "yome-side-" index)}
               (cons
                (draw :side yome)
                (map #(draw % yome)
                     (keep identity [corner face])))])))

(defn draw-yome [yome]
  (sab/html
   [:g {:transform (str "rotate(" (/ (yome-deg yome) 2) ", 0, 0)")}
    (map (partial yome-side yome) (range (side-count yome)))]))

;;******************** End Drawing Functions **************************

(def base-corner-controls [:stove-vent :zip-door :door-frame])

(defn count-item [yome item]
  (count (filter #(= item %) (map :corner (:sides yome)))))

(defn addable-stove-vent? [yome]
  (zero? (count-item yome :stove-vent)))

(defn addable-door? [yome]
  (> 3 (+ (count-item yome :zip-door) (count-item yome :door-frame))))

(defn addable-door-frame? [yome]
  (and (addable-door? yome) (> (side-count yome) 6)))

(defn control-visible? [yome type]
  (condp = type
    :zip-door   (addable-door? yome)
    :door-frame (addable-door-frame? yome)
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

(defn corner-transition [{:keys [op item type index] :as ctl} yome-state]
  (om/transact! yome-state
                (fn [state]
                  (let [res (update-in state [:sides index]
                                       (fn [side]
                                  (assoc side type
                                         (when (= op :add) item))))
                        res (update-in res [:form]
                                       (fn [form]
                                         (if (= item :stove-vent)
                                           (condp = op
                                             :add (assoc form :stove-vent-hole true)
                                             :remove (dissoc form :stove-vent-hole)
                                             form)
                                           form)))]
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
                   :className (str "op-" (name (:op corner-control)))
                   :onClick (prevent-> (fn [] (corner-transition corner-control yome)))}
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
              :key (name (:item ctl))              
              :class (str
                      "op-"
                      (name (:op ctl))
                      " "
                      (name (:item ctl)))
              :onClick (prevent-> (fn [] (corner-transition ctl yome)))}
          (control-to-string ctl)])]])))

(defn side-controls [yome index]
  (let [side (get-in yome [:sides index])]
    (sab/html [:div
               (corner-controls yome side index)
               (face-controls yome side index)])))

(defn draw-yome-controls [yome]
    (sab/html [:div.yome-controls (map (partial side-controls yome) (range (side-count yome)))]))

(defn select-yome-size [n state]
  (sab/html
   [:select.yome-type-select
    {:value    n
     :onChange (prevent->value
                (fn [v]
                  (om/transact! state (fn [s] (change-yome-sides s v)))))
                  }
    (map-indexed
     (fn [i y]
       [:option {:value (+ 6 i)} y])
     ["HexaYome" "SeptaYome" "OctaYome"])]))

(defn select-yome-kit [form-state]
  (sab/html
   [:select.yome-kit
    {:value    (:kit form-state)
     :onChange (prevent->value
                (fn [v]
                  (om/update! form-state :kit (= v "true"))))}
    (map
     (fn [[t v]]
       [:option {:value v} t])
     [["Yome" false] ["Yome Kit" true]])]))

(def price-table {:yome                       {6 2550 7 3475 8 3995}
                  :yome-kit                   {6 2290 7 3090 8 3440}
                  :window-reg                 {6 120 7 120 8 120}
                  :window-poly                {6 165 7 165 8 165}
                  :door-frame                 {6 150 7 150 8 150}
                  :zip-door                   {6 50 7 50 8 50}
                  :wall-insulation            {6 845 7 995 8 1155}
                  :roof-insulation-kit        {6 335 7 410 8 525}
                  :roof-insulation-plus-kit   {7 770 8 865}
                  :sunglow-sidewalls          {6 325 7 375 8 430}
                  :snow-load-kit              {6 235 7 275 8 315}
                  :insulation-strips          {6 70  7 85  8 95}
                  :fabric-flashing            {6 95  7 110  8 125}
                  :stove-vent-hole            {6 50  7 50  8 50}
                  :ultra-yome                 {6 0   7 0   8 2280}
                  })

(def option-names {:wall-insulation           "Wall Insulation"
                   :roof-insulation-kit       "Roof Insulation Kit"
                   :roof-insulation-plus-kit  "Roof Insulation Package"
                   :sunglow-sidewalls         "SunGlow Sidewalls"
                   :snow-load-kit             "Snow Load Kit"
                   :insulation-strips         "Insulation Strips"
                   :fabric-flashing           "Fabric Flashing"
                   })

(def binary-options [:kit
                     :poly-window
                     :wall-insulation
                     :roof-insulation-kit       
                     :roof-insulation-plus-kit  
                     :sunglow-sidewalls 
                     :snow-load-kit             
                     :insulation-strips         
                     :fabric-flashing
                     :ultra-yome])

(defn int->mask [i]
  (if (zero? i) 1 (bit-shift-left 1 i)))

(defn convert-options [state]
  (reduce (fn [a [i k]]
            (if (get-in state [:form k])
              (bit-or a (int->mask i))
              a))
          0
          (map-indexed vector binary-options)))

(defn unconvert-options [options-int]
  (reduce
   (fn [a i]
     (if (= (bit-and
             options-int
             (int->mask i))
            (int->mask i))
       (cons (nth binary-options i) a)
       a))
   '() (range (count binary-options))))


(defn chargable-windows [state]
  (let [wc (window-count state)
        poly? (get-in state [:form :poly-window])]
    (if (pos? wc) (dec wc) wc)))

(defn option-cost [type state]
  (get-in price-table [type (side-count state)]))

(defn poly-window-cost-difference [state]
  (if (get-in state [:form :poly-window])
    (* (window-count state)
       (- (option-cost :window-poly state)
          (option-cost :window-reg  state)))
    0))

(defn window-cost [state]
  (+ (* (chargable-windows state)
        (option-cost :window-reg state))
     (poly-window-cost-difference state)))

(defn total-option-cost [state]
  (let [opts (keep (fn [[k v]] (when v k)) (:form state))]
    (reduce + 0 (keep (fn [t] (option-cost t state)) opts))))

(defn yome-cost [state]
  (get-in price-table [(if (get-in state [:form :kit])
                         :yome-kit
                         :yome)
                       (side-count state)]))

(defn door-frame-cost [state]
  (* (door-frame-count state) (option-cost :door-frame state)))

(defn zip-door-cost [state]
  (let [door-number (zip-door-count state)]
    (*
     (if (zero? door-number)
      door-number
      (dec door-number))
     (option-cost :zip-door state))))

(defn stove-vent-hole-cost [state]
  (if (stove-vent? state)
    (option-cost :stove-vent-hole state)
    0))

(defn ultra-yome-cost [state]
  (if (-> state :form :ultra-yome) 
    (option-cost :ultra-yome state)
    0
    ))

(defn get-price [state]
  (apply + ((juxt
             yome-cost
             window-cost
             total-option-cost
             door-frame-cost
             zip-door-cost
             #_stove-vent-hole-cost
             ) state)))



(def price-break-down-parts
  (concat
   [["Base Price"      yome-cost]
    ["UltraYome Upgrade"  ultra-yome-cost]
    ["Windows"         window-cost]
    ["Door Frame"      door-frame-cost]
    ["Zip Doors"       zip-door-cost]    
    ["Stove Pipe Vent" stove-vent-hole-cost]]
   (mapv (fn [[k v]] [v (fn [state]
                         (if (get-in state [:form k])
                           (option-cost k state)
                           0))] ) option-names)))

(defn price-break-down [state]
  (sab/html
   [:div.yome-price-breakdown 
    (map (fn [[k f]]
           (let [p (f state)]
             (if (zero? p)
               (sab/html [:span])
               (sab/html
                [:div.yome-price-row
                 [:div.yome-price-name k] [:div.yome-price-num p ]]))))
         price-break-down-parts)]))

(defn event-checked? [e]
  (.-checked (.-target e)))

(defn checkbox [label value onchange]
  (sab/html [:div {:key label}
             [:label
              [:input.yome-widget-checkbox {:type "checkbox"
                       :value 1
                       :checked value
                       :onChange onchange}]
              [:span.yome-widget-checkbox-label label]]]))

;(defn hidden-ultrayome-checkbox [state]
;  (fn [state] (om/update! state [:form :ultra-yome] false)))

   ;(let [ultra-yome-cost (option-cost :ultra-yome  state)
        ;label (str "Test Label")
        ;value (= true false)
        ;onchange (fn [c] (om/update! state [:form :ultra-yome] false))]
    ;(if (or ;(zero? ultra-yome-cost)
            ;;(< (side-count state) 8)
            ;(-> state :form :kit)
            ;(-> state :form :snow-load-kit)
            ;(-> state :form :roof-insulation-kit) 
            ;(-> state :form :roof-insulation-plus-kit))
              ;(sab/html [:span]) 
              ;(sab/html [:div {:key label}
                          ;[:label
                            ;[:input.yome-widget-checkbox {:type "checkbox"
                                                          ;:value 1
                                                          ;:checked value
                                                          ;:on-change onchange}]
                            ;[:span.yome-widget-checkbox-label label]]])
              ;)))
                            
              
              ;;(checkbox (str "UltraYome Upgrade (includes Roof Insulation/Ceiling Cover) $" ultra-yome-cost)
                ;;(= true false)
                ;;(fn [c] (om/update! state [:form :ultra-yome] false))))))


(defn polycarbonate-window-choice [state]
  (let [poly-cost (* (window-count state)
                     (- (option-cost :window-poly state)
                        (option-cost :window-reg  state)))]
    (if (zero? poly-cost)
      (sab/html [:span])
      (checkbox (str "Polycarbonate Window Covers $" poly-cost)
                (-> state :form :poly-window)
                (prevent->checked
                 (fn [c] (om/update! state [:form :poly-window] c)))))))

(defn ultra-yome-choice [state]
  (let [ultra-yome-cost (option-cost :ultra-yome  state)]
    (if (or ;(zero? ultra-yome-cost)
            (< (side-count state) 8)
            (-> state :form :kit)
            (-> state :form :snow-load-kit)
            (-> state :form :roof-insulation-kit) 
            (-> state :form :roof-insulation-plus-kit))
              (sab/html [:span]) 
              (checkbox (str "UltraYome Upgrade (includes Roof Insulation/Ceiling Cover) $" ultra-yome-cost)
                (-> state :form :ultra-yome)
                (prevent->checked
                  (fn [c] (om/update! state [:form :ultra-yome] c)))))))

(defn option-checkbox [lab type state]
  (when-let [cost (option-cost type state)]
    (checkbox (str lab " $" cost)
              (-> state :form type)
              (prevent->checked
               (fn [c] (om/update! state [:form type] c))))))

(let [roof-options #{:roof-insulation-kit :roof-insulation-plus-kit :ultra-yome}]
  (defn insulation-options-filter [state]
    (if-let [res (not-empty
                    (set (take 1
                               (intersection (->> state
                                                  :form
                                                  (filter second)
                                                  keys
                                                  set) 
                                             roof-options))))]
      
        (clojure.set/difference roof-options res)
        #{})))

(let [snow-load-options #{:snow-load-kit :ultra-yome}]
  (defn snow-load-options-filter [state]
    (if-let [res (not-empty
                    (set (take 1
                               (intersection (->> state
                                                  :form
                                                  (filter second)
                                                  keys
                                                  set) 
                                             snow-load-options))))]
      (clojure.set/difference snow-load-options res)
      #{})))

(defn options [state]
  (let [sides (side-count state)
        insulationFilt  (comp not (insulation-options-filter state) first) ;Make UltraYome and Roof Insulations Mutually Exclusive
        snowLoadFilt  (comp not (snow-load-options-filter state) first)] ;Snow-Load and UltraYome Mutually Exclusive
    (sab/html
     [:div.yome-widget-form-control
      [:div.yome-widget-label [:label "4. Choose any of these options:"]]
      [:div.yome-widget-center
       [:div.yome-widget-options-container 
        ;(if (< sides 8)
        ;(hidden-ultrayome-checkbox state)
        (ultra-yome-choice state);)
       ;(-> state [:form :ultra-yome] false)
        
        (polycarbonate-window-choice state)      
        [:div
         (map (fn [[t n]]
                (option-checkbox n t state))
              (filter snowLoadFilt (filter insulationFilt option-names))
)]]]])))

(defn ship-form-input [state label ky]
  (sab/html
   [:div.yome-widget-form-control
    [:label.yome-widget-inline-label label]
    [:input {:type "text" :name (name ky)
             :value (get-in state [:shipping-form ky])
             :onChange
             (prevent->value (fn [v]
                               (om/transact! state :shipping-form
                                            (fn [f] (assoc f ky v)))))}]]))

(defn extract-mail-data [state]
  (assoc
   (get state :shipping-form)
   :code (serialize-yome state)))

(defn send-form-data [state]
  (let [data (extract-mail-data state)]
    (prn data)
    (om/update! state :shipping-form-state :sending)
    (go
      (let [res (<! (http/post
                     "https://yomewidget.herokuapp.com/mail/deets"
                     #_"http://localhost:9292/mail/deets"
                     {:form-params data}))]
        (om/update! state :shipping-form-state :sent)
        (prn res)))))



(defn shipping-form-filled-out? [state]
  (let [f (get state :shipping-form)]
    (and
     (:email f)
     (< 2 (count (:email f)))
     (:zip f)
     (< 2 (count (:zip f))))))

(defn shipping-form [state]
  (sab/html
     [:div.yome-widget-shipping-form-container
      (ship-form-input state "Name" :name)
      (ship-form-input state "Email" :email)
      (ship-form-input state "City" :city)
      (ship-form-input state "ZIP Code" :zip)
      [:div.yome-widget-form-control
       [:label.yome-widget-inline-label "Comments"]
       [:textarea {:type "text" :name (name :comments)
                   :value (get-in state [:shipping-form :comments])
                   :onChange
                   (prevent->value (fn [v]
                                     (om/transact! state :shipping-form
                                                   (fn [f] (assoc f :comments v)))))}]]
      [:button.yome-shipping-button
       {:onClick (prevent-> (fn [] (send-form-data state)))
        :disabled (not (shipping-form-filled-out? state))}
       "Get Shipping Estimate"]]))

(defn get-shipping-estimate [state]
  (sab/html
   [:div.yome-widget-form-control
    [:div.yome-widget-label [:label "6. Get a shipping estimate:"]]
   (condp = (:shipping-form-state state)
     :show (shipping-form state)
     :sending (sab/html [:h2.yome-widget-center "Sending Email ..."])
     :sent (sab/html [:div.yome-widget-center
                      [:h2 "Email Sent!"]
                      [:p "Thank you for your interest. We'll get a quote to you shortly."]])
     (sab/html
      [:div.yome-widget-center
       [:a.yome-widget-get-estimate-link
        {:href "#"
         :onClick
         (prevent-> (fn [_] (om/update! state :shipping-form-state :show)))}
        "Get Shipping Estimate"]]))]))

(defn place-windows-and-doors [state]
  (sab/html
   [:div.yome-widget-form-control
    [:div.yome-widget-label
     [:label "3. Choose the positions of the door and windows:"]]
    [:div.yome-widget-flex 
     [:div.yome-doors-windows-text 
      [:div.yome-graphic-image]
      [:p "The walls of a Yome are made up of a series of upward and downward facing triangles (see side-bar illustration). The diagram below represents the top plate (the plate between the top of the walls and the bottom of the roof). The diagram's corners represent the tips of the upward facing triangles and the edges represent the downward facing triangles."]
      [:p "The doors and stovepipe vent are placed in upward triangles while the windows and large screen opening are placed in the downward triangles."]
      [:h3 {:style {:textAlign "center"}} "Click on the diagram below to add windows and doors to your Yome:"]]]
    [:div.yome-svg-container 
    [:svg {:class "yome" :height 500 :width 500
           :viewBox "-250 -250 500 500"
           :preserveAspectRatio "xMidYMid meet" }
     (draw-yome state)]
     (draw-yome-controls state)]]))

(defn yome [state]
  (sab/html
   [:div.yome-widget
    [:h1.yome-widget-header "BUILD YOUR YOME"]
    [:div.yome-widget-form-control
     [:div.yome-widget-label [:label "1. Would you like a complete Yome or a Yome Kit?"]]
     [:div.yome-widget-center (select-yome-kit (:form state))]]
    [:div.yome-widget-form-control
     [:div.yome-widget-label [:label "2. How big do you want your Yome to be?"]]
     [:div.yome-widget-center (select-yome-size (side-count state) state)]]

    (place-windows-and-doors state)

    (options state)

    [:div.yome-widget-form-control
     [:div.yome-widget-label [:label "5. Review price below:"]]
     (price-break-down state)
     [:h3.yome-widget-center  "Price Before Shipping: "
      [:span.yome-widget-price-before-shipping
       (str "$" (get-price state))]]]

    (get-shipping-estimate state)

    [:div.yome-state (serialize-yome state)]
    #_[:div [:pre [:code
                 (with-out-str (p/pprint @state))

                 ]]]]))

(defn convert-js-price-data [js-prices]
  (when js-prices
    (->> (js->clj js-prices)
         (map (fn [[k v]]
                [(keyword (string/replace k "_" "-"))
                 (->> v
                      (map (fn [[k v]] [(js/parseInt k) v]))
                      (into {}))]))
         (into {}))))

(defn import-prices! []
  (when-let [js-prices (gobj/get js/window "YOME_WIDGET_PRICES")]
    (when-let [imported (try (convert-js-price-data js-prices)
                             (catch js/Error e nil))]
      (when (and (map? imported)
                 (= (set (keys imported))
                    (set (keys price-table))))
        (js/console.log "importing prices")
        (set! price-table imported)))))

;; import prices as a top level side effect before rendering
(import-prices!)

(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_] (yome data))))
  app-state
  {:target (. js/document (getElementById "com-rigsomelight-yome-widget"))})

(defn handle-hash-change []
  (let [hash (.-hash js/location)]
    (when-let [[_ code] (re-matches #"#\!/yome/(.*)" hash)]
      (reset! app-state (deserialize-yome code)))))

;; not sure if I need this
(defonce location-hash-change
  (do
    (.addEventListener js/window "hashchange" (fn [] (handle-hash-change)))
    true))

(defonce initial-hash-check
  (do
    (js/setTimeout handle-hash-change 2000)
    true))

(defn on-js-reload []
  (swap! app-state update-in [:__figwheel_counter] inc)) 
