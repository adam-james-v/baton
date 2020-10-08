(ns ^:figwheel-hooks baton.core
  (:require [reagent.dom :as rdom]
            [goog.events :as events]
            [reagent.core :as r]
            [reagent.ratom :refer [reaction]])
  (:import [goog.events EventType]))

(defn to-pts
  [coords]
  (into [] 
        (for [pt coords]
          [(:x @pt) (:y @pt)])))

(defn coords-exist?
  [coords [x y]]
  (not (empty? 
        (filter #(and (= (:x %) x) (= (:y %) y)) coords))))

(defn get-ancestor [node target-type]
  (if-not (= (.. node -nodeName) target-type)
    (recur (.. node -parentNode) target-type)
    node))

(defn get-parent-offset [e]
  (let [parent (.getBoundingClientRect
                (get-ancestor (.. e -target) "svg"))
        target (.getBoundingClientRect (.. e -target))
        x (.-left parent)
        y (.-top parent)
        x-max (- (.-width parent) (.-width target))
        y-max (- (.-height parent) (.-height target))]
    {:x x
     :y y
     :x-max x-max
     :y-max y-max}))

(defn mouse-move-handler [offset state]
  (fn [e]
    (let [sc (:scale @state)
          x (/ (- (.-clientX e) (:x offset)) sc)
          y (/ (- (.-clientY e) (:y offset)) sc)]
      (reset! state {:scale sc
                     :x (f/clamp x 0 (:x-max offset))
                     :y (f/clamp y 0 (:y-max offset))}))))

(defn mouse-up-handler [on-move]
  (fn [e]
    (events/unlisten js/window EventType.MOUSEMOVE on-move)
    (events/unlisten js/window EventType.TOUCHMOVE on-move)))

(defn mouse-down-handler [state]
  (fn [e]
    (let [offset (get-parent-offset e)
          on-move (mouse-move-handler offset state)]
      (events/listen js/window EventType.MOUSEMOVE on-move)
      (events/listen js/window EventType.TOUCHMOVE on-move)
      (events/listen js/window EventType.MOUSEUP 
                     (mouse-up-handler on-move))
      (events/listen js/window EventType.TOUCHEND 
                     (mouse-up-handler on-move)))))

(defn draggable-point [[x y] state]
  (let [sc (reaction (:scale @state))
        x-pos (reaction (:x @state))
        y-pos (reaction (:y @state))]
    (fn [[x y]]
      [:<>
       [:circle {:class ["attn"]
                 :r (/ 4 @sc)
                 :cx @x-pos
                 :cy @y-pos
                 :on-touch-start (mouse-down-handler state)
                 :on-mouse-down (mouse-down-handler state)}]
       (label [(+ @x-pos (/ 6 @sc)) (+ @y-pos (/ 3 @sc))]
              @sc 
              (str [(f/round @x-pos 3) (f/round @y-pos 3)]))])))

(defn draw-event [state]
  (fn [e]
    (let [sc (:scale @state)
          target (.. e -target -nodeName)
          x (/ (.. e -nativeEvent -offsetX) sc)
          y (/ (.. e -nativeEvent -offsetY) sc)]
      (when-not (or 
                 (coords-exist? @state [x y])
                 (not (= (str target) "svg")))
        (swap! state update :pts
               #(conj % ^{:key (gensym "key-")} 
                      (r/atom {:scale sc :x x :y y})))))))

(defn drawable [w h]
  (let [state (r/atom {:scale 80
                       :pts []})] 
    (fn [w h]
      [:<>
       [:svg
        {:style {:touch-action "none"}
         :width w
         :height h
         :viewBox (str "-1 -1 " w " " h)
         :xmlns "http://www.w3.org/2000/svg"
         :on-click (draw-event state)}
        [:g {:transform (str "scale(" (:scale @state) ")")}
         (svg/polygon (to-pts (:pts @state)))
         #_(fabric/sq 1)
         (doall
          (for [pt (:pts @state)]
            ^{:key (gensym "key-")} ;; messes with touch-events??
            [draggable-point [(:x @pt) 
                              (:y @pt)] pt]))]]
       [:div
        [:button {:on-click 
                  (fn [e] 
                    (swap! state assoc :pts []))} "clear"]
        #_[:div
         "scale: "
         [:input {:type "text"
                  :value (:scale @state)
                  :on-change 
                  (fn [e]
                    (swap! state assoc :scale (.. e -target -value)))}]]
        #_[:pre 
         [:code "coords: " (str (to-pts (:pts @state)))]]]])))

(defn spawner []
  (let [items (r/atom [])]
    (fn []
      [:div
       (when-not (empty? @items) (into [:<>] @items))
       [:button
        {:on-click
         (fn [e]
           (swap! items #(conj % ^{:key (gensym "key-")} [drawable 300 300])))}
         "+ draw"]])))

(defn textbox [value]
  [:input {:type "text"
           :value @value
           :on-change 
           #(reset! value (-> % .-target .-value))}])

(defn slider [value min max step]
  [:input {:type "range"
           :value @value
           :min min
           :max max
           :step step
           :on-change
           #(reset! value (-> % .-target .-value))}])

(defn mount [app]
  (rdom/render [app] (js/document.getElementById "root")))
