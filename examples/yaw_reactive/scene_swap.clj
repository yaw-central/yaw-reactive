(ns yaw-reactive.scene_swap
  "An example to show the scene swapping"
  (:require
    [clojure.set :as set]
    [yaw.world :as w]
    [yaw-reactive.scene :as s]
    [yaw-reactive.reaction :as r]
    [yaw-reactive.render :as render]))

;; Define all the scenes

(defn scene1 []
  [:scene
   [:ambient {:color :white :i 0.4}]
   [:sun {:color :red :i 1 :dir [-1 0 0]}]
   [:light ::light {:color :yellow :pos [0.5 0 -4]}]
   [:item :test/box {:mesh :mesh/box
                     :pos [0 0 -5]
                     :rot [45 45 45]
                     :mat :red
                     :scale 0.3}]])

;; Add the scenes

(s/register-scene! :scene1 scene1)

;; Start the render

(def +myctrl+ (w/start-universe!))

(render/render! +myctrl+ [scene1])