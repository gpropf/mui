(ns mui.core
  "mui: Minimalist User Interface. The idea here is that one should not
  spend time developing a complex gui for an app unless you are either
  done with the core functionality or going to have non-technical
  people using it. Guis are enormously expensive to create and serve
  little purpose if the user base is just yourself and a handful of
  others. Here most commands are going to be just a single letter and
  are processed immediately, rather than after pressing 'Enter'. The
  MUI, however will display the commands you've typed and will also
  prompt you for any arguments that may be needed. I imagine this
  prompting will occur in one window and your command input (along
  with arguments) will appear in another. As such MUI lends itself to
  the creation of macros and programs automatically. To duplicate what
  you've done, just copy and paste the commands you've typed. This can
  be saved as a program and replayed. This is one advantage over GUIs
  where replicating a series of actions exactly can be quite difficult
  or even impossible."
  (:require
   [reagent.dom :as rd]
   [clojure.string :as str]
   [reagent.core :as reagent :refer [atom]]
   [cljs.pprint :as pp :refer [pprint]]
   [rasto.util :as rut]))


(defonce mui-state (atom {:command-buffer ""}))




(def mui-cmd-map
  ;"Basic Mui commands common to all applications, even those besides Rasto."
  {"c" {:fn (fn []
              (let [cmd-txtarea (. js/document getElementById  "command-window")]
                (set! (. cmd-txtarea -value) "")
                (swap! mui-state assoc :command-buffer "")))}})


(defn append-to-field
  [field text]
  (let [field-obj (. js/document getElementById field)
        field-obj-val (. field-obj -value)]
    (set! (. field-obj -value) (str field-obj-val text))))



(defn mui-gui [app-cfg]
  (let [keystroke-handler (fn [event]
                            (let [k (.-key event)
                                  mui-cmd-map-including-app-cmds
                                  (merge mui-cmd-map (:app-cmds app-cfg))
                                  mui-cmd (get-in mui-cmd-map-including-app-cmds [k :fn])
                                  cmd-txtarea (. js/document getElementById  "command-window")
                                  keycode (.-keyCode event)
                                  key (.-key event)]
                              (println "mui/core - CMDS2: " cmd-txtarea)
                              ;(pprint app-cfg)
                              (println (repeat 30 "="))
                              (println "CMDS: " mui-cmd-map-including-app-cmds)
                              (println "KEY: " key
                                       ", CODE" (.-code event)
                                       ", KEYCODE" keycode
                                       ", WHICH" (.-which event)
                                       ", Alt, Cntr, Shift, Meta" (.-altKey event)
                                       (.-ctrlKey event)
                                       (.-shiftKey event)
                                       (.-metaKey event))
                              #_(println "Textarea properties: " (js/jQuery "#command-window"))
                              #_(println "Textarea selectionStart: "
                                         (. cmd-txtarea -selectionStart))
                              #_(set! (.. cmd-txtarea -selectionEnd) 4)
                              #_(set! (.. cmd-txtarea -selectionStart) 4)
                              (when mui-cmd
                                (println "APPLYING CMD " k)
                                (apply mui-cmd [{}]))
                              #_(swap! mui-state assoc :command-buffer
                                     (-> event .-target .-value))))]

    [:div
     [:textarea  (merge (:command-window app-cfg)
                        {:value (:command-buffer @mui-state)
                                        ;:on-key-press (fn [event] (println (.-key event)))
                         ;:on-key-press keystroke-handler
                         :on-key-down keystroke-handler
                         :on-change (fn [event]
                                      (swap! mui-state assoc :command-buffer
                                             (-> event .-target .-value)))})]

     [:div "Status Readout2"
      ]
     [:div "Structure View" ;maybe status bar or something
      ]]))
