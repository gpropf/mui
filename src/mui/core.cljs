(ns rasto.mui
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
  {"c" (fn [] (swap! mui-state assoc :command-buffer ""))


                  })






(defn mui-gui [app-cfg]
  (let [keystroke-handler (fn [event]
                            (let [k (.-key event)
                                  mui-cmd-map-including-app-cmds
                                  (merge mui-cmd-map (:app-cmds app-cfg))
                                  mui-cmd (mui-cmd-map-including-app-cmds k)
                                  cmd-txtarea (. js/document getElementById  "command-window")]
                              (println "rasto/mui - CMDS1: " cmd-txtarea)
                              ;(pprint app-cfg)
                              (println (repeat 30 "="))
                              (println "CMDS: " mui-cmd-map-including-app-cmds)
                              (println "KEY: " (.-key event)
                                       ", CODE" (.-code event)
                                       ", KEYCODE" (.-keyCode event)
                                       ", WHICH" (.-which event)
                                       ", Alt, Cntr, Shift, Meta" (.-altKey event)
                                       (.-ctrlKey event)
                                       (.-shiftKey event)
                                       (.-metaKey event))
                              (println "Textarea properties: " (js/jQuery "#command-window"))
                              (println "Textarea selectionStart: "
                                       (. cmd-txtarea -selectionStart))
                              (set! (.. cmd-txtarea -selectionEnd) 4)
                              (set! (.. cmd-txtarea -selectionStart) 4)
                              (when mui-cmd (apply mui-cmd []))
                              (swap! mui-state update :command-buffer str k)) true)]

    [:div
     [:textarea  (merge (:command-window app-cfg)
                        {:value (:command-buffer @mui-state)
                                        ;:on-key-press (fn [event] (println (.-key event)))
                         ;:on-key-press keystroke-handler
                         :on-key-down keystroke-handler})]

     [:div ;output frame
      ]
     [:div ;maybe status bar or something
      ]]))
