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


(defonce mui-state (atom {:command-buffer ""
                          :mode :normal
                          :query {}}))


(defn append-to-field
  [field text]
  (let [field-obj (. js/document getElementById field)
        field-obj-val (. field-obj -value)]
    (set! (. field-obj -value) (str field-obj-val text))))


(defn println-fld [field text]
  (append-to-field field (str text "\n")))


(def mui-cmd-map
  ;"Basic Mui commands common to all applications, even those besides Rasto."
  {"c" {:fn (fn []
              (let [cmd-txtarea (. js/document getElementById  "command-window")]
                (set! (. cmd-txtarea -value) "")
                (swap! mui-state assoc :command-buffer "")))}})




(def tickets (atom 0))

(defn take-ticket! []
  (let [ticket-num @tickets]
    (println "Ticket #" ticket-num " taken.")
    (swap! tickets inc)
    ))


(defn set-mode [mode query-map]
  (let []
    (swap! mui-state assoc
           :mode mode
           :query query-map)
    )
  )


(defn load-next-arg []
  (let [query-args (get-in @mui-state [:query :args])
        args-to-get (filter (fn [[arg arg-data]]
                              (nil? (:val arg-data))) query-args)
        first-arg (when args-to-get (first args-to-get))]
    (swap! mui-state assoc :current-arg first-arg)
    (println-fld "command-window" (str "\n" (last first-arg)))))


(defn mui-gui [app-cfg]
  (let [keystroke-handler (fn [event]
                            (let [k (.-key event)
                                  mui-cmd-map-including-app-cmds
                                  (merge mui-cmd-map (:app-cmds app-cfg))
                                  mui-cmd (mui-cmd-map-including-app-cmds k)  #_(get-in mui-cmd-map-including-app-cmds [k :fn])
                                  cmd-txtarea (. js/document getElementById  "command-window")
                                  keycode (.-keyCode event)
                                  key (.-key event)]
                              (take-ticket!)
                              #_(println "mui/core - CMDS2: " cmd-txtarea)
                              ;(pprint app-cfg)
                              (println (repeat 30 "="))
                              (println "CMDS: ")
                              (pprint mui-cmd-map-including-app-cmds)
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
                              (case (:mode @mui-state)
                                :normal (when mui-cmd
                                          (println "Command Entered: " k)
                                          #_(apply mui-cmd [{}])
                                          (set-mode :query mui-cmd)
                                          (load-next-arg))
                                :query (let [query-args (get-in @mui-state [:query :args])
                                             args-to-get (filter (fn [[arg arg-data]]
                                                                        (nil? (:val arg-data)) ) query-args)]
                                         (when args-to-get
                                           (swap! mui-state assoc :current-arg (first args-to-get)))


                                         ))

                              #_(swap! mui-state assoc :command-buffer
                                       (-> event .-target .-value))))]

    [:div
     [:textarea  (merge (:command-window app-cfg)
                        {:value (:command-buffer @mui-state)
                                        ;:on-key-press (fn [event] (println (.-key event)))
                         :on-key-up keystroke-handler
                         ;:on-key-down keystroke-handler
                         :on-change (fn [event]
                                      (println ":on-change")
                                      (take-ticket!)
                                      (swap! mui-state assoc :command-buffer
                                             (-> event .-target .-value)))})]

     [:div "Status Readout2"]
     [:div "Structure View" ;maybe status bar or something
      ]]))
