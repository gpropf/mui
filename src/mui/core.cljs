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

(def mui-cfg {:command-window-prompt ":> "})


(def conversion-fn-map {:int js/parseInt
                        :float js/parseFloat})

(defn command-buffer-clear []
  (swap! mui-state assoc :command-buffer ""))


(defn command-buffer-append [text]
  (swap! mui-state update :command-buffer str text))




(defn append-to-field
  [field text]
  (let [field-obj (. js/document getElementById field)
        field-obj-val (. field-obj -value)]
    (set! (. field-obj -value) (str field-obj-val text))))


(defn println-fld [field text]
  (append-to-field field (str text "\n")))


(def mui-cmd-map
  ;"Basic Mui commands common to all applications, even those besides Rasto."
  {"c" {:fn (fn [arg-map]
              (let [cmd-txtarea (. js/document getElementById  "command-window")]
                (println "CLEARING WINDOW!!")
                (set! (. cmd-txtarea -value) "")
                (command-buffer-clear)  #_(swap! mui-state assoc :command-buffer "")))
        :args {}}})




(def tickets (atom 0))

#_(defn take-ticket! []
  (let [ticket-num @tickets]
    (println "Ticket #" ticket-num " taken.")
    (swap! tickets inc)
    ))


(defn set-mode [mode query-map]
  (let []
    (println "SET-MODE with query: " query-map ", mode: " mode)
    (swap! mui-state assoc
           :mode mode
           :query query-map)
    )
  )


(defn present-prompt [arg-data textarea-element]
  (println-fld "command-window" (str "\n" (:prompt arg-data)))
  (swap! mui-state assoc :prompt-end (.-selectionStart textarea-element)))


(defn load-prompts [textarea-element]
  (let [query-args (get-in @mui-state [:query :args])
        args-to-get (filter (fn [[arg arg-data]]
                              (nil? (:val arg-data))) query-args)
        [arg arg-data] (first args-to-get)]
    (swap! mui-state assoc :current-arg arg)
    (if arg-data
      (do (println "ARG-DATA FOUND: " arg-data)
          (present-prompt arg-data textarea-element))
      (let [command-fn (get-in @mui-state [:query :fn])
            args (get-in @mui-state [:query :args])]
        (println "Mui-STATE: " @mui-state
                 "FN: " command-fn
                 "\nARGS: " args)
        (command-fn args)
        (set-mode :normal {})))))


#_(defn load-next-arg []
  (let [query-args (get-in @mui-state [:query :args])
        args-to-get (filter (fn [[arg arg-data]]
                              (nil? (:val arg-data))) query-args)
        [arg arg-data] (first args-to-get)]
    (swap! mui-state assoc :current-arg [arg arg-data])
    (println-fld "command-window" (str "\n" (:prompt arg-data)))))


(defn mui-gui [app-cfg]
  (let [keystroke-handler (fn [event]
                            (let [k (.-key event)
                                  mui-cmd-map-including-app-cmds
                                  (merge mui-cmd-map (:app-cmds app-cfg))
                                  mui-cmd (mui-cmd-map-including-app-cmds k)
                                  cmd-txtarea (. js/document getElementById  "command-window")
                                  keycode (.-keyCode event)
                                  key (.-key event)]
                              #_(take-ticket!)
                              (println (repeat 30 "="))
                              ;(println "CMDS: ")
                              ;(pprint mui-cmd-map-including-app-cmds)
                              #_(println "KEY: " key
                                       ", CODE" (.-code event)
                                       ", KEYCODE" keycode
                                       ", WHICH" (.-which event)
                                       ", Alt, Cntr, Shift, Meta" (.-altKey event)
                                       (.-ctrlKey event)
                                       (.-shiftKey event)
                                       (.-metaKey event))
                              (case (:mode @mui-state)
                                :normal (when mui-cmd
                                          (println "NORMAL MODE, Command Entered: " k)
                                          (set-mode :query mui-cmd)
                                          (load-prompts cmd-txtarea))
                                :query (case keycode
                                         13 (let [current-arg (:current-arg @mui-state)
                                                  arg-data
                                                  (get-in @mui-state [:query :args current-arg])
                                                  arg-type (:type arg-data)
                                                  ;_ (println "ARG TYPE is " arg-type)
                                                  conversion-fn (conversion-fn-map arg-type)
                                                  arg-val (conversion-fn (:command-buffer @mui-state))]
                                              (swap! mui-state assoc-in
                                                     [:query :args current-arg :val] arg-val)
                                              (command-buffer-clear)
                                              (load-prompts cmd-txtarea))

                                         (command-buffer-append (char keycode))))

                              #_(swap! mui-state assoc :command-buffer
                                       (-> event .-target .-value))))]

    [:div
     [:textarea  (merge (:command-window app-cfg)
                        {;:value (:command-buffer @mui-state)
                                        ;:on-key-press (fn [event] (println (.-key event)) true)
                         :on-key-up keystroke-handler
                         ;:on-key-down (fn [event] (command-buffer-append (.-key event)))
                         #_(fn [event]
                             (when (= (.-key event) "D") (.preventDefault event))
                             true)
                         :on-change (fn [event]
                                      (println ":on-change")
                                      #_(take-ticket!)
                                      #_(swap! mui-state assoc :command-buffer
                                               (-> event .-target .-value)))})]

     [:div "Status Readout2"]
     [:div "Structure View" ;maybe status bar or something
      ]]))
