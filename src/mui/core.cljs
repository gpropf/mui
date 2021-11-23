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
   [clojure.set :as set]
   [cljs.pprint :as pp :refer [pprint]]
   [rasto.util :as rut]))


(def letter-to-ascii-map {:b  66 :c  67 :F2  113 :Enter  13
                          :n 78})


                                        ; (.charCodeAt \b 0)

(def ascii-to-letter-map (set/map-invert letter-to-ascii-map))


(defonce mui-state (atom {:command-buffer ""
                          :mode :normal
                          :query {}}))

(defonce application-defined-types (atom (sorted-map)))



;; mui-object-store: This is actually the memory of the mui language.
;; Anything you create or import into it gets stored here.  I did not
;; want rapidly changing things like the mouse position or
;; command/history buffers to share and atom with large data
;; structures.
(defonce mui-object-store
   (atom {}))


;(defonce mui-constructor-map (atom {}))


(def mui-default-cfg {:command-window-prompt ":> "
                      :command-window {}
                      :history-window {:style {:height "auto"
                                               :margin-bottom "5px"
                                               :float "right"}
                                       :id "history-window"
                                       :rows "8"
                                       :cols "60"
                                       :class ""
                                       }})

(def command-history (atom '()))

(def conversion-fn-map {:int #(let [parsed-int (js/parseInt %)]
                                (when (js/isNaN parsed-int)
                                  (throw "Bad input for integer!")
                                  #_(println "BAD INTEGER!!!!"))
                                parsed-int)
                        :float #(let [parsed-float (js/parseFloat %)]
                                 (when (js/isNaN parsed-float)
                                   (throw "Bad input for float!"))
                                 parsed-float)})

(defn prettify-list-to-string
  "Prints the collection in lst with numbers associated with each item
  for user selection purposes."
  [lst]
  (map
   (fn [[i s]] (str i ") " s))
   (map-indexed vector lst)))



(defn command-buffer-clear []
  (swap! mui-state assoc :command-buffer "" :prompt-end 0))


(defn command-buffer-append [text]
  (swap! mui-state update :command-buffer str text))




(defn append-to-field
  [field text]
  (let [field-obj (. js/document getElementById field)
        field-obj-val (. field-obj -value)]
    (set! (. field-obj -value) (str field-obj-val text))))


(defn println-fld [field text]
  (println "CALLED: println-fld with: " text)
  (append-to-field field (str "\n" text))
  (set! (.-scrollTop field) (.-scrollHeight field))
;; This ^^ doesn't work to keep the textarea scrolled
  )


(defn prepare-query-for-history [query]
  (let [query' (dissoc query :fn :help)
        args (:args query')
        args' (into {} (map (fn [[arg arg-data]]
                              [arg (:val arg-data)]) args))]
    (assoc query' :args args')))



(defn add-to-history [args]
  (println "Trying to add " args " to hist")
  (swap! command-history conj (prepare-query-for-history args)))


(defn present-prompt [arg-data textarea-element]
  (println-fld "command-window" (str (:prompt arg-data) " "))
  (swap! mui-state assoc :prompt-end (.-selectionStart textarea-element)))


(defn set-mode [mode query-map key-keyword]
  (let []
    (println "CALLED: set-mode (mode, query-map, command-key): " [mode query-map key-keyword])
    (swap! mui-state assoc
           :return-to-normal true
           :mode mode
           :query (assoc query-map :command-key key-keyword))))


(defn load-prompts [textarea-element]
  (println "LOAD PROMPTS CALLED !!!!!!!!!!!!!! Trying to add " " to hist")
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
        (println "Mui-STATE: redacted " ;; @mui-state
                 "FN: " command-fn
                 "\nARGS: " args)
        (add-to-history (:query @mui-state))
        (command-fn args)
        ; ^^^ This is important! ^^^
        ; This is where the commands are run with their arguments.
        (when (:return-to-normal @mui-state)
          (set-mode :normal {} nil))))))




(defn rebuild-mui-cmd-map []
  {:F2
   {:fn (fn [arg-map]
          (let [cmd-txtarea (. js/document getElementById  "command-window")]
            (println "CLEARING WINDOW!!")
            (set! (. cmd-txtarea -value) "")
            (command-buffer-clear)  #_(swap! mui-state assoc :command-buffer "")))
    :args {}
    :help {:msg "F2\t: Clear command window."}}
   :n
   {:fn (fn [arg-map]
          (let [cmd-txtarea (. js/document getElementById  "command-window")
                user-selection-index (get-in arg-map [:t :val])
                selected-type-name (nth (keys @application-defined-types) user-selection-index)
                new-object-query (get-in @application-defined-types [selected-type-name :new])]

            (println "\n\n\nWould create object of type: " selected-type-name)
            (println "Loading new query: " new-object-query)
            (set-mode :query new-object-query :nb)
            (swap! mui-state assoc :return-to-normal false)
            (load-prompts cmd-txtarea)))

    :help {:msg "n\t: Create a new object."}
    :args
    {:t
     {:prompt (apply str "Choose the type of object to create from the following list by entering the number of your selection:"
                     (prettify-list-to-string (keys @application-defined-types)))
      :type :int}}}})

(def mui-cmd-map
  ;"Basic Mui commands common to all applications, even those besides Rasto."
  (atom (rebuild-mui-cmd-map)))


(defn register-application-defined-type
  [type-name constructor-prompts]
  (swap! application-defined-types assoc type-name constructor-prompts)
  (reset! mui-cmd-map (rebuild-mui-cmd-map)))


(register-application-defined-type "BOO" {:b 2})
(register-application-defined-type "COO" {:c 3})







(def tickets (atom 0))





(defn prettify-history [history-list]
  (reduce #(str %1 %2) ""
          (reverse (map (fn [history-item]
                          (str history-item "\n")) history-list))))









#_(defn load-next-arg []
  (let [query-args (get-in @mui-state [:query :args])
        args-to-get (filter (fn [[arg arg-data]]
                              (nil? (:val arg-data))) query-args)
        [arg arg-data] (first args-to-get)]
    (swap! mui-state assoc :current-arg [arg arg-data])
    (println-fld "command-window" (str "\n" (:prompt arg-data)))))


(defn set-cursor-pos [input-element offset]
  (let [current-pos (.-selectionStart input-element)]
    (set! (.-selectionStart input-element) (+ current-pos offset))))

(defn get-cursor-pos [input-element]
  (.-selectionStart input-element))


(defn filter-keystrokes [event]
  (let [keycode (.-keyCode event)
        cmd-txtarea
        (. js/document getElementById  "command-window")
        prompt-end-pos (:prompt-end @mui-state)
        cursor-pos (.-selectionStart cmd-txtarea)
        selection-end (.-selectionEnd cmd-txtarea)
        _ (println "Cursor-pos: " cursor-pos
                   ", prompt-end-pos: " prompt-end-pos
                   ", selectionEnd: " selection-end)]
    (println ":on-key-down, cursor is at "
             (get-cursor-pos cmd-txtarea))
    (case keycode
      (8 37) (when (< cursor-pos (inc prompt-end-pos))
               (do (println "Trying to stop cursor from going back more.")
                   (.preventDefault event)))
      (38 40) (.preventDefault event)
      "default")))


(defn print-key-from-event [event]
  (let [k (.-key event)
        ;cmd-txtarea (. js/document getElementById  "command-window")
        keycode (.-keyCode event)
        key-keyword (ascii-to-letter-map keycode)
        key (.-key event)
        alt-key (.-altKey event)
        ctrl-key (.-ctrlKey event)
        shift-key (.-shiftKey event)
        meta-key (.-metaKey event)
        keycode-and-flags [key-keyword alt-key ctrl-key shift-key meta-key]]
    #_(println "KEY: " key
             ", CODE" (.-code event)
             ", KEYCODE" keycode
             ", WHICH" (.-which event)
             ", Alt, Cntr, Shift, Meta :::"
             keycode-and-flags)
    (println "keycode-and-flags: " keycode-and-flags)
    keycode-and-flags
    )
  )


(defn prettify-help
  "Picks out the help text summary for each command and puts a newline
  after it to make the help screen look tidy."
  [mui-cmd-map-including-app-cmds]
  (apply str (map (fn [[key-keyword cmd-map]]
         (str (get-in cmd-map [:help :msg]) "\n")
         ) mui-cmd-map-including-app-cmds)))


(defn merge-in-app-cmds [app-cfg mui-cmd-map-atom]
  (let [mui-cmd-map @mui-cmd-map-atom
        app-cmds (:app-cmds app-cfg)
        merged-cmd-map (merge mui-cmd-map app-cmds)]
    (reset! mui-cmd-map-atom merged-cmd-map)))


(defn mui-gui
  "This is the function that displays the actual Mui console. It will be
  called by the application using Mui, not by anything within it so
  the IDE may identify it as unused."
  [app-cfg]
  (let [mui-cmd-map-including-app-cmds
        (merge-in-app-cmds app-cfg mui-cmd-map)
        keystroke-handler
        (fn [event]
          (let [cmd-txtarea
                (. js/document getElementById "command-window")
                keycode (.-keyCode event)
                _ (println
                   "mui-cmd-map-including-app-cmds:"
                   mui-cmd-map-including-app-cmds)
                key (.-key event)
                keycode-and-flags (print-key-from-event event)
                key-keyword (first keycode-and-flags)
                mui-cmd
                (mui-cmd-map-including-app-cmds key-keyword)]
            (println ":on-key-up, cursor is at "
                     (get-cursor-pos cmd-txtarea))
            (case (:mode @mui-state)
              :normal (case keycode-and-flags
                        [:c, false, true, false, false]
                        (let [_ (println "CNTRL-C")])
                        (when mui-cmd
                          (println "NORMAL MODE, Command Entered: " keycode)
                          (set-mode :query mui-cmd key-keyword)
                          (load-prompts cmd-txtarea)))
              :query (case keycode-and-flags
                       [:Enter, false, false, false, false]
                       (try
                         (let [current-arg (:current-arg @mui-state)
                               arg-data (get-in @mui-state
                                       [:query :args current-arg])
                               arg-type (:type arg-data)
                               conversion-fn (conversion-fn-map arg-type)
                               arg-val (conversion-fn (subs
                                                       (. cmd-txtarea -value)
                                                       (:prompt-end
                                                        @mui-state)))]
                           (swap! mui-state assoc-in
                                  [:query :args current-arg :val] arg-val)
                           #_(command-buffer-clear)
                           #_(load-prompts cmd-txtarea))
                         (catch js/Object e (println e))
                         (finally (do
                                    (command-buffer-clear)
                                    (load-prompts cmd-txtarea))))
                       [:c, false, true, false, false]
                       (let []
                         (set-mode :normal nil nil)
                         (println "CNTRL-C - interrupting command.")
                         (println-fld "command-window" "-- INTERRUPT! --\n"))
                       "default"))))]
    [:div
     [:div {:style {:width "45%" :margin "auto"}}
      [:label {:for "command-window"} "Command Entry: "]
      [:textarea  (merge (:command-window app-cfg)
                         {;:on-load #(swap! mui-state assoc :implicits (:implicits app-cfg))
                          :on-key-up keystroke-handler
                          :on-key-down filter-keystrokes
                          :on-change (fn [event]
                                       (let [cmd-txtarea (. js/document getElementById
                                                            "command-window")]
                                         (println ":on-change, cursor is at "
                                                  (get-cursor-pos cmd-txtarea))
                                         (println "IMPLICITS: " '(:implicits app-cfg))
                                         (swap! mui-state assoc
                                                :implicits (:implicits app-cfg))))})]]
     [:div {:style {:width "45%" :margin "auto"}}
      [:label {:for "history-window"} "Command History: "]
      [:textarea (merge (:history-window mui-default-cfg)
                        {:value (prettify-history @command-history)
                         :readOnly true})]]
     [:div {:style {:width "45%" :margin "auto"}}
      [:label {:for "help-window"} "Help: "]
      [:textarea (merge (:history-window mui-default-cfg)
                        {:value (prettify-help mui-cmd-map-including-app-cmds)
                         :readOnly true})]]]))
