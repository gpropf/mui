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
    [gputils.core :as gpu]
    ;[cljs.js]
    ))

#_(def state (cljs.js/empty-state))

#_(defn evaluate [source cb]
    (cljs.js/eval-str state source nil {:eval cljs.js/js-eval :context :expr} cb))




;; This is how to do reflection - (:arglists (meta #'rasto.core/make-raster))


(def letter-to-ascii-map {:b 66 :c 67 :F2 113 :Enter 13
                          :n 78 :s 83 :d 68 :ArrowUp 38 :ArrowDown 40})


(def key-sym-to-key-with-modifiers {
                                    :F2        [113 false false false false]
                                    :Ctrl-C    [67 false true false false]
                                    :ArrowDown [40 false false false false]
                                    :ArrowUp [38 false false false false]
                                    :Enter     [13 false false false false]
                                    :s [83 false false false false]
                                    :n [78 false false false false]
                                    :d [68 false false false false]
                                    })
;;keycode-and-flags [keycode alt-key ctrl-key shift-key meta-key]]
(def key-with-modifiers-to-key-sym (set/map-invert key-sym-to-key-with-modifiers))

(def ascii-to-letter-map (set/map-invert letter-to-ascii-map))


(defonce mui-state (atom {:command-buffer ""
                          :mode           :normal
                          :query          {}}))

(defonce application-defined-types (atom (sorted-map)))



;; mui-object-store: This is actually the memory of the mui language.
;; Anything you create or import into it gets stored here.  I did not
;; want rapidly changing things like the mouse position or
;; command/history buffers to share and atom with large data
;; structures.
(defonce mui-object-store
         (atom (sorted-map)))


;(defonce mui-constructor-map (atom {}))


(def mui-default-cfg {:command-window-prompt ":> "
                      :command-window        {}
                      :history-window        {:style {:height        "auto"
                                                      :margin-bottom "5px"
                                                      :float         "right"}
                                              :id    "history-window"
                                              :rows  "8"
                                              :cols  "60"
                                              :class ""
                                              }})

(def command-history (atom '()))

(def conversion-fn-map {:int    #(let [parsed-int (js/parseInt %)]
                                   (when (js/isNaN parsed-int)
                                     (throw "Bad input for integer!")
                                     #_(println "BAD INTEGER!!!!"))
                                   parsed-int)
                        :float  #(let [parsed-float (js/parseFloat %)]
                                   (when (js/isNaN parsed-float)
                                     (throw "Bad input for float!"))
                                   parsed-float)
                        :yn     #(let [trimmed-input (str/trim %1)]
                                  #_(println "conversion-fn-map: '" trimmed-input "'")
                                   (case trimmed-input
                                     ("y" "Y") "Y"
                                     ("n" "N") "N"
                                     (throw "Only 'y' or 'n' answers allowed!")
                                     ))                     ;; FIXME
                        :string #(let []
                                   (when (empty? (str/trim %1))
                                     (throw "Empty strings not allowed!")
                                     ) (str/trim %1))})


(defn prettify-list-to-string
  "Prints the collection in lst with numbers associated with each item
  for user selection purposes."
  [lst]
  (interpose ", "
             (map
               (fn [[i s]] (str i ") " s))
               (map-indexed vector lst))))


#_(defn select-object-from-store
    ([object-store-atom query-fn]
     (select-object-from-store object-store-atom query-fn nil))
    ([object-store-atom query-fn type-key]
     (let [object-store @object-store-atom
           choices (if (nil? type-key)
                     (query-fn (apply str
                                      "Choose type of object from list: "
                                      (prettify-list-to-string (keys object-store)) " "))
                     (query-fn (apply str
                                      "Choose object from list: "
                                      (prettify-list-to-string (keys (object-store type-key))) " ")))]
       choices
       )))


(defn get-object-id-by-numbers
  ([map-atom k]
   (get-object-id-by-numbers map-atom k nil))
  ([map-atom k n]
   (let [types (keys @map-atom)
         obj-type (nth types k)
         objs (keys (@map-atom obj-type))]
     (if (nil? n)
       (nth types k)
       (nth objs n)))))


(defn choose-object
  "Associated with the 'select' command. Chooses a particular object
  for further operations."
  ([map-atom query-text k]
   (choose-object map-atom query-text k nil))
  ([map-atom query-text k n]
   (let [objs (@map-atom k)
         obj-ids (keys objs)

         ]
     (if (nil? n)
       (apply str query-text " " (prettify-list-to-string obj-ids))
       (nth obj-ids n)
       ))))


(defn list-objects-of-type [t]
  (let [objects (@mui-object-store t)
        object-ids (keys objects)

        ]
    (apply str "Select an object by entering its number: " (prettify-list-to-string object-ids))))



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
  (let [sh (.-scrollHeight field)]
   #_(println "CALLED: println-fld with: " text)
    (append-to-field field (str "\n" text))
    (set! (. field -scrollTop) sh))

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
  (let [prompt (:prompt arg-data)
        prompt-text (if (= (type prompt) (type (fn [])))
                      (apply prompt ())
                      prompt)]

    (println-fld "command-window" (str prompt-text " "))
    (swap! mui-state assoc :prompt-end (.-selectionStart textarea-element))))


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
        (println "Mui-STATE: redacted "                     ;; @mui-state
                 "FN: " command-fn
                 "\nARGS: " args)

        (add-to-history (:query @mui-state))
        (when command-fn (command-fn args))
        ; ^^^ This is important! ^^^
        ; This is where the commands are run with their arguments.
        (when (:return-to-normal @mui-state)
          (set-mode :normal {} nil))))))


(defn choose-type
  "Associated with the 'select' command. Chooses a particular type. When followed
  with a call to 'choose-object' we have set the selection for a particular type of
  object."
  ([]
   (choose-type false))
  ([only-choose-from-extant-types]
   (let [map-atom (if only-choose-from-extant-types mui-object-store application-defined-types)
         prompt-text "Choose the type of object from the following list by entering the number of your selection:"]
     (apply str
            prompt-text
            (prettify-list-to-string (keys @map-atom))))))



(defn confirm-action
  "Generally this asks you to confirm a risky action like deleting something."
  ([default-answer]
   (let [prompt-text "Do you want to do this (y/n):"]
     (apply str
            prompt-text
            ))))


(defn select-object [obj-type obj-id]
  (let []
    (swap! application-defined-types assoc-in [obj-type :selection] obj-id)))




(defn rebuild-mui-cmd-map
  "Prompts can now be functions that return text instead of static
  text. Any prompt whose text might change based on something that can
  change during a run should be a function so that it can build its
  text based on what's going on in the program."
  []
  {:Enter
              {:fn (fn [arg-map]
                     (let [_ (println "Running ENTER command form mui-cmd-map!!!!!")
                           cmd-txtarea (. js/document getElementById "command-window")]
                       (try
                         (let [
                               current-arg (:current-arg @mui-state)
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
                                    (load-prompts cmd-txtarea)))))

                     )
               :args {}
               :help nil
               :active-in-states (set [:query])
               }
   :F2
              {:fn               (fn [arg-map]
                                   (let [cmd-txtarea (. js/document getElementById "command-window")]
                                    #_(println "CLEARING WINDOW!!")
                                     (set! (. cmd-txtarea -value) "")
                                     (command-buffer-clear)  #_(swap! mui-state assoc :command-buffer "")))
               :args             {}
               :active-in-states (set [:normal])
               :help             {:msg "F2\t: Clear command window."}}
   :s         {:fn               (fn [arg-map]
                                   (let [cmd-txtarea (. js/document getElementById "command-window")
                                         selected-object-id-index
                                         (get-in (:query @mui-state) [:args :obj :val])
                                         selected-object-type-index
                                         (get-in (:query @mui-state) [:args :t :val])
                                         selected-object-id
                                         (get-object-id-by-numbers mui-object-store
                                                                   selected-object-type-index selected-object-id-index)
                                         selected-object-type
                                         (get-object-id-by-numbers mui-object-store
                                                                   selected-object-type-index)]
                                     (select-object selected-object-type selected-object-id)

                                    #_(println "Selecting object!"
                                              [selected-object-type selected-object-id])))
               :active-in-states (set [:normal])
               :args             {:t
                                  {:prompt (fn [] (choose-type true))
                                   :type   :int}
                                  :obj
                                  {:prompt (fn []
                                             (let [type-index
                                                   (get-in @mui-state [:query :args :t :val])
                                                   type-names (keys @mui-object-store)
                                                   selected-type-name (nth type-names type-index)]
                                               (choose-object mui-object-store
                                                              "Select an object by entering its number: "
                                                              selected-type-name)))
                                   :type   :int}}
               :help             {:msg "s\t: Select an object for further use."}}
   :d         {:fn               (fn [arg-map]
                                   (let [cmd-txtarea (. js/document getElementById "command-window")
                                         yes-or-no (get-in arg-map [:confirm :val])
                                         ]
                                    #_(println "Would Delete currently selected object?: " yes-or-no
                                              )))
               :active-in-states (set [:normal])
               :args             {:confirm
                                  {:prompt (fn [] (confirm-action true))
                                   :type   :yn}

                                  }
               :help             {:msg "d\t: Delete currently selected object."}}
   :ArrowDown {:fn               (fn [arg-map]
                                   (let [download-filename (get-in arg-map [:download-filename :val])
                                         data (rebuild-mui-cmd-map)
                                         data-map {:download-filename download-filename
                                                   :data              data}]
                                     (gpu/send-data data-map download-filename)
                                     ))
               :active-in-states (set [:normal])
               :args             {:download-filename
                                  {:prompt "Enter a filename:"
                                   :type   :string}
                                  }
               :help             {:msg "↓\t: Download the current command map to an edn file."}
               }
   :Ctrl-C    {:fn               (fn [arg-map]
                                   (let []
                                     (set-mode :normal nil nil)
                                    #_(println "CNTRL-C - interrupting command. New version")
                                     (println-fld "command-window" "-- INTERRUPT! New version --\n"))

                                   )
               :help             {:msg "Ctrl-C\t: Abort command."}
               :active-in-states (set [:normal :query])
               :args             {}}
   :n
              {:fn               (fn [arg-map]
                                   (let [cmd-txtarea (. js/document getElementById "command-window")
                                         user-selection-index (get-in arg-map [:t :val])
                                         selected-type-name (nth (keys @application-defined-types) user-selection-index)
                                         new-object-query (get-in @application-defined-types [selected-type-name :prompts :new])]

                                    #_(println "\n\n\nWould create object of type: " selected-type-name)
                                    #_(println "Loading new query: " new-object-query)
                                     (set-mode :query new-object-query :nb)
                                     ;; We don't return to normal mode so the queries for the
                                     ;; constructor args will be processed.
                                     (swap! mui-state assoc :return-to-normal false)
                                     (load-prompts cmd-txtarea)))
               :help             {:msg "n\t: Create a new object."}
               :active-in-states (set [:normal])
               :args
                                 {:t
                                  {:prompt (choose-type)
                                   :type   :int}}}})

(def mui-cmd-map
  ;"Basic Mui commands common to all applications, even those besides Rasto."
  (atom (rebuild-mui-cmd-map)))


(defn register-application-defined-type
  [type-name constructor-prompts]
  (swap! application-defined-types assoc type-name {:prompts   constructor-prompts
                                                    :selection nil})
  (reset! mui-cmd-map (rebuild-mui-cmd-map)))






(defn add-object-to-object-store [obj obj-type obj-id parent-obj-id]
  (do (swap! mui-object-store assoc-in [obj-type obj-id] {:obj obj :parent-obj-id parent-obj-id})
      (select-object obj-type obj-id)))




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
        (. js/document getElementById "command-window")
        prompt-end-pos (:prompt-end @mui-state)
        cursor-pos (.-selectionStart cmd-txtarea)
        selection-end (.-selectionEnd cmd-txtarea)
        ]
   #_(println ":on-key-down, cursor is at "
             (get-cursor-pos cmd-txtarea))
    (case keycode
      (8 37) (when (< cursor-pos (inc prompt-end-pos))
               (do #_(println "Trying to stop cursor from going back more.")
                   (.preventDefault event)))
      (38 40) (.preventDefault event)
      "default")))


(defn print-key-from-event2 [event]
  (let [k (.-key event)
        ;cmd-txtarea (. js/document getElementById  "command-window")
        keycode (.-keyCode event)
        key-keyword (ascii-to-letter-map keycode)
        key (.-key event)
        alt-key (.-altKey event)
        ctrl-key (.-ctrlKey event)
        shift-key (.-shiftKey event)
        meta-key (.-metaKey event)
        keycode-and-flags [keycode alt-key ctrl-key shift-key meta-key]]
    #_(println "KEY: " key
               ", CODE" (.-code event)
               ", KEYCODE" keycode
               ", WHICH" (.-which event)
               ", Alt, Cntr, Shift, Meta :::"
               keycode-and-flags)
    (println "print-key-from-event2: keycode-and-flags: " keycode-and-flags)
    keycode-and-flags
    )
  )


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
    (println "print-key-from-event: keycode-and-flags: " keycode-and-flags)
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


(defn mui-gui2
  "This is the function that displays the actual Mui console. It will be
  called by the application using Mui, not by anything within it so
  the IDE may identify it as unused."
  [mui-gui-cfg app-cmd-map]
  (let [mui-cmd-map-including-app-cmds
        (merge-in-app-cmds app-cmd-map mui-cmd-map)
        keystroke-handler
        (fn [event]
          (let [cmd-txtarea
                (. js/document getElementById "command-window")
                ;;keycode (.-keyCode event)
                keycode-with-modifiers (print-key-from-event2 event)
                key-keyword (key-with-modifiers-to-key-sym keycode-with-modifiers)
                mui-cmd (mui-cmd-map-including-app-cmds key-keyword)]
            (println "keystroke-handler (mui-gui2) :on-key-up, cursor is at "
                     (get-cursor-pos cmd-txtarea) ", mui-cmd: " mui-cmd ", key-keyword: " key-keyword)
            (when mui-cmd
              (case (:mode @mui-state)
                :normal
                #_(println "NORMAL MODE, Command Entered: " key-keyword)
                (let [active-in-normal ((mui-cmd :active-in-states) :normal)]
                  (println "mui-cmd is " key-keyword ", NORMAL MODE, active-in-normal: " active-in-normal)
                  (when active-in-normal
                    (set-mode :query mui-cmd key-keyword)
                    (load-prompts cmd-txtarea)))
                :query
                #_(println "QUERY MODE, Command Entered: " key-keyword)
                (let [active-in-query ((mui-cmd :active-in-states) :query)]
                  (println "mui-cmd is " key-keyword ", QUERY MODE, active-in-query: " active-in-query)
                  (when active-in-query
                    (let [command-fn (get-in mui-cmd-map-including-app-cmds [key-keyword :fn])]
                      (command-fn)
                      )
                    #_(set-mode :normal nil nil)
                    #_(load-prompts cmd-txtarea)))
                )
              )))]
    [:div
     [:div {:style {:width "45%" :margin "auto"}}
      [gpu/upload-control {} js/alert]
      [:label {:for "command-window"} "Command Entry: "]
      [:textarea (merge (:command-window mui-gui-cfg)
                        {:on-key-up   keystroke-handler
                         :on-key-down filter-keystrokes
                         :on-change   (fn [event]
                                        (let [cmd-txtarea (. js/document getElementById
                                                             "command-window")]
                                         #_(println ":on-change, cursor is at "
                                                   (get-cursor-pos cmd-txtarea))
                                         #_(println "IMPLICITS: " '(:implicits app-cmd-map))
                                          (swap! mui-state assoc
                                                 :implicits (:implicits mui-gui-cfg))))})]]
     [:div {:style {:width "45%" :margin "auto"}}
      [:label {:for "history-window"} "Command History: "]
      [:textarea (merge (:history-window mui-default-cfg)
                        {:value    (prettify-history @command-history)
                         :readOnly true})]]
     [:div {:style {:width "45%" :margin "auto"}}
      [:label {:for "help-window"} "Help: "]
      [:textarea (merge (:history-window mui-default-cfg)
                        {:value    (prettify-help mui-cmd-map-including-app-cmds)
                         :readOnly true})]]]))



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

                key (.-key event)
                keycode-and-flags (print-key-from-event event)
                key-keyword (first keycode-and-flags)
                mui-cmd
                (mui-cmd-map-including-app-cmds key-keyword)]
            (println "mui-gui :on-key-up, cursor is at "
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
      [gpu/upload-control {} js/alert]
      [:label {:for "command-window"} "Command Entry: "]
      [:textarea (merge (:command-window app-cfg)
                        {:on-key-up   keystroke-handler
                         :on-key-down filter-keystrokes
                         :on-change   (fn [event]
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
                        {:value    (prettify-history @command-history)
                         :readOnly true})]]
     [:div {:style {:width "45%" :margin "auto"}}
      [:label {:for "help-window"} "Help: "]
      [:textarea (merge (:history-window mui-default-cfg)
                        {:value    (prettify-help mui-cmd-map-including-app-cmds)
                         :readOnly true})]]]))
