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
    [gputils.core :as gpu]))


#_(def state (cljs.js/empty-state))
#_(defn evaluate [source cb]
    (cljs.js/eval-str state source nil {:eval cljs.js/js-eval :context :expr} cb))
;; This is how to do reflection - (:arglists (meta #'rasto.core/make-raster))


;; mui-state: Basically persistent state for the mui widget. Currently, distinct from
;; other mutable data.
(defonce mui-state (atom {:command-buffer ""
                          :mode           :normal
                          :query          {}}))


;; application-defined-types: Map of the types we can use.
(defonce application-defined-types (atom (sorted-map)))


;; mui-object-store: This is actually the memory of the mui language.
;; Anything you create or import into it gets stored here.  I did not
;; want rapidly changing things like the mouse position or
;; command/history buffers to share an atom with large data
;; structures.
(defonce mui-object-store
         (atom (sorted-map)))


(defonce mui-object-store-ids (atom (sorted-map)))
;; Add the set of identifiers so that we don't need the object type to retrieve it.
;; Also used to make certain that everything in the system has a unique id. We store
;; the object's type as the val in the key/val pair. So when we get a request for an id
;; it will tell us the type. Then we can fetch the object from the object-store using
;; the id and the type.
#_(swap! mui-object-store assoc :obj-ids (sorted-set))


;; mui-default-cfg: basically a few simple styles that keep the Mui
;; console somewhat more readable.
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


;; command-history: Pretty much what you might think. It's a simple list of what
;; has happened in a session.
(def command-history (atom '()))

;; conversion-fn-map: Tells the system how to turn raw text into various forms
;; of typed data.
(def conversion-fn-map {:int    #(let [parsed-int (js/parseInt %)]
                                   (when (js/isNaN parsed-int)
                                     (throw "Bad input for integer!"))
                                   parsed-int)
                        :float  #(let [parsed-float (js/parseFloat %)]
                                   (when (js/isNaN parsed-float)
                                     (throw "Bad input for float!"))
                                   parsed-float)
                        :yn     #(let [trimmed-input (str/trim %1)]
                                   (case trimmed-input
                                     ("y" "Y") "Y"
                                     ("n" "N") "N"
                                     (throw "Only 'y' or 'n' answers allowed!")
                                     ))                     ;; FIXME, needs default answer support.
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


(defn get-object-id-by-numbers
  "The user is periodically presented with a selection of things where he must
  choose a number to represent his choice. This function converts those numeric
  choices into object ids."
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
  "Associated with the 'select' command. Returns a numbered list of all
   objects of a given type. I don't think the second signature is used."
  ([map-atom query-text k]
   (choose-object map-atom query-text k nil))
  ([map-atom query-text k n]
   (let [objs (@map-atom k)
         obj-ids (keys objs)]
     (if (nil? n)
       (apply str query-text " " (prettify-list-to-string obj-ids))
       (nth obj-ids n)))))


(defn list-objects-of-type
  "Returns a selection list of all objects of type t."
  [t]
  (let [objects (@mui-object-store t)
        object-ids (keys objects)]
    (apply str "Select an object by entering its number: "
           (prettify-list-to-string object-ids))))


(defn command-buffer-clear []
  (swap! mui-state assoc :command-buffer "" :prompt-end 0))


#_(defn command-buffer-append [text]
    (swap! mui-state update :command-buffer str text))


(defn append-to-field
  "Utility function used by println-fld. Simply adds provided text to the end
   of the provided field."
  [field text]
  (let [field-obj (. js/document getElementById field)
        field-obj-val (. field-obj -value)]
    (set! (. field-obj -value) (str field-obj-val text))))


(defn println-fld
  "Prints the text into a certain input field. The field is a JS field
  object extracted from the DOM."
  [field text]
  (let [sh (.-scrollHeight field)]
    (append-to-field field (str "\n" text))
    (set! (. field -scrollTop) sh))
  ;; FIXME: This ^^ doesn't work to keep the textarea scrolled
  )


(defn prepare-query-for-history
  "Removes parts of the query that are not relevant for purposes of playback
   and reconstruction of sessions."
  [query]
  (let [query' (dissoc query :fn :help :active-in-states)
        args (:args query')
        args' (into {} (map (fn [[arg arg-data]]
                              [arg (:val arg-data)]) args))]
    (assoc query' :args args')))


(defn add-to-history
  "Simple function to tack an action to the end of the history queue."
  [args]
  (swap! command-history conj (prepare-query-for-history args)))


(defn present-prompt
  "Print one of the questions to ask the user for a parameter value in the
   command shell. The prompt may be a literal or a function. If a function the
   value of the function is printed."
  [arg-data textarea-element]
  (let [prompt (:prompt arg-data)
        prompt-text (if (= (type prompt) (type (fn [])))
                      (apply prompt ())
                      prompt)]
    (println-fld "command-window" (str prompt-text " "))
    (swap! mui-state assoc :prompt-end (.-selectionStart textarea-element))))


(defn set-mode
  "Switches back and forth between 'normal' and 'query' mode. Also loads the
   :query field of mui-state with the map of questions and tells us whether to
   return immediately to normal. Some commands piggy-back on others (like the
   'new' command) and so you want the system to stay in :query mode after the
   initial command executes."
  [mode query-map key-keyword]
  (let []
    #_(println "CALLED: set-mode (mode, query-map, command-key): " [mode query-map key-keyword])
    (swap! mui-state assoc
           :return-to-normal true
           :mode mode
           :query (assoc query-map :command-key key-keyword))))


(defn load-prompts
  "This function grabs the args for a given command and then presents the user
   with the first prompt that does not already have a :val key, meaning the
   first arg that has not yet been filled in by the user answering a prompt.
   The :fn in the :Enter key in :cmd-func-map is what keeps us cycling through
   the prompts until all args have concrete values. The (if arg-data...) sexp
   right after the let expression keeps prompting the user for input until
   there's nothing left in the arg-data map, then it moves on and calls the fn
   that uses the args that were just input."
  [textarea-element]
  (let [query-args (get-in @mui-state [:query :args])
        args-to-get (filter (fn [[_ arg-data]]
                              (nil? (:val arg-data))) query-args)
        [arg arg-data] (first args-to-get)]
    (swap! mui-state assoc :current-arg arg)
    (if arg-data
      (do #_(println "ARG-DATA FOUND: " arg-data)
        (present-prompt arg-data textarea-element))
      (let [command-fn (get-in @mui-state [:query :fn])]
        #_(println "Mui-STATE: redacted "                   ;; @mui-state
                   "FN: " command-fn
                   "\nARGS: " (pprint query-args))
        (add-to-history (:query @mui-state))
        (when command-fn (command-fn query-args))
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
   ;; FIXME: default-answer param is unsupported as yet.
   (let [prompt-text "Do you want to do this (y/n):"]
     (apply str
            prompt-text
            ))))


(defn select-object
  "Sets the selection for a given type of object in the object store."
  [obj-type obj-id]
  (let []
    (swap! application-defined-types assoc-in [obj-type :selection] obj-id)))

;; cmd-maps-atom: The command maps are modifiable at runtime by the user
;; and can also be loaded in from a file.
(def cmd-maps-atom (atom {}))

(def basic-cmd-maps
  "Prompts can now be functions that return text instead of static
  text. Any prompt whose text might change based on something that can
  change during a run should be a function so that it can build its
  text based on what's going on in the program."

  {:key-sym-keystroke-map {
                           :F2        [113 false false false false]
                           :Ctrl-C    [67 false true false false]
                           :ArrowDown [40 false false false false]
                           :ArrowUp   [38 false false false false]
                           :Enter     [13 false false false false]
                           :s         [83 false false false false]
                           :n         [78 false false false false]
                           :d         [68 false false false false]
                           }

   :cmd-func-map          {:Enter
                                      {:fn               (fn [arg-map]
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
                                                                        [:query :args current-arg :val] arg-val))
                                                               (catch js/Object e (println e))
                                                               (finally (do
                                                                          (command-buffer-clear)
                                                                          (load-prompts cmd-txtarea))))))
                                       :args             {}
                                       :help             nil
                                       :active-in-states (set [:query])}
                           :F2
                                      {:fn               (fn [arg-map]
                                                           (let [cmd-txtarea (. js/document getElementById "command-window")]
                                                             (set! (. cmd-txtarea -value) "")
                                                             (command-buffer-clear)  #_(swap! mui-state assoc :command-buffer "")))
                                       :args             {}
                                       :active-in-states (set [:normal])
                                       :help             {:msg "F2\t: Clear command window."}}
                           :s         {:fn               (fn [arg-map]
                                                           (let [selected-object-id-index
                                                                 (get-in (:query @mui-state) [:args :obj :val])
                                                                 selected-object-type-index
                                                                 (get-in (:query @mui-state) [:args :t :val])
                                                                 selected-object-id
                                                                 (get-object-id-by-numbers mui-object-store
                                                                                           selected-object-type-index
                                                                                           selected-object-id-index)
                                                                 selected-object-type
                                                                 (get-object-id-by-numbers mui-object-store
                                                                                           selected-object-type-index)]
                                                             (select-object selected-object-type selected-object-id)))
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
                                                             #_(println "Would Delete currently selected object?: " yes-or-no)))
                                       :active-in-states (set [:normal])
                                       :args             {:confirm
                                                          {:prompt (fn [] (confirm-action true))
                                                           :type   :yn}}
                                       :help             {:msg "d\t: Delete currently selected object."}}
                           :ArrowDown {:fn               (fn [arg-map]
                                                           (let [download-filename (get-in arg-map [:download-filename :val])
                                                                 data @cmd-maps-atom
                                                                 data-map {:download-filename download-filename
                                                                           :data              data}]
                                                             (gpu/send-data data-map download-filename)))
                                       :active-in-states (set [:normal])
                                       :args             {:download-filename
                                                          {:prompt "Enter a filename:"
                                                           :type   :string}}
                                       :help             {:msg "↓\t: Download the current command map to an edn file."}}
                           :Ctrl-C    {:fn               (fn [arg-map]
                                                           (let []
                                                             (set-mode :normal nil nil)
                                                             (println-fld "command-window" "-- COMMAND TERMINATED! --\n")))
                                       :help             {:msg "Ctrl-C\t: Abort command."}
                                       :active-in-states (set [:normal :query])
                                       :args             {}}
                           :n
                                      {:fn               (fn [arg-map]
                                                           (let [cmd-txtarea (. js/document getElementById "command-window")
                                                                 user-selection-index (get-in arg-map [:t :val])
                                                                 selected-type-name (nth (keys @application-defined-types) user-selection-index)
                                                                 new-object-query (get-in @application-defined-types [selected-type-name :prompts :new])]
                                                             (set-mode :query new-object-query :nb)
                                                             ;; We don't return to normal mode so the queries for the
                                                             ;; constructor args will be processed.
                                                             (swap! mui-state assoc :return-to-normal false)
                                                             (load-prompts cmd-txtarea)))
                                       :help             {:msg "n\t: Create a new object."}
                                       :active-in-states (set [:normal])
                                       :args
                                                         {:t
                                                          {:prompt #(choose-type)
                                                           :type   :int}}}}})
;; basic-cmd-maps: A persistent object that stores all the basic commands
;; and is loaded into the cmd-maps-atom at the start of each run.
(reset! cmd-maps-atom basic-cmd-maps)

;; keystroke-to-key-sym-map-atom: A keystroke is represented by a 5-vector
;; [keycode alt-key ctrl-key shift-key meta-key]. This should uniquely identify
;; which command we want. This inverse mapping takes that 5-vec and converts
;; it into a command keyword.
(def keystroke-to-key-sym-map-atom
  (atom (set/map-invert (:key-sym-keystroke-map @cmd-maps-atom))))


(defn build-cmd-maps
  "The idea here is that the lowest level command map is the one in mui-core.
  In the Rasto app (the first app to use Mui for command and control) we also
  have the idea that Rasto is actually just a generic widget that can itself be
  used by other apps. So the Rasto core commands are then supplemented by
  whatever commands are added by the app that uses rasto.core. We thus build
  our command maps using a series of these 'squash' operations is a fold-like
  pattern. So [rasto-core-cmd-maps, rasto-app-cmd-maps] --> app-level-cmd-maps,
  [mui-core-cmd-maps, app-level-cmd-maps] --> overall-runtime-cmd-maps."
  [lower-level-cmd-maps upper-level-cmd-maps]

  (let [ll-key-sym-keystroke-map (:key-sym-keystroke-map lower-level-cmd-maps)
        ul-key-sym-keystroke-map (:key-sym-keystroke-map upper-level-cmd-maps)
        ll-cmd-func-map (:cmd-func-map lower-level-cmd-maps)
        ul-cmd-func-map (:cmd-func-map upper-level-cmd-maps)
        merged-cmd-maps {:key-sym-keystroke-map (merge ll-key-sym-keystroke-map ul-key-sym-keystroke-map)
                         :cmd-func-map          (merge ll-cmd-func-map ul-cmd-func-map)}]
    merged-cmd-maps))


(defn register-application-defined-type
  "Let Mui know about a new type that can be instantiated, destroyed, etc..."
  [type-name constructor-prompts]
  (swap! application-defined-types assoc type-name {:prompts   constructor-prompts
                                                    :selection nil}))


(defn add-object-to-object-store
  "Puts an object of obj-type into the object store using obj-id as its
   persistent identifier. If obj is part of a tree structure, there is a field
   for the id of the parent node. Also sets the selection for this type of
   object to the newly interred object."
  [obj obj-type obj-id parent-obj-id]
  (let [obj-id-set @mui-object-store-ids
        obj-id-set' (conj @mui-object-store-ids {obj-id obj-type})]
    (if (= obj-id-set obj-id-set')
      (println "ERROR: identifier already in use!")         ;; FIXME: needs to throw exception.
      (do
        (reset! mui-object-store-ids obj-id-set')
        (swap! mui-object-store assoc-in [obj-type obj-id] {:obj obj :parent-obj-id parent-obj-id})
        (select-object obj-type obj-id)))))


(defn get-object-from-object-store
  "Returns the object with the id given by obj-id. Using the second form of
   the function is faster if you know the type."
  ([obj-id]
   (let [obj-type (get @mui-object-store-ids obj-id)]
     (get-object-from-object-store obj-id obj-type)))
  ([obj-id obj-type]
   (get-in @mui-object-store [obj-type obj-id])))


;; tickets: Just a simple way to generate unique ids.
(def tickets (atom 0))


(defn prettify-history
  "Adds a newline after each history item so the list is actually readable by
   people."
  [history-list]
  (reduce #(str %1 %2) ""
          (reverse (map (fn [history-item]
                          (str history-item "\n")) history-list))))


(defn set-cursor-pos
  "This and get-cursor-pos are meant to help navigate within a textarea."
  [input-element offset]
  (let [current-pos (.-selectionStart input-element)]
    (set! (.-selectionStart input-element) (+ current-pos offset))))


(defn get-cursor-pos
  "This and set-cursor-pos are meant to help navigate within a textarea."
  [input-element]
  (.-selectionStart input-element))


(defn filter-keystrokes
  "Returns event.preventDefault() if the key is not supposed to be allowed.
   Basically, we do not allow back arrow or backspace when the cursor is at the
   end of something the system printed. In other words, we don't want the user
   backing up over stuff the system produced, just like you can't backspace
   over the shell prompt in Bash for instance. The up and down arrows are
   always forbidden just to simplify keeping track of user input."
  [event]
  (let [keycode (.-keyCode event)
        cmd-txtarea
        (. js/document getElementById "command-window")
        prompt-end-pos (:prompt-end @mui-state)
        cursor-pos (.-selectionStart cmd-txtarea)
        selection-end (.-selectionEnd cmd-txtarea)
        ]
    (case keycode
      (8 37) (when (< cursor-pos (inc prompt-end-pos))
               (do #_(println "Trying to stop cursor from going back more.")
                 (.preventDefault event)))                  ;; (:Backspace :ArrowLeft)
      (38 40) (.preventDefault event)                       ;; (:ArrowUp :ArrowDown)
      "default")))


(defn format-keycode-and-flags
  "This function takes the event and formats the relevant parts into a
   5-vector that can be used to look up the symbol for the command the user
   want to run."
  [event]
  (let [keycode (.-keyCode event)
        alt-key (.-altKey event)
        ctrl-key (.-ctrlKey event)
        shift-key (.-shiftKey event)
        meta-key (.-metaKey event)
        keycode-and-flags [keycode alt-key ctrl-key shift-key meta-key]]
    #_(println "format-keycode-and-flags: keycode-and-flags: " keycode-and-flags)
    keycode-and-flags))


#_(defn report-keys [args]
    (println "REPORT-KEYS~!!!!!")
    (mapv (fn [arg] (println "ARG:" (keys arg))) args))


(defn prettify-help
  "Picks out the help text summary for each command and puts a newline
  after it to make the help screen look tidy."
  [mui-cmd-map-including-app-cmds]
  (apply str (map (fn [[key-keyword cmd-map]]
                    (str (get-in cmd-map [:help :msg]) "\n")
                    ) mui-cmd-map-including-app-cmds)))


(defn mui-gui2
  "This is the function that displays the actual Mui console. It will be
  called by the application using Mui, not by anything within mui.core so the
  IDE may identify it as unused."

  ;; FIXME: probably shouldn't do the reset! ops in the let block but I want them
  ;; FIXME: to run every time the keycode handler is called too.

  [mui-gui-cfg app-cmd-maps]
  (reset! cmd-maps-atom (build-cmd-maps basic-cmd-maps app-cmd-maps))
  (let [_ (reset! cmd-maps-atom (build-cmd-maps basic-cmd-maps app-cmd-maps))
        _ (reset! keystroke-to-key-sym-map-atom (set/map-invert (:key-sym-keystroke-map @cmd-maps-atom)))
        keystroke-handler
        (fn [event]
          (let [cmd-txtarea
                (. js/document getElementById "command-window")
                keycode-with-modifiers (format-keycode-and-flags event)
                key-keyword (@keystroke-to-key-sym-map-atom keycode-with-modifiers)
                mui-cmd ((:cmd-func-map @cmd-maps-atom) key-keyword)]
            (reset! cmd-maps-atom (build-cmd-maps basic-cmd-maps app-cmd-maps))
            #_(println "keystroke-handler (mui-gui2) :on-key-up, cursor is at "
                       (get-cursor-pos cmd-txtarea) ", mui-cmd: " mui-cmd ", key-keyword: " key-keyword)
            (when mui-cmd
              (case (:mode @mui-state)
                :normal
                (let [active-in-normal ((mui-cmd :active-in-states) :normal)]
                  #_(println "mui-cmd is " key-keyword ", NORMAL MODE, active-in-normal: " active-in-normal)
                  (when active-in-normal
                    (set-mode :query mui-cmd key-keyword)
                    (load-prompts cmd-txtarea)))
                :query
                #_(println "QUERY MODE, Command Entered: " key-keyword)
                (let [active-in-query ((mui-cmd :active-in-states) :query)]
                  #_(println "mui-cmd is " key-keyword ", QUERY MODE, active-in-query: " active-in-query)
                  (when active-in-query
                    (let [command-fn (get-in (:cmd-func-map @cmd-maps-atom) [key-keyword :fn])]
                      (command-fn)
                      )))))))]
    [:div {:id "mui-gui"}
     [:div {:style {:width "45%" :margin "auto"}}
      #_[gpu/upload-control {} js/alert]
      [:label {:for "command-window"} "Command Entry: "]
      [:textarea (merge (:command-window mui-gui-cfg)
                        {:on-key-up   keystroke-handler
                         :on-key-down filter-keystrokes
                         :on-change   (fn [event])})]]
     [:div {:style {:width "45%" :margin "auto"}}
      [:label {:for "history-window"} "Command History: "]
      [:textarea (merge (:history-window mui-default-cfg)
                        {:value    (prettify-history @command-history)
                         :readOnly true})]]
     [:div {:style {:width "45%" :margin "auto"}}
      [:label {:for "help-window"} "Help: "]
      [:textarea (merge (:history-window mui-default-cfg)
                        {:value    (prettify-help (:cmd-func-map @cmd-maps-atom))
                         :readOnly true})]]]))
