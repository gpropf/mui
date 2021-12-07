(ns mui.example
  "This module basically exists only as an entry point for the
  code. It's essentially 'int main ()' and shouldn't really do much
  itself. We call the function that creates the GUI from here and this
  kicks everything else off."
  (:require
   [mui.core :as mc]
   [reagent.dom :as rd]
   [clojure.string :as str]
   [reagent.core :as reagent :refer [atom]]
   [cljs.pprint :as pp :refer [pprint]]
   [gputils.core :as gpu]
   ))

(enable-console-print!)


(defonce
  mui-example-cfg {; :off-cell-color "#F5F5DC"

                  :mui-cfg {:command-window {:style {:height "auto"
                                                     :margin-bottom "5px"
                                                     :float "right"}
                                             :id "command-window"
                                             :rows "8"
                                             :cols "60"
                                             :class ""
                                             ;:default-value ""
                                             }
                            :app-cmds {}
                            }})



(mc/register-application-defined-type "T1" {:b 2})
(mc/register-application-defined-type "T2" {:c 3})

(def test-data-map {:download-filename "gputils-test.edn"
                    :data [1 2 3 4 5 :a :b :foo]})

#_(gpu/send-data test-data-map)

#_(def parsed-upload (gpu/fetch-and-parse-uploaded-file! nil prn))

(defn app
  "Creates the app and all its controls.  Everything we use is called
  from here."
  []
  [:div {}
   [mc/mui-gui2 (mui-example-cfg :mui-cfg) {}]])


(defn render-app
  "Call into Reagent to attach the app to a particular JS DOM id and
  make it visible."
  []
  (rd/render
   [app]
   (js/document.getElementById "app")))


(render-app)


(defn ^:after-load re-render []
  (render-app))


(defonce start-up (render-app))
