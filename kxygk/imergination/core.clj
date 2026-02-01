(ns
    kxygk.imergination.core
  "The GUI state and renderer are initialized and launched"
  (:require [cljfx.api :as fx]
            [kxygk.imergination.event :as event]
            [kxygk.imergination.gui :as gui]
            [kxygk.imergination.state :as state])
  (:import javafx.application.Platform)
  (:gen-class :main true))

(set!
  *warn-on-reflection*
  true)


(def renderer
  "As per the `cljfx` API this:
     `manages the lifecycle of a component`
  turns elements/descriptions into something called a `lifecycle`
  the `lifecycle` specifies how to interact with the component
  then does reconcilation
  then updates the UI tree"
  (fx/create-renderer
    :middleware (comp
                  ;; ensures you are
                  ;; `passing context (ie the state with subscriptions)
                  ;; to all lifecycles in a component graph`
                  fx/wrap-context-desc
                  ;; `fx/wrap-map-desc` docstring:
                  ;; Returns middleware function that applies f
                  ;; to passed description and passes it further
                  ;;
                  ;; Since we won't pass anything to the renderer..
                  ;; (we will just `fx/mount-renderer` to track the state )
                  ;; this is a dummy function that just get the gui `root` node
                  ;; all the input is simply ignored
                  (fx/wrap-map-desc
                    (fn [_]
                      {:fx/type gui/root})))
    :opts {;; component events are sent to the `event/handler`
           ;; see `event.clj` for details.. but essentially:
           ;; - the even handler derefs the state
           ;; - it passes the state to the handler function
           ;; - the result is then written to the state atom w/ `reset!`
           :fx.opt/map-event-handler event/dispatcher 
           ;; this part sets up use of contexts .. but it's baroque
           ;;
           ;; `fx/keyword->lifecycle` docstring:
           ;; When given fitting keyword,
           ;; returns lifecycle for corresponding JavaFX class
           ;;
           ;; `fx/fn->lifecycle-with-context` docstring:
           ;; When given function,
           ;; returns lifecycle that uses said function with context
           :fx.opt/type->lifecycle   #(or
                                        (fx/keyword->lifecycle
                                          %) 
                                        (fx/fn->lifecycle-with-context
                                          %))}))


(defn -main [& args]
  ;; Make the application exit when you close all the windows
  (Platform/setImplicitExit true)
  (fx/mount-renderer
    state/*selections
    renderer))

;; MANUAL REPL STUFF
;; `renderer` is called each time the state atom changes
#_
(fx/mount-renderer
  state/*selections
  renderer)

#_
(renderer)
#_
(require 'eof1)
#_
(require 'summary)
#_
(require 'varfield)
