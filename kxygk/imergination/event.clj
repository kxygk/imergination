(ns
    kxygk.imergination.event
  "GUI events and the handler"
  (:require [cljfx.api :as fx]
            [kxygk.imergination.state :as state]))

(defn-
  wrap-with-map
  "Gets a map with
  - a state `:snapshot`
  - an `:effect` function
  Calls the effect on the snapshot
  Returns a map with the new state at key `:updated-context`"
  [{:keys [snapshot
           effect]
    :as   event}]
#_  (println
    "Event happening:\nindex"
    event)
  {:updated-context (effect
                      snapshot
                      (dissoc event
                              :effect
                              :snapshot))})

(def
  dispatcher
  "This..
  - derefs the state
  - it passes this state `snapshot` to the `wrap-with-map`
  (which calls the `effect` function and returns a new state)
  - the result is then written to the state atom w/ `reset!`"
  (-> wrap-with-map
      (fx/wrap-co-effects
        ;; basically takes the event map
        ;; (which has keys like `:text`)
        ;; and adds another key to it (here it's `:state`)
        ;; which will have the current `deref`ed state
        {:snapshot #(deref
                      state/*selections)})
      (fx/wrap-effects
        ;; the function takes the event handler output
        ;; (which will be an updated state)
        ;; and then
        ;; overwrites the state-atom's state with it
        {:updated-context (fn
                            [state
                             _]
                            (reset!
                              state/*selections ;; the state atom
                              state))})))


