(ns reacl-ext.context-test
  (:require [midje.sweet :refer :all]
            [reacl-ext.context :as ctx]
            [reacl-ext.context.impl :as impl]))

(facts "about instantiator-defn"
  (fact "works with simple params"
    (impl/instantiator-defn 'myclass "docs" true '[a b]
                            ..class..)
    =>
    `(defn
       (vary-meta ~'myclass assoc :doc "docs")
       [~'a ~'b]
       (impl/instantiate-with-state ~..class.. [~'a ~'b])))

  (fact "works with var-args"
    (impl/instantiator-defn 'myclass nil false '[a b & c]
                            ..class..)
    =>
    `(defn
       ~'myclass
       [~'a ~'b & ~'c]
       (impl/instantiate-without-state ~..class.. (apply list ~'a ~'b ~'c))))
  )

(facts "about apply-context"
  (fact "works without states"
    (impl/apply-context {'render ..dom..
                         'handle-message ..handler..}
                        'myclass 'that nil)
    =>
    `{~'handle-message ~..handler..
      ~'render (binding [impl/*context* (impl/empty-context ~'that)]
                 ~..dom..)})

  (fact "works with an app-state"
    (impl/apply-context {'render ..dom..
                         'handle-message ..handler..}
                        'myclass 'that ..app-state..)
    =>
    `{~'handle-message (reacl-ext.context.state/wrap-handle-state-messages ~..handler.. ~..app-state.. reacl-ext.context.state/not-available)
      ~'render (binding [impl/*context* (impl/empty-context ~'that)]
                 (let [~..app-state.. (reacl-ext.context.state/->AppState ~'that ~..app-state.. active.clojure.lens/id)]
                   ~..dom..))})

  (fact "works with a local-state"
    (impl/apply-context {'render ..dom..
                         'local-state '[st ..state..]
                         'handle-message ..handler..}
                        'myclass 'that nil)
    =>
    `{~'handle-message (reacl-ext.context.state/wrap-handle-state-messages ~..handler.. reacl-ext.context.state/not-available ~'st)
      ~'local-state ~['st ..state..]
      ~'render (binding [impl/*context* (impl/empty-context ~'that)]
                 (let [~'st (reacl-ext.context.state/->LocalState ~'that ~'st active.clojure.lens/id)]
                   ~..dom..))})

  (fact "works with both app and local state"
    (impl/apply-context {'render ..dom..
                         'local-state '[st ..state..]
                         'handle-message ..handler..}
                        'myclass 'that ..app-state..)
    =>
    `{~'handle-message (reacl-ext.context.state/wrap-handle-state-messages ~..handler.. ~..app-state.. ~'st)
      ~'local-state ~['st ..state..]
      ~'render (binding [impl/*context* (impl/empty-context ~'that)]
                 (let [~'st (reacl-ext.context.state/->LocalState ~'that ~'st active.clojure.lens/id)]
                   (let [~..app-state.. (reacl-ext.context.state/->AppState ~'that ~..app-state.. active.clojure.lens/id)]
                     ~..dom..)))})
  
  
  )
