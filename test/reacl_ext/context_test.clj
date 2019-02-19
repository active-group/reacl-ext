(ns reacl-ext.context-test
  (:require [midje.sweet :refer :all]
            [reacl-ext.context :as ctx]
            [reacl-ext.context.impl :as impl]))

(facts "about instantiator-defn"
  (fact "works with simple params"
    (impl/instantiator-defn 'myclass "docs" true '[a b]
                            ..class..)
    =>
    `(def
       (vary-meta ~'myclass assoc :doc "docs" :arglists '~'([a b]))
       (-> (fn [~'a ~'b]
             (impl/instantiate-with-state ~..class.. [~'a ~'b]))
           (with-meta {:reacl-ext.context.impl/reacl-class ..class..}))))

  (fact "works with var-args"
    (impl/instantiator-defn 'myclass nil false '[a b & c]
                            ..class..)
    =>
    `(def
       (vary-meta ~'myclass assoc :arglists '~'([a b & c]))
       (-> (fn [~'a ~'b & ~'c]
             (impl/instantiate-without-state ~..class.. (apply list ~'a ~'b ~'c)))
           (with-meta {:reacl-ext.context.impl/reacl-class ..class..}))))
  )

(facts "about apply-context"
  (fact "works without states"
    (impl/apply-context {'render ..dom..
                         'handle-message ..handler..}
                        'myclass 'that nil)
    =>
    `{~'handle-message ~..handler..
      ~'render (binding [impl/*context* (impl/initial-context ~'that reacl-ext.context.state/not-available reacl-ext.context.state/not-available)]
                 ~..dom..)})

  (fact "works with an app-state"
    (impl/apply-context {'render ..dom..
                         'handle-message ..handler..}
                        'myclass 'that ..app-state..)
    =>
    `{~'handle-message (reacl-ext.context.state/wrap-handle-state-messages ~..handler.. ~..app-state.. reacl-ext.context.state/not-available)
      ~'render (binding [impl/*context* (impl/initial-context ~'that ~..app-state.. reacl-ext.context.state/not-available)]
                 ~..dom..)})

  (fact "works with a local-state"
    (impl/apply-context {'render ..dom..
                         'local-state '[st ..state..]
                         'handle-message ..handler..}
                        'myclass 'that nil)
    =>
    `{~'handle-message (reacl-ext.context.state/wrap-handle-state-messages ~..handler.. reacl-ext.context.state/not-available ~'st)
      ~'local-state ~['st ..state..]
      ~'render (binding [impl/*context* (impl/initial-context ~'that reacl-ext.context.state/not-available ~'st)]
                 ~..dom..)})

  (fact "works with both app and local state"
    (impl/apply-context {'render ..dom..
                         'local-state '[st ..state..]
                         'handle-message ..handler..}
                        'myclass 'that ..app-state..)
    =>
    `{~'handle-message (reacl-ext.context.state/wrap-handle-state-messages ~..handler.. ~..app-state.. ~'st)
      ~'local-state ~['st ..state..]
      ~'render (binding [impl/*context* (impl/initial-context ~'that ~..app-state.. ~'st)]
                 ~..dom..)})
  
  
  )
