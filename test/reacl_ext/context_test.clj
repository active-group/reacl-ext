(ns reacl-ext.context-test
  (:require [midje.sweet :refer :all]
            [reacl-ext.context :as ctx]
            [reacl-ext.context.impl :as impl]))

(facts "about instantiator-defn"
  (fact "works with simple params"
    (impl/instantiator-defn 'myclass "docs" true '[a b]
                            ..class..)
    =>
    `(def ~'myclass
       (-> (fn [~'a ~'b]
             (impl/instantiate-with-state ~..class.. [~'a ~'b]))
           (with-meta {:reacl-ext.context.impl/reacl-class ..class..}))))

  (fact "works with var-args"
    (impl/instantiator-defn 'myclass nil false '[a b & c]
                            ..class..)
    =>
    `(def
       ~'myclass
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
      ~'render (impl/wrap-initial-context* ~'that reacl-ext.context.state/not-available reacl-ext.context.state/not-available
                                           (fn [] ~..dom..))})

  (fact "works with an app-state"
    (impl/apply-context {'render ..dom..}
                        'myclass 'that ..app-state..)
    =>
    `{~'handle-message (let [~..other.. reacl-ext.context.state/fallback-handle-message]
                         (fn [~..msg..] (reacl-ext.context.state/handle-state-messages ~..msg.. ~..other.. ~..app-state.. reacl-ext.context.state/not-available)))
      ~'render (impl/wrap-initial-context* ~'that ~..app-state.. reacl-ext.context.state/not-available
                                           (fn [] ~..dom..))}
    (provided (gensym "msg") => ..msg..
              (gensym "other") => ..other..))

  (fact "works with a local-state"
    (impl/apply-context {'render ..dom..
                         'local-state '[st ..state..]
                         'handle-message ..handler..}
                        'myclass 'that nil)
    =>
    `{~'handle-message (let [~..other.. ~..handler..]
                         (fn [~..msg..] (reacl-ext.context.state/handle-state-messages ~..msg.. ~..other.. reacl-ext.context.state/not-available ~'st)))
      ~'local-state ~['st ..state..]
      ~'render (impl/wrap-initial-context* ~'that reacl-ext.context.state/not-available ~'st
                                           (fn [] ~..dom..))}
    (provided (gensym "msg") => ..msg..
              (gensym "other") => ..other..))

  (fact "works with both app and local state"
    (impl/apply-context {'render ..dom..
                         'local-state '[st ..state..]
                         'handle-message ..handler..}
                        'myclass 'that ..app-state..)
    =>
    `{~'handle-message (let [~..other.. ~..handler..]
                         (fn [~..msg..] (reacl-ext.context.state/handle-state-messages ~..msg.. ~..other.. ~..app-state.. ~'st)))
      ~'local-state ~['st ..state..]
      ~'render (impl/wrap-initial-context* ~'that ~..app-state.. ~'st
                                           (fn [] ~..dom..))}
    (provided (gensym "msg") => ..msg..
              (gensym "other") => ..other..))
  
  )
