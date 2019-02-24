(ns reacl-ext.context-test
  (:require [midje.sweet :refer :all]
            [reacl-ext.context :as ctx]
            [reacl-ext.context.translation :as t]))

(facts "about create-ctx-class"
  (fact "works with simple params"
    (t/create-ctx-class 'myclass true '[a b]
                        ..class..)
    =>
    `(reify
       ~@(t/reify-reacl-class ..class..)
       ~'IFn
       (~'-invoke [~'_ ~'a ~'b] (reacl-ext.context.runtime/instantiate-with-state ..class.. [~'a ~'b]))
       (~'-invoke [~'_ ~'app-state ~'a ~'b] (reacl-ext.context.runtime/instantiate-with-state ..class.. (cons ~'app-state [~'a ~'b])))
       (~'-invoke [~'_ ~'opt ~'app-state ~'a ~'b] (reacl-ext.context.runtime/instantiate-with-state ..class.. (cons ~'opt (cons ~'app-state [~'a ~'b])))))
    (provided (gensym "opt") => 'opt
              (gensym "app-state") => 'app-state))

  (fact "works with var-args"
    (t/create-ctx-class 'myclass false '[a b & c]
                        ..class..)
    =>
    `(reify
       ~@(t/reify-reacl-class ..class..)
       ~'IFn
       (~'-invoke [~'_ ~'a ~'b & ~'c] (reacl-ext.context.runtime/instantiate-without-state ..class.. (apply list ~'a ~'b ~'c)))
       (~'-invoke [~'_ ~'opt ~'a ~'b & ~'c] (reacl-ext.context.runtime/instantiate-without-state ..class.. (cons ~'opt (apply list ~'a ~'b ~'c)))))
    (provided (gensym "opt") => 'opt)))

(facts "about apply-context"
  (fact "works without states"
    (t/apply-context {'render ..dom..
                      'handle-message ..handler..}
                     'myclass 'that nil)
    =>
    `{~'handle-message ..handler..
      ~'render (reacl-ext.context.runtime/with-initial-context*
                 ~'that reacl-ext.context.state/not-available reacl-ext.context.state/not-available
                 (fn [] ..dom..))})

  (fact "works with an app-state"
    (t/apply-context {'render ..dom..}
                     'myclass 'that ..app-state..)
    =>
    `{~'handle-message (let [..other.. reacl-ext.context.state/fallback-handle-message]
                         (fn [..msg..] (reacl-ext.context.state/handle-state-messages ..msg.. ..other.. ..app-state.. reacl-ext.context.state/not-available)))
      ~'render (reacl-ext.context.runtime/with-initial-context*
                 ~'that ..app-state.. reacl-ext.context.state/not-available
                 (fn [] ..dom..))}
    (provided (gensym "msg") => ..msg..
              (gensym "other") => ..other..))

  (fact "works with a local-state"
    (t/apply-context {'render ..dom..
                      'local-state '[st ..state..]
                      'handle-message ..handler..}
                     'myclass 'that nil)
    =>
    `{~'handle-message (let [..other.. ..handler..]
                         (fn [..msg..] (reacl-ext.context.state/handle-state-messages ..msg.. ..other.. reacl-ext.context.state/not-available ~'st)))
      ~'local-state ~['st ..state..]
      ~'render (reacl-ext.context.runtime/with-initial-context*
                 ~'that reacl-ext.context.state/not-available ~'st
                 (fn [] ..dom..))}
    (provided (gensym "msg") => ..msg..
              (gensym "other") => ..other..))

  (fact "works with both app and local state"
    (t/apply-context {'render ..dom..
                      'local-state '[st ..state..]
                      'handle-message ..handler..}
                     'myclass 'that ..app-state..)
    =>
    `{~'handle-message (let [..other.. ..handler..]
                         (fn [..msg..] (reacl-ext.context.state/handle-state-messages ..msg.. ..other.. ..app-state.. ~'st)))
      ~'local-state ~['st ..state..]
      ~'render (reacl-ext.context.runtime/with-initial-context*
                 ~'that ..app-state.. ~'st
                 (fn [] ..dom..))}
    (provided (gensym "msg") => ..msg..
              (gensym "other") => ..other..))
  
  )
