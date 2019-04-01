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
       (~'-invoke [~'_ ~'app-state ~'a ~'b] (reacl-ext.context.runtime/instantiate-with-state ..class.. [~'app-state ~'a ~'b]))
       (~'-invoke [~'_ ~'opt ~'app-state ~'a ~'b] (reacl-ext.context.runtime/instantiate-with-state ..class.. [~'opt ~'app-state ~'a ~'b])))
    (provided (gensym "opt") => 'opt
              (gensym "app-state") => 'app-state))

  (fact "works with var-args"
    (t/create-ctx-class 'myclass false '[a b & c]
                        ..class..)
    =>
    `(reify
       ~@(t/reify-reacl-class ..class..)
       ~'IFn
       (~'-invoke [~'_ ~'a ~'b] (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2] (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3] (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14 ~'arg15]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14 ~'arg15]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14 ~'arg15 ~'arg16]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14 ~'arg15 ~'arg16]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14 ~'arg15 ~'arg16 ~'arg17]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14 ~'arg15 ~'arg16 ~'arg17]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14 ~'arg15 ~'arg16 ~'arg17 ~'arg18]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14 ~'arg15 ~'arg16 ~'arg17 ~'arg18]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14 ~'arg15 ~'arg16 ~'arg17 ~'arg18 ~'arg19]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14 ~'arg15 ~'arg16 ~'arg17 ~'arg18 ~'arg19]))
       (~'-invoke [~'_ ~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14 ~'arg15 ~'arg16 ~'arg17 ~'arg18 ~'arg19 ~'rest]
         (reacl-ext.context.runtime/instantiate-without-state ..class.. (concat [~'a ~'b ~'arg2 ~'arg3 ~'arg4 ~'arg5 ~'arg6 ~'arg7 ~'arg8 ~'arg9 ~'arg10 ~'arg11 ~'arg12 ~'arg13 ~'arg14 ~'arg15 ~'arg16 ~'arg17 ~'arg18 ~'arg19]
                                                                                 ~'rest)))
       )
    (provided (gensym "opt") => 'opt
              (gensym "arg2") => 'arg2
              (gensym "arg3") => 'arg3
              (gensym "arg4") => 'arg4
              (gensym "arg5") => 'arg5
              (gensym "arg6") => 'arg6
              (gensym "arg7") => 'arg7
              (gensym "arg8") => 'arg8
              (gensym "arg9") => 'arg9
              (gensym "arg10") => 'arg10
              (gensym "arg11") => 'arg11
              (gensym "arg12") => 'arg12
              (gensym "arg13") => 'arg13
              (gensym "arg14") => 'arg14
              (gensym "arg15") => 'arg15
              (gensym "arg16") => 'arg16
              (gensym "arg17") => 'arg17
              (gensym "arg18") => 'arg18
              (gensym "arg19") => 'arg19
              (gensym "rest") => 'rest)))

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
