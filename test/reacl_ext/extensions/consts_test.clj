(ns reacl-ext.extensions.consts-test
  (:require [midje.sweet :refer :all]
            [reacl-ext.extensions.core :as core]
            reacl-ext.extensions.consts))

(facts "about the consts extension"
  (fact "works with existing localstate"
    (core/class-extension 'consts
                          `[~'x ~..vx..
                            ~'y ~..vy..]
                          {'local-state `[baz ~..st..]
                           'render ..dom..}
                          ..myclass..
                          'this
                          ..app-state..
                          ..params..)
    =>
    `{~'local-state [baz
                     (do
                       (reacl-ext.extensions.core/set-extension-data!
                        ~'this
                        :reacl-ext.extensions.consts/consts
                        {(quote ~'x) ~..vx.. (quote ~'y) ~..vy..})
                       ~..st..)]
      ~'render (clojure.core/let
                   [{~'x (quote ~'x) ~'y (quote ~'y)}
                    (reacl-ext.extensions.core/get-extension-data
                     ~'this
                     :reacl-ext.extensions.consts/consts)]
                 ~..dom..)})
  
  (fact "works with anonymous localstate"
    (-> (core/class-extension 'consts
                              `[~'x ~..vx..
                                ~'y ~..vy..]
                              {'render ..dom..}
                              ..myclass..
                              'this
                              ..app-state..
                              ..params..)
        (get 'local-state)
        second)
    =>
    `(do
       (reacl-ext.extensions.core/set-extension-data!
        ~'this
        :reacl-ext.extensions.consts/consts
        {(quote ~'x) ~..vx.. (quote ~'y) ~..vy..})
       nil)))
