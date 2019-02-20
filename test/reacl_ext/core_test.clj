(ns reacl-ext.core-test
  (:require [midje.sweet :refer :all]
            [reacl-ext.core :as ext]))

(fact
  (macroexpand-1 '(ext/defclass button
                    this [label]
                  
                    local-state [ls nil]
                  
                    methods [(click [_] (reacl2.core/send-message! this :clicked))]
  
                    render
                    (dom/button {:onclick click}
                                label)
  
                    handle-message
                    (fn [_]
                      (reacl2.core/return :action true))))
  =>
  `(let
       [..class..
        (reacl2.core/class
         "reacl-ext.core-test/button"
         ~'this
         [~'label]
         ~'local-state
         [~'ls
          (do
            (reacl-ext.extensions.core/set-extension-data!
             ~'this
             :reacl-ext.extensions.methods/methods
             {(quote ~'click) (fn [~'_] (reacl2.core/send-message! ~'this :clicked))})
            nil)]
         ~'render
         (let [{~'click (quote ~'click)}
               (reacl-ext.extensions.core/get-extension-data
                ~'this
                :reacl-ext.extensions.methods/methods)]
           (reacl-ext.context.runtime/with-initial-context*
            ~'this
            reacl-ext.context.state/not-available
            ~'ls
            (fn [] (dom/button {:onclick ~'click} ~'label))))
         ~'handle-message
         (let [..other.. (~'fn [~'_] (reacl2.core/return :action true))]
           (fn
             [..msg..]
             (reacl-ext.context.state/handle-state-messages
              ..msg..
              ..other..
              reacl-ext.context.state/not-available
              ~'ls)))
         )]
     (def
       ~'button
       (->
        (fn
          [~'label]
          (reacl-ext.context.runtime/instantiate-without-state
           ..class..
           [~'label]))
        (reacl-ext.context.runtime/set-reacl-class ..class..))))
  
  (provided (gensym "class") => ..class..
            (gensym "msg") => ..msg..
            (gensym "other") => ..other..)
  )
