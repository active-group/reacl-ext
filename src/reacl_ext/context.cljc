(ns reacl-ext.context
  (:require [reacl2.core :as reacl]
            #?(:cljs [reacl-ext.context.impl :as impl])
            #?(:cljs [reacl-ext.context.state :as state])
            #?(:cljs goog.async.nextTick)))

#?(:cljs
   (defn- init-context-field [field value thunk]
     (impl/update-context
      (fn [context]
        (assert (or (= field :state) (= field :reaction)))
        (assert (not (:state context)))
        (assert (not (:reaction context)))
        (assoc context field value))
      thunk)))

#?(:cljs
   (defn- update-context-field [field f thunk]
     (impl/update-context
      (fn [context]
        (assert (or (not= field :state) (:state context)))
        (assert (not= field :reaction))
        (update context field f))
      thunk)))

#_(:cljs TODO
   (defn render-component [element clazz & rst]
     (apply reacl/render-component element
            (reacl-class clazz)
            rst)))

#?(:cljs
   (defn bind* [state thunk]
     (init-context-field :state state
                         thunk)))

#?(:clj
   (defmacro bind [state form]
     `(bind* ~state (fn [] ~form))))

#?(:cljs
   (defn focus* [lens thunk]
     (update-context-field :state #(state/-focus % lens)
                           thunk)))

#?(:clj
   (defmacro focus [lens form]
     `(focus* ~lens (fn [] ~form))))

#?(:cljs
   (defn- compose-reduce-action [outer inner]
     (assert (ifn? inner))
     (if (nil? outer)
       inner
       ;; TODO: unless the outer returns app-state it is possible:
       (throw (ex-info "Reducing actions cannot be composed yet." {})))))

#?(:cljs
   (defn reduce-action* [f thunk]
     (update-context-field :reduce-action #(compose-reduce-action % f)  ;; TODO: non-generative fn!
                           thunk)))

#?(:clj
   (defmacro reduce-action [f form]
     `(reduce-action* ~f (fn [] ~form))))

#?(:cljs
   (defn map-action* [thunk f & args]
     (reduce-action* (fn [app-state action] ;; TODO: non-generative fn!
                       (if-let [a (apply f action)]
                         (reacl/return :action a)
                         (reacl/return)))
                     thunk)))

#?(:clj
   (defmacro map-action [form f & args]
     `(map-action* (fn [] ~form)
                   ~f ~@args)))

#?(:cljs
   (defn handle-action* [thunk f & args]
     (let [this (impl/get-context)]
       (reduce-action* (fn [app-state action] ;; TODO: non-generative fn!
                         (if-let [msg (apply f action args)]
                           ;; TODO: use reacl/return :message when available.
                           (do (goog.async.nextTick (fn []
                                                      (reacl/send-message! this msg)))
                               (reacl/return))
                           (reacl/return :action action)))
                       thunk))))

#?(:clj
   (defmacro handle-action [form f & args] ;; TODO: maybe take 'this' as an arg? (then also don't need it in the context)
     `(handle-action* (fn [] ~form)
                      ~f ~args)))

#?(:cljs
   (defn bind-reaction* [reaction app-state thunk]
     (init-context-field :reaction [reaction app-state]
                         thunk)))

#?(:clj
   (defmacro bind-reaction [reaction app-state form]
     `(bind-reaction* ~reaction ~app-state
                      (fn [] ~form))))

#_(:clj
   ;; TODO?
   (defmacro pass-through [state & body]
     ...))

#?(:cljs
   (defn fixed* [value thunk]
     (bind* (state/->FixedState value lens/id)
            thunk)))

#?(:clj
   (defmacro fixed [value form]
     `(fixed* ~value
              (fn [] ~form))))

