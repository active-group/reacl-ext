(ns reacl-ext.context
  #?(:cljs (:require [reacl2.core :as reacl]
                     [reacl-ext.context.impl :as impl]
                     [reacl-ext.context.state :as state]
                     [active.clojure.lens :as lens]
                     goog.async.nextTick)))

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

#?(:cljs
   (defn bind-state* [state-f thunk]
     (impl/update-context
      (fn [context]
        (assoc context :state (state-f context)))
      thunk)))

;; TODO: if app-state is the default, then could only be used to 'override' a previous local-state context; questionable if that is wise.
#?(:cljs
   (defn app-state*
     ([thunk]
      (app-state* lens/id thunk))
     ([lens thunk]
      (bind-state* (fn [context]
                     (let [v (:app-state context)]
                       (when (= state/not-available v)
                         ;; TODO: positive formulation; hint to do it right...
                         (throw (ex-info "Cannot embed into app-state, class does not have one." {})))
                       (state/->AppState (:component context) v active.clojure.lens/id)))
                   thunk))))

#?(:clj
   (defmacro app-state
     ([form] `(app-state* (fn [] ~form)))
     ([lens form] `(app-state* ~lens (fn [] ~form)))))

#?(:cljs
   (defn local-state*
     ([thunk]
      (local-state* lens/id thunk))
     ([lens thunk]
      (bind-state* (fn [context]
                     (let [v (:local-state context)]
                       (when (= state/not-available v)
                         ;; TODO: positive formulation; hint to do it right...
                         (throw (ex-info "Cannot embed into local-state, class does not have one." {})))
                       (state/->LocalState (:component context) v active.clojure.lens/id)))
                   thunk))))

#?(:clj
   (defmacro local-state
     ([form] `(local-state* (fn [] ~form)))
     ([lens form] `(local-state* ~lens (fn [] ~form)))))

;; TODO: mixed-state

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
                       (if-let [a (apply f action args)]
                         (reacl/return :action a)
                         (reacl/return)))
                     thunk)))

#?(:clj
   (defmacro map-action [form f & args]
     `(map-action* (fn [] ~form)
                   ~f ~@args)))

#?(:cljs
   (defn handle-action* [thunk f & args]
     (let [this (impl/get-component)]
       (reduce-action* (fn [app-state action] ;; TODO: non-generative fn!
                         (if-let [msg (apply f action args)]
                           ;; FIXME: this is hack; use reacl/return :message when available.
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
   (defn reaction* [reaction app-state thunk]
     (init-context-field :reaction [reaction app-state]
                         thunk)))

#?(:clj
   (defmacro reaction [reaction app-state form]
     `(reaction* ~reaction ~app-state
                 (fn [] ~form))))

#_(:clj
   ;; TODO?
   (defmacro pass-through [state & body]
     ...))

#?(:cljs
   (defn fixed* [value thunk]
     (bind-state* (constantly (state/->FixedState value lens/id))
                  thunk)))

#?(:clj
   (defmacro fixed [value form]
     `(fixed* ~value
              (fn [] ~form))))

