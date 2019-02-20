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

#_(:cljs
   (defn bind-app-state*
     ([thunk]
      (app-state* lens/id thunk))
     ([lens thunk]
      (bind-state* (fn [context]
                     ;; must use this - with reacl internals one might
                     ;; to be able to get the current app-state of a
                     ;; different then the 'current' component, but
                     ;; then that is doubtfully sane.
                     ;; Here, it serves some 'code quality' purpose
                     ;; that we require the component to be passed.
                     (assert (= component (:component context)))
                     (let [v (:app-state context)]
                       (when (= state/not-available v)
                         ;; TODO: positive formulation; hint to do it right...
                         (throw (ex-info "Cannot embed into app-state, class does not have one." {})))
                       (state/->AppState (:component context) v active.clojure.lens/id)))
                   thunk))))

#_(:clj
   (defmacro bind-app-state
     ([component form] `(bind-app-state* ~component (fn [] ~form)))
     ([component lens form] `(bind-app-state* ~component ~lens (fn [] ~form)))))

#?(:cljs
   (defn locally*
     ([component thunk]
      (locally* component lens/id thunk))
     ([component lens thunk]
      (bind-state* (fn [context]
                     ;; must use this - with reacl internals one might
                     ;; to be able to get the current local-state of a
                     ;; different then the 'current' component, but
                     ;; then that is doubtfully sane.
                     ;; Here, it serves some 'code quality' purpose
                     ;; that we require the component to be passed.
                     (assert (= component (:component context)))
                     (let [v (:local-state context)]
                       (when (= state/not-available v)
                         ;; TODO: positive formulation; hint to do it right...
                         (throw (ex-info "Cannot embed into local-state, class does not have one." {})))
                       (state/->LocalState (:component context) v active.clojure.lens/id)))
                   thunk))))

#?(:clj
   (defmacro locally
     ([component form] `(locally* ~component (fn [] ~form)))
     ([component lens form] `(locally* ~component ~lens (fn [] ~form)))))

;; TODO: mixed-state

#?(:cljs
   (defn focus* [lens thunk]
     (update-context-field :state #(state/-focus % lens)
                           thunk)))

#?(:clj
   (defmacro focus [lens form]
     `(focus* ~lens (fn [] ~form))))

#?(:cljs
   (defn- compose-reduce-action [outer inner-f inner-args]
     (assert (some? inner-f))
     (if (nil? outer)
       (fn [app-state action]  ;; TODO: non-generative fn!
         (apply inner-f app-state action inner-args))
       
       ;; https://github.com/active-group/reacl/issues/9
       ;; (:args ret)
       ;; TODO: unless the outer returns app-state it is possible:
       (throw (ex-info "Reducing actions cannot be composed yet." {})))))

#?(:cljs
   (defn reduce-action* [thunk f & args]
     (update-context-field :reduce-action #(compose-reduce-action % f args)
                           thunk)))

#?(:clj
   (defmacro reduce-action
     "Use with care. Prefer to use [[map-action]] or [[handle-action]]."
     [form f & args]
     `(reduce-action* (fn [] ~form) ~f ~@args)))

#?(:cljs
   (letfn [(action-mapper [app-state action f args]
             (let [a (apply f action args)]
               (if (some? a)
                 (reacl/return :action a)
                 (reacl/return))))]
     (defn map-action* [thunk f & args]
       (reduce-action* thunk
                       action-mapper f args))))

#?(:clj
   (defmacro map-action [form f & args]
     `(map-action* (fn [] ~form)
                   ~f ~@args)))

#?(:cljs
   (letfn [(action-handler [app-state action component f args]
             (let [msg (apply f action args)]
               ;; FIXME: this is hack; use reacl/return :message when available.
               (if (some? msg)
                 (do (goog.async.nextTick (fn []
                                            (reacl/send-message! component msg)))
                     (reacl/return))
                 (reacl/return :action action))))]
     (defn handle-action* [thunk component f & args]
       (reduce-action* thunk
                       action-handler component f args))))

#?(:clj
   (defmacro handle-action [form component f & args]
     `(handle-action* (fn [] ~form)
                      ~component ~f ~@args)))

#?(:cljs
   (defn reaction* [reaction app-state thunk]
     (init-context-field :reaction [reaction app-state]
                         thunk)))

#?(:clj
   (defmacro reaction
     "Use with care. Prefer to use [[app-state]] or [[local-state]] and maybe [[focus]]."
     [reaction app-state form]
     `(reaction* ~reaction ~app-state
                 (fn [] ~form))))

#_(:clj
   ;; TODO?
   #_(defmacro pass-through [state & body]
     ...))

#?(:cljs
   (defn fixed* [value thunk]
     (bind-state* (constantly (state/->FixedState value lens/id))
                  thunk)))

#?(:clj
   (defmacro fixed [value form]
     `(fixed* ~value
              (fn [] ~form))))

