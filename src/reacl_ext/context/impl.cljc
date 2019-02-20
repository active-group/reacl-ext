(ns reacl-ext.context.impl
  #?(:cljs (:require [reacl2.core :as reacl]
                     [active.clojure.lens :as lens]
                     [reacl-ext.context.state :as state])))

#?(:cljs
   (def ^{:dynamic true :private true} *context*))

#?(:cljs
   (defn ^:private initial-context [component app-state local-state]
     {:component component  ;; "read-only"
      :app-state app-state  ;; "read-only"
      :local-state local-state  ;; "read-only"
      :state (if (not= state/not-available app-state)
               (state/->AppState component app-state lens/id)
               (if (not= state/not-available local-state)
                 (state/->LocalState component local-state lens/id)
                 nil))
      :reaction nil
      :reduce-action nil}))

#?(:cljs
   (defn update-context [f thunk]
     (assert *context*)
     (binding [*context* (let [context (f *context*)]
                           ;; TODO: check some pre/post conditions here.
                           ;; remove state/reaction when the other is set.
                           context)]
       (thunk))))

#?(:cljs
   (defn- get-instantiation-args-with-state []
     ;; checks there is a context suitable to instantiate classes with an app-state
     ;; returns [opt app-state]
     (assert *context*)
     (assert (or (:state *context*) (:reaction *context*)) *context*)
     (let [[reaction state]
           (if-let [state (:state *context*)]
             [(state/-reaction state) @state]
             ;; :reaction is actually a tuple already
             (:reaction *context*))]
       [(reacl/opt :reaction reaction
                   :reduce-action (:reduce-action *context*))
        state])))

#?(:cljs
   (defn- get-instantiation-args-without-state []
     ;; checks there is a context suitable to instantiate classes without an app-state
     ;; returns only an opt
     (assert *context*)
     ;; warn?? (assert (not (:reaction *context*)))
     (reacl/opt :reduce-action (:reduce-action *context*))))

#?(:cljs
   (defn wrap-initial-context* [this app-state local-state thunk]
     (binding [*context* (initial-context this app-state local-state)]
       (thunk))))

(defn- wrap-initial-context [render this app-state? local-state?]
  `(wrap-initial-context* ~this
                          ~(if app-state? app-state? 'reacl-ext.context.state/not-available)
                          ~(if local-state? local-state? 'reacl-ext.context.state/not-available) ;; TODO: via methods/consts, localstate is always set. So always allow it?
                          (fn [] ~render)))

(defn- wrap-context [render this app-state? local-state?]
  (-> render
      (wrap-initial-context this app-state? local-state?)))

(defn- get-local-state [specs-map]
  (first (get specs-map 'local-state)))

(defn- wrap-handle-state-messages [handler app-state? local-state?]
  ;; app-state and local-state can both be 'nil'/no symbol here.
  (if (or app-state? local-state?)
    ;; Note: using (gensym) for easier testing here
    (let [msg (gensym "msg")
          other (gensym "other")]
      `(let [~other ~(if handler handler 'reacl-ext.context.state/fallback-handle-message)]
         (fn [~msg]
           (reacl-ext.context.state/handle-state-messages ~msg ~other
                                                          ~(or app-state? 'reacl-ext.context.state/not-available)
                                                          ~(or local-state? 'reacl-ext.context.state/not-available)))))
    handler))

(defn apply-context [specs-map name this app-state?]
  (assert (contains? specs-map 'render) specs-map) ;; ...
  (-> specs-map
      (update 'render wrap-context this app-state? (get-local-state specs-map))
      (update 'handle-message wrap-handle-state-messages app-state? (get-local-state specs-map))))

#?(:cljs
   (defn ^:no-doc instantiate-with-state [class args]
     (let [[opt state] (get-instantiation-args-with-state)]
       (apply class opt state args))))

#?(:cljs
   (defn ^:no-doc instantiate-without-state [class args]
     (let [opt (get-instantiation-args-without-state)]
       (apply class opt args))))

(defn- undestructuring
  "[a b] => [a b], (list a b)
  [a b & c] => [a b & c], (apply list a b c)
  [{:x x} b & [c]] => [a# b & c#], (apply list a# b c#)
  "
  ;; Note [... :as all] is apparently only possible in lets, not in fn args :-( (Bug in clojure?)
  [params]
  (let [symbols (mapv (fn [x]
                        (cond
                          (symbol? x) x
                          :else `arg#))
                      params)
        varg? (contains? (set symbols) '&)]
    (if varg?
      [symbols `(apply list ~@(filter #(not= % '&) symbols))]
      [symbols `[~@symbols]])))

(defn instantiator-fn [has-state? params class]
  (assert (vector? params) params)
  (let [[mparams args] (undestructuring params)]
    `(-> (fn ~mparams
           ~(if has-state?
              `(instantiate-with-state ~class ~args)
              `(instantiate-without-state ~class ~args)))
         (with-meta {::reacl-class ~class}))))

(defn instantiator-defn [name docstring? has-state? params class]
  `(def ~(cond-> (vary-meta name assoc
                            :arglists '(list params))
           docstring? (vary-meta assoc :doc docstring?))
     ~(instantiator-fn has-state? params class)))

(defn reacl-class [instantiator-fn]
  (::reacl-class (meta instantiator-fn)))
