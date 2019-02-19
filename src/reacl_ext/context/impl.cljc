(ns reacl-ext.context.impl
  (:require [reacl-ext.context :as ctx]
            #?(:cljs [reacl2.core :as reacl])
            #?(:cljs [active.clojure.lens :as lens])
            #?(:cljs [reacl-ext.context.state :as state])))

#?(:cljs
   (def ^{:dynamic true :private true} *context*))

#?(:cljs
   (defn ^:private initial-context [component app-state local-state]
     {:component component  ;; "read-only"
      :app-state app-state  ;; "read-only"
      :local-state local-state  ;; "read-only"
      :state (if (not= state/not-available app-state)
               (state/AppState component app-state lens/id)
               (if (not= state/not-available local-state)
                 (state/LocalState component local-state lens/id)
                 nil))
      :reaction nil
      :reduce-action nil}))

#?(:cljs
   (defn get-component []
     (assert *context*)
     (:component *context*)))

#?(:cljs
   (defn update-context [f thunk]
     (assert *context*)
     (binding [*context* (let [context (f *context)]
                           ;; TODO: check some pre/post conditions here.
                           ;; remove state/reaction when the other is set.
                           context)]
       (thunk))))

#?(:cljs
   (defn- get-instantiation-args-with-state []
     ;; checks there is a context suitable to instantiate classes with an app-state
     ;; returns [opt app-state]
     (assert *context*)
     (assert (or (:state *context*) (:reaction *context*)))
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
     (assert (not (or (:state *context*) (:reaction *context*))))
     (reacl/opt :reduce-action (:reduce-action *context*))))


(defn- wrap-initial-context [render this app-state? local-state?]
  `(binding [*context* (initial-context ~this
                                        ~(if app-state? app-state? 'reacl-ext.context.state/not-available)
                                        ~(if local-state? local-state? 'reacl-ext.context.state/not-available))]
     ~render))

(defn- wrap-context [render this app-state? local-state?]
  (-> render
      (wrap-initial-context this app-state? local-state?)))

(defn- get-local-state [specs-map]
  (first (get specs-map 'local-state)))

(defn- wrap-handle-state-messages [handler app-state? local-state?]
  ;; app-state and local-state can both be 'nil'/no symbol here.
  (if (or app-state? local-state?)
    `(reacl-ext.context.state/wrap-handle-state-messages ~handler
                                                         ~(or app-state? 'reacl-ext.context.state/not-available)
                                                         ~(or local-state? 'reacl-ext.context.state/not-available))
    handler))

(defn apply-context [specs-map name this app-state?]
  (when-not (contains? specs-map 'render)) ;; ...
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
  (let [[mparams args] (undestructuring params)]
    `(-> (fn ~mparams
           ~(if has-state?
              `(instantiate-with-state ~class ~args)
              `(instantiate-without-state ~class ~args)))
         (with-meta {::reacl-class ~class}))))

(defn instantiator-defn [name docstring? has-state? params class]
  (let [n `(vary-meta ~name assoc
                      ~@(when docstring? [:doc docstring?])
                      :arglists '(~params))]
    `(def ~n
       ~(instantiator-fn has-state? params class))))

(defn reacl-class [instantiator-fn]
  (::reacl-class (meta instantiator-fn)))
