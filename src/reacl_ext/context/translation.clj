(ns reacl-ext.context.translation)

(defn- wrap-initial-context [render this app-state? local-state?]
  `(reacl-ext.context.runtime/with-initial-context*
     ~this
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
              `(reacl-ext.context.runtime/instantiate-with-state ~class ~args)
              `(reacl-ext.context.runtime/instantiate-without-state ~class ~args)))
         (reacl-ext.context.runtime/set-reacl-class ~class))))

(defn instantiator-defn [name docstring? has-state? params class]
  `(def ~(cond-> (vary-meta name assoc
                            :arglists '(list params))
           docstring? (vary-meta assoc :doc docstring?))
     ~(instantiator-fn has-state? params class)))
