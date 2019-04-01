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
  "[a b] => [[a b] nil], (list a b)
  [a b & c] => [[a b] [c]], (apply list a b c)
  [{:x x} b & [c]] => [[arg1# b] arg2#], (apply list arg1# b arg2#)
  "
  ;; Note [... :as all] is apparently only possible in lets, not in fn args :-( (Bug in clojure?)
  [params]
  (let [symbols_ (mapv (fn [x]
                        (cond
                          (symbol? x) x
                          :else `arg#))
                       params)
        [symbols vargs_] (split-with #(not= '& %) symbols_)
        vargs (rest vargs_)]
    (assert (<= (count vargs) 1))
    (if (empty? vargs)
      [[symbols nil] `[~@symbols]]
      [[symbols (first vargs)] `(apply list ~@(filter #(not= % '&) symbols_))])))

(defn reify-reacl-class [class]
  ;; delegates all methods to the internal reacl class
  `[reacl/IReaclClass
    (~'-instantiate-toplevel-internal [~'_ rst#]
                                      (reacl/-instantiate-toplevel-internal ~class rst#))
    (~'-compute-locals [~'_ app-state# args#]
                       (reacl/-compute-locals ~class app-state# args#))
    (~'-make-refs [~'_]
                  (reacl/-make-refs ~class))
    (~'-react-class [~'_]
                    (reacl/-react-class ~class))])

(defn- ifn-clause [f fixed-args args]
  `(~'-invoke [~'_ ~@args] (~f ~@fixed-args ~(vec args))))

(defn- ifn-vargs-clauses [f fixed-args args]
  ;;(assert (<= (count args) 21))
  (loop [args (vec args)
         res []]
    (if (>= (count args) 20)
      (-> res
          (conj (ifn-clause f fixed-args args))
          (conj (let [rest (gensym "rest")]
                  `(~'-invoke [~'_ ~@args ~rest] (~f ~@fixed-args (concat ~args ~rest))))))
      (recur (conj args (gensym (str "arg" (count args))))
             (conj res (ifn-clause f fixed-args args))))))

(defn create-ctx-class [name has-state? params class]
  (assert (vector? params) (str "not a vector: " (pr-str params)))
  (let [[[mparams vparam] args] (undestructuring params)
        opt (gensym "opt")
        app-state (when has-state? (gensym "app-state"))]
    `(reify
       ~@(reify-reacl-class class)
       ~'IFn
       ;; Note opt and app-state args don't actually have to be that, if class has varargs
       ~@(if (nil? vparam)
           (if has-state?
             [(ifn-clause `reacl-ext.context.runtime/instantiate-with-state [class] mparams)
              (ifn-clause `reacl-ext.context.runtime/instantiate-with-state [class] (cons app-state mparams))
              (ifn-clause `reacl-ext.context.runtime/instantiate-with-state [class] (cons opt (cons app-state mparams)))]
             [(ifn-clause `reacl-ext.context.runtime/instantiate-without-state [class] mparams)
              (ifn-clause `reacl-ext.context.runtime/instantiate-without-state [class] (cons opt mparams))])
           (if has-state?
             (ifn-vargs-clauses `reacl-ext.context.runtime/instantiate-with-state [class] (cons app-state mparams)) ;; mparams+1 minimally.
             (ifn-vargs-clauses `reacl-ext.context.runtime/instantiate-without-state [class] mparams))))))


