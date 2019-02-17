(ns reacl-ext.extensions.consts
  (:require [reacl-ext.extensions.core :as core]))

(defn- add-consts [local-state-decl bindings this]
  (-> (or local-state-decl `[name# nil]) ;; add an anonymous local-state if there is none
      (update 1 (fn [init]
                  `(do (core/set-extension-data! ~this ::consts
                                                 ~(into {}
                                                        (map (fn [[name expr]]
                                                               `['~name ~expr])
                                                             bindings)))
                       ~init)))))

(defn ^:no-doc let-consts [form names this]
  (let [values `values#]
    `(let [~(into {} (map (fn [n] [n `'~n]) ;; could optimize this a little by not using a map (but array or obj)
                          names))
           (core/get-extension-data ~this ::consts)]
       ~form)))

(defmethod core/class-extension 'consts
  [_ form specs name this app-state? params]
  ;; 1. Hook into the local-state spec, to realize a one-time initialization
  ;; 2. That stores a map in an extra field of the component (this)
  ;; 3. Wrap the rendering form with a 'let' with all names from the map bound to the constant values.
  ;; Note: unlike locals, these are never re-evaluated during the livetime of the component, and, consquently, can only see 'this'.

  ;; TOOD: exceptions: also when names are not symbols?  (use spec for that?)
  (when-not (vector? form))
  (when-not (even? (count form)))
    
  (let [bindings (apply array-map form)]
    (-> specs
        (update 'local-state
                add-consts bindings this)
        (update 'render
                let-consts (keys bindings) this))))
