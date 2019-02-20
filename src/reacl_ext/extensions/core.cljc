(ns reacl-ext.extensions.core)

(defmulti class-extension
  "Extensible multimethod for the translation of defclass specs, a map from symbol to a form."
  (fn [tag form specs name this app-state? params] tag))

(def reacl-tags #{'render 'handle-message 'local-state
                  'component-will-mount
                  'component-did-mount
                  'component-will-receive-args
                  'should-component-update?
                  'component-will-update
                  'component-did-update
                  'component-will-unmount})

(defmethod class-extension :default [tag form specs name this app-state? params]
  (if (contains? reacl-tags tag)
    (assoc specs tag form)
    (throw (ex-info (str "Unknown tag used in class specification: " tag) {:name name
                                                                           :tag tag}))))

(defn- apply-one-class-extension [specs-map name this app-state? params]
  ;; only the first class-extension that changes something is applied, then this returns.
  (loop [specs-1 (seq specs-map)]
    (if (empty? specs-1)
      specs-map
      (let [[tag form] (first specs-1)
            m (class-extension tag form (dissoc specs-map tag) name this app-state? params)]
        (if (= m specs-map)
          (recur (rest specs-1))
          m)))))

(defn apply-class-extensions [specs-map name this app-state? params]
  ;; Note: recurse, to see changes in 'render from one extension when translating anther extension, e.g.
  ;; If might allow one extension to use the other, but that's hard to implement in the ext. method.
  (loop [specs-map specs-map]
    (let [m (apply-one-class-extension specs-map name this app-state? params)]
      (if (= m specs-map)
        m
        (recur m)))))

;; many extensions may want to 'smuggle' some extra data in the component.

#?(:cljs
   ;; virtual type, for easy advanced compilation.
   (deftype ^:no-doc ExtComp [reacl_ext_extensions_data]))

#?(:cljs
   (defn get-extension-data [component tag]
     (get (.-reacl_ext_extensions_data component) tag)))

#?(:cljs
   (defn set-extension-data! [component tag value]
     (set! (.-reacl_ext_extensions_data component)
           (assoc (.-reacl_ext_extensions_data component) tag value))))

#?(:cljs
   (defn update-extension-data! [component tag f & args]
     (set-extension-data! component tag
                          (apply f (get-extension-data component tag) args))))
