(ns reacl-ext.extensions.core)

(defmulti class-extension
  "Extensible multimethod for the translation of defclass specs, a map from symbol to a form."
  (fn [tag form specs name this app-state? params] tag))

(defmethod class-extension :default [tag form specs name this app-state? params]
  (throw (ex-info (str "Undefined tag in class specification: " tag) {:name name
                                                                      :tag tag})))


(defn apply-class-extensions [specs-map name this app-state? params]
  ;; TODO: may one extension use another? then repeat until nothing changes?
  (reduce (fn [specs-map [tag form]]
            (class-extension tag form (dissoc specs-map tag) name this app-state? params))
          specs-map
          specs-map))

;; many extensions may want to 'smuggle' some extra data in the component.

#?(:cljs
   ;; virtual type, for easy advanced compilation.
   (deftype ^:no-doc ExtComp [reacl_ext_extensions_data]))

#?(:cljs
   (defn get-extension-data [component tag]
     (.-reacl_ext_extensions_data component)))

#?(:cljs
   (defn set-extension-data! [component tag value]
     (set! (.-reacl_ext_extensions_data component)
           (assoc (.-reacl_ext_extensions_data component) tag value))))

#?(:cljs
   (defn update-extension-data! [component tag f & args]
     (set-extension-data! component tag
                          (apply f (get-extension-data component tag) args))))
