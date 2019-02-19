(ns reacl-ext.core
  (:require [reacl-ext.impl :as impl]
            [reacl2.core :as reacl]))

#?(:clj
   (defmacro defclass [name this app-state? docstring? params & specs]
     ;; TODO: move docstring before this.
     (apply impl/translate-defclass name this app-state? docstring? params specs)))

#?(:cljs
   (defn reacl-class [ext-class]
     (impl/reacl-class ext-class)))

;; #?(:clj
;;    (defmacro defc [name params & body]
;;      ;; ...needs a variant with an app-state decl?
;;      `(defclass ~name this# ~params
;;         ~'render
;;         (do ~@body))))
 

#?(:cljs
 (defn return [& items]
   ;; Allows action to be nil, meaning no action.
   (apply reacl/return (apply concat (let [m (apply array-map items)]
                                       (if (nil? (:action m))
                                         (dissoc m :action)
                                         m))))))

#?(:cljs
   (defn render-component [element clazz & rst]
     (apply reacl/render-component element
            (reacl-class clazz)
            rst)))

