(ns reacl-ext.core
  (:require [reacl-ext.impl :as impl]))

#?(:clj
   (defmacro defclass [name this app-state? docstring? params & specs]
     (apply impl/translate-defclass name this app-state? docstring? params specs)))

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

