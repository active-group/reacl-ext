(ns reacl-ext.core
  (:require #?(:clj [reacl-ext.impl :as impl])
            #?(:cljs [reacl-ext.context.runtime :as ctx-rt])
            #?(:cljs [reacl2.core :as reacl :include-macros true])
            #?(:cljs reacl-ext.extensions.core)
            #?(:clj reacl-ext.extensions.consts)
            #?(:clj reacl-ext.extensions.methods)))

#?(:clj
   (defmacro defclass [name docstring? this app-state? params & specs]
     (apply impl/translate-defclass name docstring? this app-state? params specs)))

#?(:clj
   (defmacro class [name this app-state? params & specs]
     (apply impl/translate-class name this app-state? params specs)))

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
