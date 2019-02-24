(ns reacl-ext.core
  (:require #?(:clj [reacl-ext.impl :as impl])
            #?(:cljs [reacl-ext.context.runtime :as ctx-rt])
            #?(:cljs [reacl2.core :as reacl :include-macros true])
            #?(:cljs reacl-ext.extensions.core)
            #?(:clj reacl-ext.extensions.consts)
            #?(:clj reacl-ext.extensions.methods)))

#?(:clj
   (defmacro defclass
     "Like reacl/defclass, but with the following additions:
  - optional docstring after the `name`
  - namespaced React 'displayName's.
  - 'consts' and 'method' specifications.
  - implicit instantiation contexts.
"
     [name docstring? this app-state? params & specs]
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


(defn- map-flat-tuple-list
  "Maps over a list by calling f on each tuple of even-indexed and the
  next value. f should return a tuple or nil, which are flattened then again."
  [f l]
  (assert (even? (count l)))
  (apply concat
         (reduce (fn [vec tup]
                   (conj vec (f tup)))
                 []
                 (let [prev (vec l)]
                   (map (fn [i]
                          [(get prev i) (get prev (inc i))])
                        (map #(* 2 %)
                             (range (/ (count prev) 2))))))))

#?(:cljs
   (defn return [& items]
     ;; https://github.com/active-group/reacl/issues/9
     ;; Allows action to be nil, meaning no action.
     (apply reacl/return
            (map-flat-tuple-list (fn [[k v]]
                                   (when-not (and (= :action k) (nil? v))
                                     [k v]))
                                 items))))

#?(:cljs
   (defn update-return
     "Returns a `reacl/return` value, where for each `field` (`:app-state`, `:action` etc), the value is piped through `f`."
     [ret field f & args]
     (assert (instance? reacl/Effects ret))
     ;; Note: ext/return also removes nil actions
     (apply return (map-flat-tuple-list (fn [[k v]]
                                          (if (= k field)
                                            [k (apply f v args)]
                                            [k v]))
                                        ;; TODO: need an exported access; not use internals.
                                        (:args ret)))))
