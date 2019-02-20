(ns reacl-ext.impl
  (:require [reacl-ext.extensions.core :as ext]
            #?(:cljs [reacl-ext.context.runtime :as ctx-rt])
            #?(:cljs [reacl2.core :as reacl])
            #?(:clj [reacl-ext.context.translation :as ctx-tr])
            #?(:clj reacl-ext.extensions.consts)
            #?(:clj reacl-ext.extensions.methods)))

#?(:clj
   (defn generate-base-class
     "Adds:
  - namespaced class names (https://github.com/active-group/reacl/issues/16)
  "
     [name this app-state? params specs-map]
     (assert (map? specs-map) (str "specs must be a map: " (pr-str specs-map)))
     `(reacl/class ~(str *ns* "/" (clojure.core/name name))
                   ~this
                   ~@(if app-state? (list app-state?) nil)
                   ~params
                   ~@(mapcat identity specs-map))))

#?(:clj
   (defn analyze-class-args [docstring? this? app-state? params? specs?]
     ;; TODO: use clojure.spec?
     (let [[docstring this app-state? params? specs?]
           (if (string? docstring?)
             [docstring? this? app-state? params? specs?]
             [nil docstring? this? app-state? (cons params? specs?)])
           
           [app-state params specs]
           (if (symbol? app-state?)
             [app-state? params? specs?]
             [nil app-state? (cons params? specs?)])
           ]
       ;; TODO: throw.
       (assert (or (nil? docstring) (string? docstring)) docstring)
       (assert (symbol? this))
       (assert (or (nil? app-state) (symbol app-state)) app-state)
       (assert (vector? params) params)

       ;; Note: keeps the order of specs via array-map:
       [this docstring app-state params specs])))

#?(:clj
   (defn generate-ext-class
     "Adds:
  - optional docstring just before the binding form for the arguments.
  "
     [name docstring? this? app-state? params & specs]
     (let [[this docstring? app-state? params specs] (analyze-class-args docstring? this? app-state? params specs)]
       (assert (even? (count specs)) specs)
       (let [specs-map (apply array-map specs)] ;; keep order (because of shadowing)
         (assert (every? symbol? (keys specs-map)) (remove symbol? (keys specs-map)))
         (let [specs-map (-> specs-map
                             (ctx-tr/apply-context name this app-state?)
                             (ext/apply-class-extensions name this app-state? params))
               class (generate-base-class name this app-state? params specs-map)]
           [docstring? (some? app-state?) params class])))))

#?(:clj
   (defn translate-defclass [name docstring? this? app-state? params & specs]
     (let [[docstring? has-app-state? params class] (apply generate-ext-class name docstring? this? app-state? params specs)
           clazz (gensym "class")]
       `(let [~clazz ~class]
          ~(ctx-tr/instantiator-defn name docstring? has-app-state? params clazz)))))

#?(:cljs
   (defn reacl-class [ext-class]
     (ctx-rt/reacl-class ext-class)))
