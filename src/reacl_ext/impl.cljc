(ns reacl-ext.impl
  (:require [reacl-ext.extensions.core :as ext]
            [reacl-ext.context.impl :as ctx-impl]
            #?(:clj reacl-ext.extensions.consts)
            #?(:clj reacl-ext.extensions.methods)))

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
                ~@(mapcat identity specs-map)))

(defn analyze-class-args [app-state? docstring? params? specs?]
  (let [[app-state docstring? params? specs?]
        (if (symbol? app-state?)
          [app-state? docstring? params? specs?]
          [nil app-state? docstring? (cons params? specs?)])
        [docstring params specs]
        (if (string? docstring?)
          [docstring? params? specs?]
          [nil docstring? (cons params? specs?)])]
    (when-not (vector? params) ;; use clojure.spec?
      #_(throw (ex-info ""))
      )
    (when-not (even? (count specs))
      )
    ;; Note: keeps the order of specs via array-map:
    [app-state docstring params (apply array-map specs)]))

(defn generate-ext-class
  "Adds:
  - optional docstring just before the binding form for the arguments.
  "
  [name this app-state? docstring? params & specs]
  (let [[app-state? docstring? params specs-map] (analyze-class-args app-state? docstring? params specs)]
    (assert (map? specs-map) "specs must be a map")
    (let [specs-map (-> specs-map
                        (ctx-impl/apply-context name this app-state?)
                        (ext/apply-class-extensions name this app-state? params))
          class (generate-base-class name this app-state? params specs-map)]
      [docstring? (some? app-state?) params class])))

(defn translate-defclass [name this app-state? docstring? params & specs]
  (let [[docstring? has-app-state? params class] (apply generate-ext-class name this app-state? docstring? params specs)
        clazz (gensym "class")]
    `(let [~clazz ~class]
       ~(ctx-impl/instantiator-defn name docstring? has-app-state? params clazz))))

(defn reacl-class [ext-class]
  (ctx-impl/reacl-class ext-class))
