(ns reacl-ext.impl
  (:require [reacl-ext.extensions.core :as ext]
            [reacl-ext.context.translation :as ctx-tr]
            [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as s-clj]))

(defn generate-reacl-class
  "Adds:
  - namespaced class names (https://github.com/active-group/reacl/issues/16)
  "
  [name this app-state? params specs-map]
  (assert (map? specs-map) (str "specs must be a map: " (pr-str specs-map)))
  `(reacl2.core/class ~(if (symbol? name) (str *ns* "/" (clojure.core/name name)) name)
                      ~this
                      ~@(if app-state? (list app-state?) nil)
                      ~params
                      ~@(mapcat identity specs-map)))


(s/def ::class-args
  (s/cat :name symbol?
         :this simple-symbol?
         :app-state? (s/? simple-symbol?)
         ;; TODO:? :params :clojure.core.specs.alpha/param-list   (already splits into :params & :var-params)
         :params (s/coll-of simple-symbol?)
         :specs (s/* any?)))

#_(s/def ::defclass-args
  (s/cat :name simple-symbol?
         :docstring? (s/? string?)
         ;; TODO: how to reuse ::class-args ?
         :this simple-symbol?
         :app-state? (s/? simple-symbol?)
         ;; TODO:? :params :clojure.core.specs.alpha/param-list   (already splits into :params & :var-params)
         :params (s/coll-of simple-symbol?)
         :specs (s/* any?)))

(defn- analyze-args [spec & args]
  (let [m (s/conform spec args)]
    (if (s/invalid? m)
      (throw (ex-info (s/explain-str spec args) (s/explain-data spec args)))
      m)))

(defn generate-base-class
  [name this app-state? params & specs]
  (let [{:keys [name this app-state? params specs]} (apply analyze-args ::class-args name this app-state? params specs)]
    (assert (even? (count specs)) specs)
    (let [specs-map (apply array-map specs)] ;; keep order (because of shadowing)
      (assert (every? symbol? (keys specs-map)) (remove symbol? (keys specs-map)))
      
      (let [specs-map (-> specs-map
                          (ctx-tr/apply-context name this app-state?)
                          (ext/apply-class-extensions name this app-state? params))]
        [(some? app-state?) params (generate-reacl-class name this app-state? params specs-map)]))))

(defn translate-class* [name this app-state? params & specs]
  (let [clazz (gensym "class")
        [has-app-state? params class] (apply generate-base-class name this app-state? params specs)]
    [params `(let [~clazz ~class]
               ~(ctx-tr/create-ctx-class name has-app-state? params clazz))]))

(defn translate-class [name this app-state? params & specs]
  (let [[_ class] (apply translate-class* name this app-state? params specs)]
    class))

(defn translate-defclass [name docstring? this app-state? params & specs]
  (let [[docstring? cargs] (if (string? docstring?)
                             [docstring? (apply list name this app-state? params specs)]
                             [nil (apply list name docstring? this app-state? params specs)])
        [params class] (apply translate-class* cargs)]
    `(def ~(cond-> (vary-meta name assoc
                              :arglists '(list params))
             docstring? (vary-meta assoc :doc docstring?))
       ~class)))
