(ns reacl-ext.extensions.methods
  (:require [reacl-ext.extensions.core :as core]
            reacl-ext.extensions.consts))

(comment
  ;; this would be a great syntax too:
  wrap
  (let [...]

    render (dom/div))

  )

(defmethod core/class-extension 'methods
  [_ form specs name this app-state? params]
  ;; methods are consts in letfn syntax.
  (-> specs
      (update 'consts (fn [consts]
                        (vec (concat consts
                                     (map (fn [method-form]
                                            ;; (name [args] body)
                                            ;; TODO: exception. Use spec?
                                            (when-not (list? method-form))
                                            (when-not (>= (count method-form) 2))
                                            (when-not (vector? (second method-form)))
                                            [(first method-form) `(fn ~(second method-form)
                                                                    ~@(rest (rest method-form)))]))))))))

