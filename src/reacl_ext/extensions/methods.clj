(ns reacl-ext.extensions.methods
  (:require [reacl-ext.extensions.core :as core]
            [reacl-ext.extensions.consts :as consts]))

(defmethod core/class-extension 'methods
  [_ form specs name this app-state? params]
  ;; methods are consts in letfn syntax.
  (consts/define-consts specs ::methods this
    (vec (mapcat (fn [method-form]
                   ;; (name [args] body)
                   ;; TODO: exceptions. Use spec?
                   (assert (list? method-form))
                   (assert (>= (count method-form) 2))
                   (assert (vector? (second method-form)))
                   [(first method-form) `(fn ~(second method-form)
                                           ~@(rest (rest method-form)))])
                 form))))

