(ns reacl-ext.context.runtime
  (:require [reacl2.core :as reacl]
            [active.clojure.lens :as lens]
            [reacl-ext.context.state :as state]))

(def ^{:dynamic true :private true} *context*)

(defn ^:private initial-context [component app-state local-state]
  {:component component        ;; "read-only"
   :app-state app-state        ;; "read-only"
   :local-state local-state    ;; "read-only"
   :state (if (not= state/not-available app-state)
            (state/->AppState component app-state lens/id)
            (if (not= state/not-available local-state)
              (state/->LocalState component local-state lens/id)
              nil))
   :reaction nil
   :reduce-action nil})

(defn update-context [f thunk]
  (assert *context*)
  (binding [*context* (let [context (f *context*)]
                        ;; TODO: check some pre/post conditions here.
                        ;; remove state/reaction when the other is set.
                        context)]
    (thunk)))

(defn- get-instantiation-args-with-state []
  ;; checks there is a context suitable to instantiate classes with an app-state
  ;; returns [opt app-state]
  (assert *context*)
  (assert (or (:state *context*) (:reaction *context*)) *context*)
  (let [[reaction state]
        (if-let [state (:state *context*)]
          [(state/-reaction state) @state]
          ;; :reaction is actually a tuple already
          (:reaction *context*))]
    [(reacl/opt :reaction reaction
                :reduce-action (:reduce-action *context*))
     state]))

(defn- get-instantiation-args-without-state []
  ;; checks there is a context suitable to instantiate classes without an app-state
  ;; returns only an opt
  (assert *context*)
  ;; warn?? (assert (not (:reaction *context*)))
  (reacl/opt :reduce-action (:reduce-action *context*)))

(defn with-initial-context* [this app-state local-state thunk]
  (binding [*context* (initial-context this app-state local-state)]
    (thunk)))

(defn ^:no-doc instantiate-with-state [class args]
  (if-not *context*
    (apply class args) ;; 'compatibility mode'
    (let [[opt state] (get-instantiation-args-with-state)]
      (apply class opt state args))))

(defn ^:no-doc instantiate-without-state [class args]
  (if-not *context*
    (apply class args) ;; 'compatibility mode'
    (let [opt? (get-instantiation-args-without-state)]
      (apply class (if opt? (cons opt? args))))))
