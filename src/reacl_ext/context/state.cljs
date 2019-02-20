(ns reacl-ext.context.state
  (:require [active.clojure.lens :as lens]
            [reacl2.core :as reacl]))

(defprotocol IState
  (-focus [this lens] "Focus this state, by appending a lens.")
  (-reaction [this] "Returns the reacl/reaction, needed to create a class instance focused on this state."))

(defprotocol IUpdateableState
  (-owner [this] "Return a component")
  (-handle-update [this app-state local-state value] "Returns a reacl/return"))

(defrecord SetStateMessage [new-app-state state])

(def not-available #js {})

(defn handle-state-messages [msg handler app-state local-state]
  ;; states may be not-available (to distinguish from nil values)
  (if (instance? SetStateMessage msg)
      (-handle-update (:state msg) app-state local-state (:new-app-state msg))
      (handler msg)))

(defn fallback-handle-message [msg]
  ;; TODO throw? ignore?
  (assert false))

(defrecord AppState [owner value lens]
  IState
  (-focus [this l] (assoc this :lens (lens/>> lens l)))
  (-reaction [this] (reacl/reaction owner ->SetStateMessage this))
  IUpdateableState
  (-owner [_] owner)
  (-handle-update [_ app-state local-state value]
    (assert (not= app-state not-available) "app-state change sent to class without an app-state")
    (reacl/return :app-state (lens/shove app-state lens value)))
  IDeref
  (-deref [_] (lens/yank value lens)))

(defrecord LocalState [owner value lens]
  IState
  (-focus [this l] (assoc this :lens (lens/>> lens l)))
  (-reaction [this] (reacl/reaction owner ->SetStateMessage this))
  IUpdateableState
  (-owner [_] owner)
  (-handle-update [_ app-state local-state value]
    (assert (not= local-state not-available) "app-state change sent to class without a local-state")
    (reacl/return :local-state (lens/shove local-state lens value)))
  IDeref
  (-deref [_] (lens/yank value lens)))

(defrecord MixedState [owner app-state local-state lens]
  IState
  (-focus [this l] (assoc this :lens (lens/>> lens l)))
  (-reaction [this] (reacl/reaction owner ->SetStateMessage this))
  IUpdateableState
  (-owner [_] owner)
  (-handle-update [_ app-state local-state value]
    (assert (not= app-state not-available) "mixed state change sent to class without an app-state")
    (assert (not= local-state not-available) "mixed state change sent to class without a local-state")
    (let [[ast lst] (lens/shove [app-state local-state] lens value)]
      (reacl/return :local-state lst
                    :app-state ast)))
  IDeref
  (-deref [_] (lens/yank [app-state local-state] lens)))

(defrecord FixedState [value lens]
  IState
  (-focus [this l] (assoc this :lens (lens/>> lens l)))
  (-reaction [_] reacl/no-reaction))
