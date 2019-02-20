(ns examples.todo.core
  (:require [reacl-ext.core :as ext :include-macros true]
            [reacl-ext.context :as ctx :include-macros true]
            [active.clojure.lens :as lens]
            [reacl2.core :as reacl]
            [reacl2.dom :as dom]))

(enable-console-print!)

(ext/defclass checkbox
  ;; "Control for a boolean value in the form of a checkbox"
  this checked []

  methods [(change [e] (reacl/send-message! this
                                            (.. e -target -checked)))]
  render
  (dom/input {:type "checkbox"
              :value checked
              :onchange change})
  
  handle-message
  (fn [checked]
    (reacl/return :app-state checked)))

(ext/defclass text-input this text []
  methods [(change [e]
                   (reacl/send-message! this
                                        (.. e -target -value)))]
  render
  (dom/input {:onchange change
              :value text})
  
  handle-message
  (fn [txt]
    (reacl/return :app-state txt)))

;;   "Control that shows a button and emits an action (true) when that is clicked."

(ext/defclass button
  this [label]
  
  methods [(click [_]
                  (reacl/send-message! this :clicked))]
  
  render
  (dom/button {:onclick click}
              label)
  
  handle-message
  (fn [_]
    (reacl/return :action true)))

(defrecord TodosApp [next-id todos])

(defrecord Todo [id text done?])

(defrecord Delete [todo])

(defrecord Submit [_])


(ext/defclass to-do-item this todo []
  render
  (dom/div (ctx/focus :done? (checkbox))
           (ctx/map-action (button "Zap")
                           ;; TODO: non-generative fn
                           (constantly (->Delete todo)))
           
           (:text todo)))

(letfn [(todo-id-yank [list id]
          (first (filter #(= id (:id %)) list)))
        (todo-id-shove [list v id]
          (map (fn [todo]
                 (if (= id (:id todo))
                   v
                   todo))
               list))]
  (defn todo-id
    "A lens over the todo item with id in a list of todos (must exist)."
    [id]
    (lens/lens todo-id-yank
               todo-id-shove
               id)))

(ext/defclass to-do-app this app-state []
  local-state [next-text ""]

  render
  (dom/div
   (dom/h3 "TODO")
   (dom/div 
    (map (fn [todo]
           (ctx/handle-action (ctx/focus (lens/>> :todos (todo-id (:id todo)))
                                         (dom/keyed (str (:id todo))
                                                    (to-do-item)))
                              identity))
         (:todos app-state)))

   (dom/div
    (ctx/local-state (text-input))
    (ctx/handle-action (button (str "Add #" (:next-id app-state)))
                       ->Submit)))

  handle-message
  (fn [msg]
    (cond
      (instance? Submit msg)
      (let [next-id (:next-id app-state)]
        (reacl/return
         :local-state ""
         :app-state
         (assoc app-state
                :todos
                (concat (:todos app-state)
                        [(->Todo next-id next-text false)])
                :next-id (+ 1 next-id))))

      (instance? Delete msg)
      (let [id (:id (:todo msg))]
        (reacl/return :app-state
                      (assoc app-state
                             :todos 
                             (remove (fn [todo] (= id (:id todo)))
                                     (:todos app-state)))))
      :else
      (assert false msg)))
  )

(reacl/render-component
 (.getElementById js/document "content")
 (ext/reacl-class to-do-app)
 (TodosApp. 0 []))

#_(ext/render-component
   (.getElementById js/document "content")
 to-do-app
 (TodosApp. 0 []))
