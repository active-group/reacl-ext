(ns examples.todo.core
  (:require [reacl-ext.core :as ext :include-macros true]
            [reacl-ext.context :as ctx :include-macros true]
            [reacl2.core :as reacl]
            [reacl2.dom :as dom]))

(enable-console-print!)

(ext/defclass checkbox
  ;; "Control for a boolean value in the form of a checkbox"
  this checked []

  methods [(click [e] (reacl/send-message! this
                                           (.. e -target -checked)))]
  render
  (dom/input {:type "checkbox"
              :value checked
              :onchange click})
  
  handle-message
  (fn [checked]
    (reacl/return :app-state checked)))

(ext/defclass text-input this text []
  methods [(change [e]
                   (reacl/send-message!
                    this
                    (.. e -target -value)))]
  render
  (dom/input {:onchange change
              :value local-state})
  
  handle-message
  (fn [txt]
    (reacl/return :app-state txt)))

(ext/defclass button
  "Control that shows a button and emits an action (true) when that is clicked."
  this [label]
  methods [(click [_] (reacl/send-message! this :clicked))]
  
  render
  (dom/button {:onclick click}
              label)
  
  handle-message
  (fn [_]
    (reacl/return :action true)))

(defrecord TodosApp [next-id todos])

(defrecord Todo [id text done?])

(defrecord Delete [todo])


(ext/defclass to-do-item this todo []
  render
  (dom/div (ctx/bind todo (ctx/focus :done?
                                     (checkbox)))
           (ctx/map-action (button "Zap")
                           ->Delete)
           
           (:text @todo)))

(defn todo-id
  "A lens over the todo item with id in a list of todos (must exist)."
  [id]
  (fn
    ([list]
     (first (filter #(= id (:id %)) list)))
    ([list v]
     (map (fn [todo]
            (if (= id (:id todo))
              v
              todo))
          list))))

(comment
  ;; better alternative?
  (-> (to-do-item)
      (dom/keyed (str (:id todo)))
      (ctx/focus (lens/>> :todos (todo-id (:id todo))))
      (ctx/handle-action ->Delete)))

(ext/defclass to-do-app this app-state []
  local-state [next-text ""]

  render
  (dom/div
   (dom/h3 "TODO")
   (ctx/bind app-state
             (dom/div 
              (map (fn [todo]
                     (ctx/handle-action (ctx/focus (lens/>> :todos (todo-id (:id todo)))
                                                   (dom/keyed (str (:id todo))
                                                              (to-do-item)))
                                        ->Delete))
                   (:todos @app-state))))
   (dom/form
    (ctx/bind next-text (text-input))
    (ctx/handle-action (button (str "Add #" (:next-id @app-state)))
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
                                     (:todos app-state)))))))
  )

(ctx/render-component
 (.getElementById js/document "content")
 to-do-app
 (TodosApp. 0 []))
