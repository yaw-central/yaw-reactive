(ns yaw-reactive.reaction
  (:require [clojure.set :as set]
            [yaw-reactive.event-queue :as ev]
            [yaw-reactive.ratom :as ratom]
            [yaw-reactive.render :as render :refer [render!]]
            [yaw.keyboard :as kbd]))

;;{
;; # Reactive atoms
;;
;; Once again inspired by reagent, we introduce a notion of *reactive atom* which
;; is a specialiized form of a Clojure atom.
;;}

(defn reaction-handler [ratom controller dep-comps old-value new-value]
  ;;(println "Reaction triggerred")
  ;;(println "  ==> old value:" old-value)
  ;;(println "  ==> new value:" new-value)
  (if (= new-value old-value)
    (do ;;(println "No need for re-rendering (same state after a swap)")
      )
    ;; rendering starts now
    (let [components (:components @controller)]
      ;;(println "Rendering triggered by reaction")
      (loop [comps dep-comps, already-rendered #{}]
        (when (seq comps)
          ;; (if (already-rendered (first comps))
          ;; already recomputed (XXX: probably buggy optimization)
          ;;(recur (rest comps) already-rendered)
          ;; not yet recomputed
          (let [component (first comps)
                comp-infos (get components component)]
            (when (not (:previous-available comp-infos))
              (throw (ex-info "Component has not been rendered previously" {:component component
                                                                            :comp-infos comp-infos})))
            (let [[rendered seen] (render! controller (into [component] (:previous-args comp-infos)))]
              (recur (rest comps) (set/union already-rendered rendered)
                                        ;)
                     ))))))))


(defn reactive-atom [controller init-val]
  (let [rat (ratom/make-ratom controller reaction-handler init-val)]
    (add-watch rat ::reaction reaction-handler)
    rat))

(defn create-update-ratom [universe]
  (let [world (:world @universe)
        ratom (atom 0.0)]
    (.registerUpdateCallback world (reify yaw.engine.UpdateCallback
                                     (update [this delta-time]
                                       ;; (println "Update! " delta-time " ms")
                                       (swap! ratom (fn [_] delta-time)))))
    ratom))

(defn create-keyboard-atom [universe]
  (let [world (:world @universe)
        keyboard-state (atom {:keysdown #{}})]
    (.registerInputCallback
     world
     (reify yaw.engine.InputCallback
       (sendKey [this key scancode action mode]
         (let [action (kbd/action action)
               key (kbd/key key)]
           (if (or (= action :press) (= action :release))
             (swap! keyboard-state
                    (fn [old-state]
                      (case action
                        :press (assoc old-state
                                      :key key
                                      :action action
                                      :keysdown (conj (:keysdown old-state) key))
                        :release (assoc old-state
                                        :key key
                                        :action action
                                        :keysdown (disj (:keysdown old-state) key))))))))))
    keyboard-state))


;;==============
;; Reframe-like
;;==============

(defonce app-db (atom {}))
(defonce subscriptions (atom {}))
(defonce event-handlers (atom {}))

;; =============================
;; State management
;; =============================

(defn register-state
  "Register a state `id` in ap-db with the initial value `val`"
  [id val]
  (swap! app-db (fn [old]
                  (assoc old id (atom val)))))

(defn update-state
  "Change the value of the  state `id` by using a function `f`
  that takes the former value and calculate a new one"
  [state-id f]
  (if-let [state (get @app-db state-id)]
    (do #_(println "[update-state] state before = " @state)
        (swap! state f)
        #_(println "    ==> state after = " @state))
    (throw (ex-info (str "No such state id: " state-id) {:state-id state-id}))))

(defn read-state
  "Read app-db or read a state `id` in app-db"
  [state-id]
  (if-let [state (get @app-db state-id)]
    @state
    (throw (ex-info (str "No such state id: " state-id) {:state-id state-id}))))

;; event handling

(defn prepare-env [state-ids]
  (let [app-db @app-db]
    (reduce (fn [env state-id]
              (if-let [state (get app-db state-id)]
                (assoc env state-id @state)
                (throw (ex-info (str "No such state id: " state-id) {:state-id state-id
                                                                     :env env}))))
            {} state-ids)))

(defn handle-event-state-effects!
  [effects]
  ;;(println "[handle-effects] effects = " effects)
  (doseq [[state-id new-state] effects]
    (update-state state-id (fn [_] new-state))))

(defn handle-event
  "Function to treat events of the agent"
  [events]
  (if (seq events)
    (let [[event-id & args] (first events)]
      (if-let [handler (get @event-handlers event-id)]
        (let [env (prepare-env (:state-ids handler))
              ;; _ (do (println "[handle-event] event-id = " event-id)
              ;;       (println "      ==> env = " env)
              ;;       (println "      ==> args = " args))
              effects (apply (:handler-fn handler) env args)]
          ;; 1) handle the effects (state changes)
          (handle-event-state-effects! (dissoc effects :events))
          ;; 2) process the new events (if any)
          (if-let [new-events (get effects :events)]
            (let [events' (apply conj (rest events) new-events)]
              events')
            (rest events)))
        ;; no handler found
        ;; (throw (ex-info (str "No handler found for event id:" event-id) {:event-id event-id}))))
        ;; it is not an error, we silently drop the event
        (rest events)))
    ;; no event (signal ?)
    events))

 ;; TODO : exploit the specified max-size
(defonce event-queue (let [q (ev/mk-event-queue 4096 handle-event)
                           ];;_ (println "[event-queue] q= " q)]
                       q))


(defn register-subscription
  "Register a subcription `sub-id` in subscriptions with a function
  `f` returning a value when the state identified by `state-id` is changed."
  [ctrl state-id sub-id f]
  (if-let [state (get @app-db state-id)]
    (let [ratom (reactive-atom ctrl (f @state))]
      (add-watch state sub-id (fn [_ _ old new]
                                ;;(println "[sub] old = " old)
                                ;;(println "  ==> new = " new)
                                (swap! ratom (fn [_] (f new)))))
      (swap! subscriptions (fn [old]
                             (assoc old sub-id ratom))))
    ;; no such state
    (throw (ex-info (str "No such state: " state-id) {:state-id state-id
                                                      :sub-id sub-id}))))
    
(defn check-state-ids [state-ids]
  (loop [ids state-ids]
    (if (seq ids)
      (if (contains? @app-db (first ids))
        (recur (rest ids))
        (throw (ex-info (str "No such state id: " (first ids)) {:state-id (first ids)})))
      nil)))

(defn register-event
  "Register an event `id` in event-handlers with its handler `f`.
The states to read and possibly effect in the event handling are
 given explicitly (either a single id or a vector of ids)."
  ([id state-ids handler-fn]
   (let [state-ids (if (vector? state-ids)
                     state-ids
                     [state-ids])]
     (check-state-ids state-ids)
     (swap! event-handlers (fn [old]
                             (assoc old id {:state-ids state-ids
                                            :handler-fn handler-fn})))))
  ([id handler-fn]
   (swap! event-handlers (fn [old]
                           (assoc old id {:state-ids []
                                          :handler-fn handler-fn})))))


;;v is a vector, maybe we can find a value to pass args
;;or we need to remove the vector and just pass the id
(defn subscribe
  "Subscribe to a state in app-db by giving the id of the subscription
  and return a ratom linked with the state"
  [id]
  (if-let [ratom (get @subscriptions id)]
    ratom
    (throw (ex-info (str "No such subscription: " id) {:id id}))))



;; TODO (later): don't use an agent but rather an atom with an
;; immutable queue (with limited size)...
(defn dispatch
  "Send an event to the agent in order to be treated asynchronously"
  [[id & args]]
  ;;(println "[dispatch] [" id args "]")
  (ev/push-event! event-queue id args))

(defn dispatch-sync
  "Treatement the event synchronously instead of putting it in the async queue"
  [[id & args]]
  ;; (println "[dispatch-sync] [" id args "]")
  (let [new-events (handle-event [(apply conj [id] args)])]
    (doseq [new-event new-events]
      (dispatch-sync new-event))))

(defn activate!
  "Function to start and render a scene"
  [controller vscene]
  (dispatch-sync [:react/initialize])
  (render/render! controller vscene)
  (let [update (create-update-ratom controller)
        keyboard (create-keyboard-atom controller)]
    (add-watch update :update-yaw (fn [_ _ _ delta-time]
                                    ;;Maybe just dispatch instead of dispatch-sync ?
                                    (dispatch-sync [:react/frame-update delta-time])))
    (add-watch keyboard :keyboard-yaw (fn [_ _ _ keyboard-state]
                                        ;;Maybe just dispatch instead of dispatch-sync ?
                                        (dispatch [:react/key-update keyboard-state])))))

