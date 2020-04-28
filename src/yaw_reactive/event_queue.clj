(ns yaw-reactive.event-queue
  "The implementation of the asynchronous event queue.")

(defn mk-event-queue
  "Creates an event queue with the specified `max-size` (maximum number of events).
  For now the maximum size is not taken into account because of the use of an `agent`
  internally."
  [max-size event-handler]
  (when-not (> max-size 0)
    (throw (ex-info "Maximum queue size must be strictly positive." {:max-size max-size})))
  (agent {:event-handler event-handler
          :events []}))

(defn push-event!
  "Push the provided `event` in the `queue`. This function can block
  if the maximum queue size is reached (TODO)."
  [queue event-id event-args]
  (send queue (fn [queue]  (update queue :events (fn [evs] (conj evs (into [event-id] event-args))))))
  (send queue (fn [queue]
                (let [events (:events queue)
                      events' ((:event-handler queue) events)]
                  (assoc queue :events events')))))
  
