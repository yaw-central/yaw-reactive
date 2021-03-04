(ns yaw-reactive.util)

(defn mk-ok
  "Makes an ok value."
  [val]
  {:tag :ok :value val})

(defn mk-ko
  "Makes a ko value."
  [val]
  {:tag :ko :value val})

(defn unwrap-all
  "Yeilds the inner value if it is ok or ko, or throws an exception."
  [e]
  (case (get e :tag)
    :ok (get e :value)
    :ko (get e :value)))

(defn unwrap-or
  "Yeilds the inner value if it is ok or a default value"
  [e d]
  (case (get e :tag)
    :ok (get e :value)
    d))