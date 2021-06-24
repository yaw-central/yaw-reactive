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

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))