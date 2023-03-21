(ns clj2el.api
  (:require [clj2el.internal]))

(defn clj2el [form]
  (clj2el.internal/transpile form {}))

(comment
  (clj2el '(map inc [1 2 3]))
  (clj2el '(let [inc #'inc] (map inc [1 2 3])))
  )
