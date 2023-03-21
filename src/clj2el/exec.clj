(ns clj2el.exec
  (:require
   [clj2el.api :refer [clj2el]]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [edamame.core :as e]))

(defn read-str-multiple [s]
  (e/parse-string-all s {:all true}))

(defn write-str-multiple [forms]
  (str/join "\n" (map #(with-out-str (pprint %)) forms)))

(defn -main [& _args]
  (->> *in* slurp read-str-multiple (map clj2el) write-str-multiple print))
