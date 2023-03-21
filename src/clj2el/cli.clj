(ns clj2el.cli
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
  (let [clj2el-fn (partial map clj2el)]
    (-> *in* slurp read-str-multiple clj2el-fn write-str-multiple print)))
