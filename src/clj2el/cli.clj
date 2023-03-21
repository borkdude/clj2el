(ns clj2el.cli
  (:require
   [clj2el.api :refer [clj2el]]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [edamame.core :as e]))

(defn pprint-str-multiple-forms [forms]
  (str/join "\n" (map #(with-out-str (pprint %)) forms)))

(defn -main [& _args]
  (let [read-str (fn [s] (e/parse-string-all s {:all true}))
        clj2el-fn (partial map clj2el)
        write-str pprint-str-multiple-forms]
    (-> *in* slurp read-str clj2el-fn write-str print)))
