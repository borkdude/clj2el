(ns clj2el.cli
  (:require
   [clj2el.api :refer [clj2el]]
   [clojure.edn :as edn]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [edamame.core :as e]

   ))

(defn read-multiple-forms [s]
  (edn/read-string (str "[" s "]")))

(defn prn-str-multiple-forms [forms]
  (str/join "\n" (map prn-str forms)))

(defn pprint-str [form] (with-out-str (pprint form)))

(defn pprint-str-multiple-forms [forms]
  (str/join "\n" (map pprint-str forms)))

#_
(pprint (e/parse-string-all "(f) (g) (h)" {:all true}))

(defn by-reinventing-edamame [args]
  (let [argset (set args)
        multiple? (contains? argset "--multiple") ;; assuming we don't want to depend on babashka.cli
        pretty? (contains? argset "--pretty")
        read-fn (if multiple? read-multiple-forms edn/read-string)
        write-str-fn (cond (and multiple? pretty?) pprint-str-multiple-forms
                           (and multiple? (not pretty?)) prn-str-multiple-forms
                           (and (not multiple?) pretty?) pprint-str
                           (and (not multiple?) (not pretty?)) prn-str)
        clj2el-fn (if multiple? (partial map clj2el) clj2el)]
    (-> *in* slurp read-fn clj2el-fn write-str-fn print)))

(defn sanely [args]
  ;; avoid --multiple and --pretty for now
  (let [read-str (fn [s] (e/parse-string-all s {:all true}))
        clj2el-fn (partial map clj2el)
        write-str pprint-str-multiple-forms]
    (-> *in* slurp read-str clj2el-fn write-str print)))

(defn -main [& args]
  (sanely args)
  #_
  (by-reinventing-edamame args))
