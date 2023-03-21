(ns clj2el.cli
  (:require
   [clj2el.api :refer [clj2el]]
   [clojure.edn :as edn]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]))

(defn read-multiple-forms [s]
  (edn/read-string (str "[" s "]")))

(defn prn-str-multiple-forms [forms]
  (str/join "\n" (map prn-str forms)))

(defn pprint-str [form] (with-out-str (pprint form)))

(defn pprint-str-multiple-forms [forms]
  (str/join "\n" (map pprint-str forms)))

(defn -main [& args]
  (let [argset (set args)
        multiple? (contains? argset "--multiple") ;; assuming we don't want to depend on babashka.cli
        pretty? (contains? argset "--pretty")
        read-fn (if multiple? read-multiple-forms edn/read-string)
        write-str-fn (cond (and multiple? pretty?) pprint-str-multiple-forms
                           (and multiple? (not pretty?)) prn-str-multiple-forms
                           (and (not multiple?) pretty?) pprint-str
                           (and (not multiple?) (not pretty?)) prn-str)]
    (-> *in* slurp read-fn clj2el write-str-fn print)))
