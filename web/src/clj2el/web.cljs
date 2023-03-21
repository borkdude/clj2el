(ns clj2el.web
  (:require
   ["@codemirror/state" :as cs]
   ["@codemirror/view" :as cv]
   ["@nextjournal/lang-clojure" :as lc]
   ["codemirror" :as cm]
   [clojure.pprint :as pp]
   [edamame.core :as e]
   [clj2el.api :as clj2el]
   [clojure.string :as str]))

(defn cm-string [cm-instance]
  (-> cm-instance .-state .-doc .toString))

(defn eval-codemirror [cm-instance]
  (let [code-str (cm-string cm-instance)
        forms (e/parse-string-all code-str {:all true})
        evaled (mapv clj2el/clj2el forms)]
    evaled))

(defonce init-instances
  (let [elts (js/document.querySelectorAll ".clj2el-editor")]
    (doseq [^js elt elts]
      (let [no-editable? (.. elt -dataset -cljsShowcaseNoEditable)
            no-eval? (.. elt -dataset -cljsShowcaseNoEval)
            eval? (not no-eval?)
            doc (.-innerText elt)
            _ (set! (.-innerText elt) "")
            cm-ref (atom nil)
            res (js/document.createElement "pre")
            eval-me (fn []
                      (when eval?
                        (let [evaled (eval-codemirror @cm-ref)]
                          (set! (.-innerText res)
                                (str/join "\n" (map #(with-out-str (pp/pprint %)) evaled))))))
            ext (.of cv/keymap
                     (clj->js [{:key "Mod-Enter"
                                :run eval-me}]))
            state (cs/EditorState.create #js {:doc doc
                                              :extensions #js [cm/basicSetup, (lc/clojure), (.highest cs/Prec ext),
                                                               (cs/EditorState.readOnly.of no-editable?)]})
            cm (cm/EditorView. #js {:state state :parent elt})]
        (when eval?
          (let [btn (js/document.createElement "button")
                _ (set! (.-style btn) "float: right")
                _ (set! btn -innerText "Eval")
                _ (.addEventListener elt "click" eval-me)]
            (.appendChild elt btn)))
        (.appendChild elt res)
        (reset! cm-ref cm)
        (when eval? (eval-me))))))

