(ns clj2el.internal
  (:require [clj2el.internal.destructure :as destructure]))

(declare transpile)

(defn add-local [env local]
  (update env :locals assoc local local))

(defn add-locals [env locals]
  (update env :locals merge (zipmap locals locals)))

(defn normalize-arg [arg]
  (if (= '& arg) '&rest arg))

(defmulti transpile-call (fn [call _env]
                           (first call)))

(defmethod transpile-call 'defn [[_defn name args & body] env]
  (let [[args & body] (destructure/maybe-destructured args body)]
    `(~'defun ~name ~(map normalize-arg args) ~@(map #(transpile % env) body))))

(defmethod transpile-call 'let [[_let bindings & body] env]
  (let [bindings (destructure/destructure bindings)
        [bindings env]
        (reduce (fn [[bindings env] [binding expr]]
                  [(conj bindings (list binding (transpile expr env))) (add-local env binding)])
                [[] env]
                (partition 2 bindings))]
    `(~'let* ~(sequence bindings)
             ~@(map #(transpile % env) body))))

(defmethod transpile-call 'inc [[_inc expr] env]
  `(~(symbol "1+") ~(transpile expr env)))

(defmethod transpile-call 'map [[_map fn expr] env]
  `(~'mapcar ~(if (and (symbol? fn)
                       (not (get (:locals env) fn)))
                (transpile (list 'var fn) env)
                (transpile fn env))
             ~(transpile expr env)))

(defmethod transpile-call 'fn [[_fn args & body] env]
  `(~'lambda ~(map normalize-arg args) ~@(map #(transpile % env) body)))

(defmethod transpile-call 'var [[_var sym] _env]
  (symbol (str "#'" (case sym
                      inc (symbol "1+")
                      sym))))

(defmethod transpile-call 'first [[_ arg] env]
  `(~'car ~(transpile arg env)))

(defmethod transpile-call 'rest [[_ arg] env]
  `(~'cdr ~(transpile arg env)))

(defmethod transpile-call 'comment [_form _env]
  nil)

(defmethod transpile-call 'do [[_do & exprs] env]
  (list* 'progn (map #(transpile % env) exprs)))

(defmethod transpile-call 'def [[_def & exprs] env]
  (list* 'setq (map #(transpile % env) exprs)))

(defmethod transpile-call 'assoc [[_assoc m k v] env]
  (list 'plist-put (transpile m env) (transpile k env) (transpile v env)))

(defmethod transpile-call 'get [[_assoc m k] env]
  (list 'plist-get (transpile m env) (transpile k env)))

(defmethod transpile-call 'nth [[_nth obj idx] env]
  (list 'elt (transpile obj env) (transpile idx env)))

(defmethod transpile-call 'juxt [[_juxt & fns] env]
  (list 'lambda
        '(&rest xs)
        `(~'mapcar
          ~'(lambda (f) (apply f xs))
          '~(map #(transpile % env) fns))))

(defmethod transpile-call :default [form env]
  (sequence (map #(transpile % env) form)))

(defn transpile-symbol [sym]
  (condp = sym
    'inc (symbol "1+")
    'dec (symbol "1-")
    sym))

(defn transpile [form env]
  (cond
    (seq? form)
    (transpile-call form env)
    (vector? form) (list* 'vector (map #(transpile % env) form))
    (map? form) (cons 'list (map #(transpile % env) (apply concat form)))
    (symbol? form) (transpile-symbol form)
    :else form))

;;;; Scratch

(comment
  (transpile '(let [[x] '(1)]
                x) {})

  (transpile '(let [{:keys [a]} {:a 1}]
                a) {})

  (transpile '(let [{:syms [a]} {'a 1}]
                a) {})

  (transpile '(defn foo [x & [opt]] opt) {})

  (transpile '(let [[x y] [1 2 3]]
                [x y]) {})

  (transpile '(juxt inc dec) {})
  ;; => (lambda
  ;;     (&rest xs)
  ;;     (mapcar
  ;;      (lambda (f) (apply f xs))
  ;;      '(1+ 1-)))
  )
