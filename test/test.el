(require 'clj2el)

(ert-deftest juxt ()
  (should (equal
           (clj2el-clj! ((juxt inc dec) 1))
           '(2 0)))
  (should (equal
           (clj2el-clj! ((juxt + -)
                         0 1 2 3 4 5))
           '(15 -15)))
  ;; Currently, `funcall' is required here. Might change in the future.
  (should (equal
           (clj2el-clj! (let [fns (juxt + -)]
                          (funcall fns 0 1 2 3 4 5)))
           '(15 -15)))
  ;; Missing `funcall'
  (should-error (clj2el-clj! (let [fns (juxt + -)]
                               (fns 0 1 2 3 4 5)))))
