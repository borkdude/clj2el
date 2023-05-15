(require 'clj2el)

(require 'cl-lib)
(require 'ert)

(cl-defmacro with-clj2el-test (&rest body &aux (test-clj2el-command "bb -x clj2el.exec/exec"))
  "Execute BODY with `clj2el-command' bound to TEST-CLJ2EL-COMMAND.

The command is supposed to execute clj2el from source.

the following fucntions are also updated to be called interactively with a prefix arg:
- `clj2el-transpile-buffer'
- `clj2el-transpile-range'."
  (declare (indent 0))
  `(let ((clj2el-command ,test-clj2el-command))
     (cl-macrolet (;; use eval to pick up the updated
                   ;; `clj2el-command'.
                   (clj2el-clj! (expr) `(eval '(clj2el-clj! ,expr)))

                   ;; call with prefix arg.
                   (clj2el-transpile-buffer () '(let ((current-prefix-arg '(4)))
                                                  (call-interactively 'clj2el-transpile-buffer)))
                   (clj2el-transpile-range () '(let ((current-prefix-arg '(4)))
                                                 (call-interactively 'clj2el-transpile-range))))
       ,@body)))

(ert-deftest basic ()
  (with-clj2el-test

    ;; clj2el-transpile-buffer
    (should (string= "(1+ 2)\n"
                     (with-temp-buffer
                       (insert "(inc 2)")
                       (clj2el-transpile-buffer)
                       (buffer-substring-no-properties (point-min) (point-max)))))

    ;; clj2el-transpile-region
    (should (string= "[[ (1+ 2)\n ]]"
                     (with-temp-buffer
                       (insert "[[ (inc 2) ]]")
                       (set-mark 4)
                       (goto-char 11)
                       (clj2el-transpile-region)
                       ;; (test--call-interactively-with-prefix 'clj2el-transpile-region)
                       (buffer-substring-no-properties (point-min) (point-max)))))


    ;; clj2el-clj!
    (should (= 3  (clj2el-clj! (inc 2))))))

(ert-deftest juxt ()
  (with-clj2el-test
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
                                 (fns 0 1 2 3 4 5))))))
