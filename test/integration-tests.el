(require 'clj2el)

(require 'cl-lib)
(require 'ert)

(defmacro with-temp-bbin-install (&rest body)
  "Create a temp dir to install clj2el to with bbin and run BODY.

The following functions are updated in BODY to use this installation

- `clj2el-clj!'
- `clj2el-transpile-buffer' (also called interactively with prefix arg)
- `clj2el-transpile-range'  (also called interactively with prefix arg)."
  (declare (indent 0))
  `(let ((bbin-install-dir (make-temp-file "clj2el" t))
         (bbin-exec (executable-find "bbin"))
         (env-bbd (getenv "BABASHKA_BBIN_DIR"))
         (env-path (getenv "PATH")))
     ;; (message ":temp-bbin-install-dir-created-at %s" bbin-install-dir)
     (unwind-protect
         (progn
           (should bbin-exec)
           (setenv "BABASHKA_BBIN_DIR" bbin-install-dir)
           (setenv "PATH" (concat (expand-file-name "bin" bbin-install-dir) path-separator
                                    (getenv "PATH")))
           (with-temp-buffer
             (let ((ret (call-process-shell-command "bbin" nil (current-buffer) nil
                                                    "install" ".")))
               (when (not (= ret 0))
                 (message ":bbin-install-error %s" (buffer-substring-no-properties (point-min) (point-max))))
               (should (= ret 0))))
           (cl-macrolet ((clj2el-clj! (expr) `(eval '(clj2el-clj! ,expr)))
                         (clj2el-transpile-buffer () '(let ((current-prefix-arg '(4)))
                                                        (call-interactively 'clj2el-transpile-buffer)))
                         (clj2el-transpile-range () '(let ((current-prefix-arg '(4)))
                                                       (call-interactively 'clj2el-transpile-range))))
             ,@body))
       (progn
         (setenv "BABASHKA_BBIN_DIR" env-bbd)
         (setenv "PATH" env-path)
         ;; (message ":temp-bbin-install-deleting... %s" bbin-install-dir)
         (delete-directory bbin-install-dir t)))))


(ert-deftest integration ()
  (with-temp-bbin-install

    ;; clj2el-transpile-buffer
    (should (string= "(1+ 3)\n"
                     (with-temp-buffer
                       (insert "(inc 3)")
                       (clj2el-transpile-buffer)
                       (buffer-substring-no-properties (point-min) (point-max)))))

    ;; clj2el-transpile-region
    (should (string= "[[ (1+ 4)\n ]]"
                     (with-temp-buffer
                       (insert "[[ (inc 4) ]]")
                       (set-mark 4)
                       (goto-char 11)
                       (clj2el-transpile-region)
                       (buffer-substring-no-properties (point-min) (point-max)))))


    ;; clj2el-clj!
    (should (= 6  (clj2el-clj! (inc 5))))))
