;;; clj2el.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:
;; Maintainer:
;; Created: March 21, 2023
;; Modified: March 21, 2023
;; Version: 0.0.1
;; Homepage: https://github.com/borkdude/clj2el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defcustom clj2el-command
  "clj2el"
  "The command used to execute clj2el."
  :type 'string
  :group 'clj2el)

(defun clj2el-transpile-buffer ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) clj2el-command (current-buffer)))

(defun clj2el-transpile-region ()
  (interactive)
  (shell-command-on-region (point) (mark) clj2el-command (current-buffer) 't))

(defmacro clj2el-clj! (expr)
  "Transpile clojure like EXPR form to elisp and eval it.

Transpilation is performed by sending EXPR as input to
`clj2el-command'.  Signals an error if the program cannot be find
or an error is reported."
  (let* ((expr-as-string (prin1-to-string expr))
         (temp-buf "*el2clj-work*"))
    (get-buffer-create temp-buf)
    (let* ((elisp-code (with-current-buffer temp-buf
                         (erase-buffer)
                         (insert expr-as-string)
                         (when (not (= (shell-command-on-region (point-min) (point-max) clj2el-command temp-buf) 0))
                           (error ":clj23el-clj!-cmd-error :clj2el-command %S :error %s"
                                  clj2el-command (buffer-substring-no-properties (point-min) (point-max))))
                         (buffer-substring (point-min) (point-max))))
           (read (read-from-string elisp-code))
           (expr (car read)))
      expr)))

(defmacro clj2el-comment
    (&rest _exprs))

(clj2el-comment
 (setq clj2el-command "bb -x clj2el.exec/exec")
 (clj2el-clj!
  (do (defn foo [x] (inc x))
      (defn bar [x] (inc x))))
 (foo (bar 3)) ;;=> 5
 (clj2el-clj!
  (def m {:a 1 :b 2})
  )
 m ;;=> (:a 1 :b 2)
 (clj2el-clj!
  (get m :a)) ;;=> 1
 (clj2el-clj!
  (do
   (defn foo [m]
     (get m :a))
   (foo {:a 1}))) ;;=> 1
 (clj2el-clj! (do (get {'foo 1 'dude 2} 'foo)))
 (clj2el-clj!
  ((juxt inc dec)
   1))
 (clj2el-clj!
  ((juxt + -)
   0 1 2 3 4 5))
 (clj2el-clj!
  (let [fns (juxt + -)]
    fns
    (funcall fns 1 2)))
 )

(provide 'clj2el)
;;; clj2el.el ends here
