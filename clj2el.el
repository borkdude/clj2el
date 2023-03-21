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

(defun clj2el-transpile-buffer ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "clj2el" (current-buffer)))

(defun clj2el-transpile-region ()
  (interactive)
  (shell-command-on-region (point) (mark) "clj2el" (current-buffer) 't))

(provide 'clj2el)
;;; clj2el.el ends here
