;; Require this in every module to make sure it can run independently.

(setq dotemacs-dir "~/.emacs.d/")
(add-to-list 'load-path dotemacs-dir)
(defun my-expand-path (f) (concat dotemacs-dir f))
(add-to-list 'load-path (my-expand-path "el-get/el-get"))
;; (eval-when-compile
;;   (load-file (my-expand-path "fd-el-get.el")))
