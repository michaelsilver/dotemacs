
;; YASnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/el-get/yasnippet/snippets/" "~/.emacs.d/my-snippets/"))
(yas--initialize)

(dolist (sdir yas-snippet-dirs)
  (when (file-accessible-directory-p sdir)
    (yas/load-directory sdir)))

(provide 'fd-yasnippet)
