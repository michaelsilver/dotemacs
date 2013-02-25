;; YASnippet
(require 'yasnippet)
(setq yas-snippet-dirs (mapcar 'my-expand-path '("el-get/yasnippet/snippets/" "my-snippets/")))
(yas-global-mode 1)

(provide 'fd-yasnippet)
