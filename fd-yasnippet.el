;; YASnippet
(require 'yasnippet)
(setq yas-snippet-dirs (mapcar 'my-expand-path '( "my-snippets/" "el-get/yasnippet/snippets/")))
(yas-global-mode 1)

(provide 'fd-yasnippet)
