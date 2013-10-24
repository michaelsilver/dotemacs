;; YASnippet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (my-expand-path "my-snippets/"))
(yas-global-mode 1)

(global-set-key (kbd "C-h y") 'yas-describe-tables)
(global-set-key (kbd "<backtab>") 'yas-expand) ; S-Tab

(provide 'fd-yasnippet)
