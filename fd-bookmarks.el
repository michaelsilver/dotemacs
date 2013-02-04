
;; BOOKMARKS
(require 'bm)
(setq bm-highlight-style nil)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

(provide 'fd-bookmarks)
