;; Requires: hide-region, expand-region

;; Hide region
(require 'hide-region)

;; Expand region - select regions
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)


(provide 'fd-expand-region)
