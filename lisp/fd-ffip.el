
;; Find file in project
(require 'find-file-in-project)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "C-x p") 'ffip-open-projects)
(setq ffip-full-paths t)


(provide 'fd-ffip)
