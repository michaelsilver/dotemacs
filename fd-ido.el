
;; Ido mode
(require 'ido)
(require 'ido-speed-hack)
(require 'ido-better-flex)
(require 'ido-ubiquitous)
(ido-mode t)
(ido-ubiquitous-mode t)
(setq ido-save-directory-list-file (my-expand-path ".ido.last"))
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
;; (ido-everywhere t)
;; This is mainly for just swapped letters. It sometimes doesnt catch
;; entire words
(ido-better-flex/enable)
;; (setq ido-file-extensions-order '(".c" ".cpp" ".h" ".py" ".txt" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
(setq org-completion-use-ido t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(provide 'fd-ido)
