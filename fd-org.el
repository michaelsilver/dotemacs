;; ORG mode
(require 'org)
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map "\M-j" 'org-meta-return)
	     (define-key org-mode-map "\M-n" 'org-forward-element)
	     (define-key org-mode-map "\M-p" 'org-backward-element)))
(add-hook 'org-mode-hook
          #'(lambda ()
	      (define-key org-mode-map [(tab)] nil)))

;; Set up org-mode capture system
(if (and (file-exists-p my-orgmode-agenda-dir)
	 (eq t (car (file-attributes my-orgmode-agenda-dir)))) ; It is actually a directory
    (setq org-default-notes-file (concat my-orgmode-agenda-dir my-notes-file))
  (message (format "Ormode directory is not valid: %s" my-orgmode-agenda-dir)))

;; Org mode key bindings
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Org agenda
(setq org-agenda-files (list my-orgmode-agenda-dir)
      org-hierarchical-todo-statistics nil
      org-support-shift-select 'always)

;; Aspell
(setq ispell-program-name "aspell")

(provide 'fd-org)
