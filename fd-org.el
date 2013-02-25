;; ORG mode
(require 'org)

;; Set up org-mode capture system
(if (and (file-exists-p my-orgmode-agenda-dir)
	 (eq t (car (file-attributes my-orgmode-agenda-dir)))) ; It is actually a directory
    (setq org-default-notes-file (concat my-orgmode-agenda-dir my-notes-file))
  (message (format "Ormode directory is not valid: %s" my-orgmode-agenda-dir)))

(add-to-list 'org-modules "org-habit")
(setq org-log-repeat "time")

(setq org-agenda-custom-commands
      '(("h" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))))

(setq org-directory "~/Ubuntu One/OrgMode/")
(setq org-mobile-directory "~/Ubuntu One/OrgMode/")

;; Org mode key bindings
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map "\M-j" 'org-meta-return)
	     (define-key org-mode-map "\M-n" 'org-forward-element)
	     (define-key org-mode-map "\M-p" 'org-backward-element)))
(add-hook 'org-mode-hook
          #'(lambda ()
	      (define-key org-mode-map [(tab)] nil)))

;; Org agenda
(setq org-agenda-files (list my-orgmode-agenda-dir)
      org-hierarchical-todo-statistics nil
      org-support-shift-select 'always)

;; Aspell
(setq ispell-program-name "aspell")

(provide 'fd-org)
