;; ORG mode
(require 'org)

;; Set up org-mode capture system
(if (and (file-exists-p my-orgmode-dir)
	 (eq t (car (file-attributes my-orgmode-dir)))) ; It is actually a directory
    (setq org-default-notes-file (concat my-orgmode-dir my-notes-file))
  (message (format "Ormode directory is not valid: %s" my-orgmode-dir)))

(add-to-list 'org-modules 'org-habit)
(setq org-log-repeat "time")

(setq org-agenda-custom-commands
      '(("h" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))))

(setq org-directory my-orgmode-dir)
(setq org-mobile-directory my-orgmode-dir)
(setq org-agenda-files (mapcar (lambda (x) (concat org-directory x)) my-agenda-files))

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

;; Aspell
(setq ispell-program-name "aspell")

(provide 'fd-org)
