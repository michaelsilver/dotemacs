(require 'cl)
(defvar dotemacs-dir "~/.emacs.d/"
  "The dotemacs dir.")

(defvar fd-secretary-enabled nil
  "Secretary mode loads configurations for gnus and automatically
  opens erc and other stuff. You may also have a slightly
  different face so you can tell them apart. Enable this with the
  `-secreatry' cli option.")

(defun my-expand-path (f)
  "Expand path to my load-path"
  (concat dotemacs-dir f))

(defun fd-enable-secreatry (switch)
  "Enable the secretary stuff"
  (setq fd-secretary-enabled t)
  (message "Secretary mode!!"))

(defvar fd-require-deps
  '(('fd-el-get t el-get)
    ('fd-misc t git-emacs bm autopair gist)
    ('fd-misc-programming t)
    ('fd-automode t yaml-mode cmake-mode markdown-mode)
    ('fd-visual t naquadah-theme)
    ('fd-clipboard t)
    ('fd-ido t ido-mode-el ido-speed-hack ido-better-flex ido-ubiquitous smex)
    ('fd-yasnippet t yasnippet yasnippet-snippets)
    ('fd-autocomplete t auto-complete)
    ('fd-python t python django-mode jedi)
    ('fd-undotree t undo-tree)
    ('fd-recentfiles t)
    ('fd-erc t)
    ('fd-bookmarks t)
    ('fd-tags t)
    ('fd-expand-region t hide-region)
    ('fd-lisp t clojure-mode cider ac-nrepl)
    ('fd-cc-mode t c-eldoc)
    ('fd-prolog t)
    ('fd-vimperator t vimperator-mode)
    ('fd-org t)
    ('fd-midnight t)
    ('fd-dired t)
    ('fd-term t multi-term)
    ('fd-compilation t compilation-setup)
    ('fd-mail t)
    ('fd-desktop t)
    (fd-chat t emacs-jabber))
  "Here is what should be loaded and when in an alist. (MODULE
  LOAD? EL_GET_PACKAGES ...). LOAD? is nil, t or some function
  with the module as arg that returns non-nil if we should load
  this. ")


(setq fd-valid-modules nil)
(setq fd-el-get-packaes nil)

(defun fd-pre-require (module-cons)
  "Given a `fd-require-deps' element setup the module. Populates
`fd-valid-modules' and `fd-el-get-packages'"
  (let ((module (car module-cons))
	(req (cadr module-cons))
	(el-packages (cddr module-cons)))
    (when (or (and (functionp req) (funcall req module))
	      req)
      ;; Do the work
      (add-to-list 'fd-valid-modules module)
      (dolist (ep el-packages)
	(add-to-list 'fd-el-get-packaes ep)))))

(defun fd-should-require (module)
  "Non-nil if module is to be required."
  (let ((mod (car d))
	(req (cadr d)))
    (if (functionp req) (funcall req) req)))

(defun fd-setup ()
  "Setup everything"
  (setq load-path '(dotemacs-dir))
  (setq command-switch-alist '(("secretary" . fd-enable-secreatry)))
  (add-to-list 'load-path (my-expand-path "el-get/el-get")) ; Change this to not be added forever

  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; Require what needs to be required
  (dolist  (d fd-require-deps)
    (fd-pre-require d))

  (dolist (m fd-valid-modules) (require m)))

;; Customs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Sandbox/TODO.org" "~/Ubuntu One/OrgMode/health.org" "/home/fakedrake/.orgmode/agenda/notes.org")))
 '(safe-local-variable-values (quote ((cscope-id . "linux") (eval progn (c-set-offset (quote innamespace) (quote 0)) (c-set-offset (quote inline-open) (quote 0))) (gtags-path-style quote root) (gtags-rootdir . "/homes/cperivol/Projects/Nema/ZYNQ/linux-zynq/") (gtags-rootdir . "~/Projects/linux-3.7-rc8/") (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "Welcome to emacs!")
