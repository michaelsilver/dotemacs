;; ETAGS
;; Requires: etags-table, gtags
(require 'etags)
(require 'etags-table)
(setq etags-table-search-up-depth 10
      etags-table-generate-tags t)
(add-to-list 'ido-ubiquitous-command-exceptions 'find-tag)

;; CC-MODE
(defun gtags-generate-gtags ()
  "Generate a gtags file in the querried directory"
  (let* ((proj-root (read-directory-name "Root of the project: "))
	 (cmd (format "cd %s ; gtags" (expand-file-name proj-root))))
    (when (not (string= "" proj-root))
      (message (format "Generating gtags files: %s" cmd))
      (shell-command cmd))))

(defun gtags-update-gtags ()
  "Update the gtags files"
  (let ((gen-cmd "global -u"))
    (message (format "Updating gatgs files: %s" gen-cmd))
    (shell-command gen-cmd)))

(defun gtags-generate-or-update ()
  "If you can update the gtags files. If not generate them."
  (interactive)
  (if (null (gtags-get-rootpath))
      (gtags-generate-gtags)
    (gtags-update-gtags)))


(defun gtags-wrap-find-tag ()
  "Just a simple wrapper for gtags-find-tag. This is needed for
ubiquitous exceptions, but it also tries to use imenu before
actually trying to use gtags. This way if we have a single file
project we do not need gtags to jump around. Also we dont need to
regenerate gtags for local symbols."
  (interactive)
  (let* ((current-token (gtags-current-token))
	 (imenu-tokens (mapcar 'car (imenu--make-index-alist)))
	 (use-imenu (member current-token imenu-tokens)))
    (if use-imenu
	(progn (gtags-push-context) (call-interactively 'imenu))
      (unless (gtags-get-rootpath)
	(gtags-generate-gtags))
      (widen)
      (gtags-find-tag))))

;; (defadvice gtags-goto-tag (around ad-fuse-imenu-gtags (tagname passer) activate)
;;   (message (concat "Trying imenu for " tagname))
;;   (when (null (imenu tagname))
;;     ad-do-it))

;; if your etags file is in some other location please add that location here
(setq gtags-elisp-file (find-if 'file-exists-p '("/usr/share/gtags/gtags.el"
						 "/usr/share/emacs/site-lisp/global/gtags.el")))
(when gtags-elisp-file
  (load-file gtags-elisp-file)
  (add-hook 'c-mode-common-hook
	    '(lambda ()
	       ;; If gtags are not setup, set them up before finding tag
	       (define-key c-mode-base-map "\M-." 'gtags-wrap-find-tag)
	       (define-key c-mode-base-map "\M-*" 'gtags-pop-stack)
	       (define-key c-mode-base-map "\C-ct" 'gtags-generate-or-update))))


(add-to-list 'ido-ubiquitous-command-exceptions 'gtags-find-tag)
(add-to-list 'ido-ubiquitous-command-exceptions 'gtags-wrap-find-tag)

(provide 'fd-tags)
