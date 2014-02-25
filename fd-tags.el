;; ETAGS
;; Requires: etags-table, gtags
(require 'etags)
(require 'etags-table)
(setq etags-table-search-up-depth 10
      etags-table-generate-tags t)

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
  ;; (add-hook 'c-mode-common-hook
  ;; 	    '(lambda ()
  ;; 	       ;; If gtags are not setup, set them up before finding tag
  ;; 	       (define-key c-mode-base-map "\M-." 'gtags-wrap-find-tag)
  ;; 	       (define-key c-mode-base-map "\M-*" 'gtags-pop-stack)
  ;; 	       (define-key c-mode-base-map "\C-ct" 'gtags-generate-or-update)))
  )


;; This is just because of an update

(setq ido-ubiquitous-command-exceptions nil)
(add-to-list 'ido-ubiquitous-command-exceptions 'gtags-find-tag)
(add-to-list 'ido-ubiquitous-command-exceptions 'gtags-wrap-find-tag)
(add-to-list 'ido-ubiquitous-command-exceptions 'find-tag)

;; Cscope: will replace gtags
(require 'cscope)

(setq cscope-master-info-table
      '(("linux"
	 ("cscope" "-p10" "-l" "-d" "-f" "/home/fakedrake/Projects/ThinkSilicon/xilinx-zynq-bootstrap/sources/linux-git/cscope.out")
	 nil "/home/fakedrake/Projects/ThinkSilicon/xilinx-zynq-bootstrap/sources/linux-git/")))

(defun fd/cscope-info (id dir)
  (list id
	(list "cscope" "-p10" "-l" "-d" "-f" (format "%s/cscope.out" dir))
	nil dir))

(defvar fd/point-stack nil
  "Push points here fore cscope. (buffer . point)")

(defun push-point-stack ()
  "Push the current position in the point stack."
  (interactive)
  (push (cons (current-buffer) (point)) fd/point-stack))

(defun pop-point-stack (&optional dont-jump)
  "Pop the point stack until you find a point you can actually go
to and go there."
  (interactive)
  (if fd/point-stack
      (let* ((ret (pop fd/point-stack))
	     (valid-ret (if (and ret (bufferp (car ret)))
			    ret (pop-point-stack t))))

	(if (or (not valid-ret) dont-jump)
	    (when (called-interactively-p 'interactive)
	      (message "Nowhere to jump (no valid jump).") nil)
	  (switch-to-buffer (car valid-ret))
	  (goto-char (cdr valid-ret))))
    (when (called-interactively-p 'interactive)
      (message "Nowhere to jump (empty stack).") nil)))

(defun fd/cscope-find-global-definition (directory remember)
  "Be sure to find the correct project root for cscope. Also
remember it for next time. If `cscope-id' is defined skip all
this. If remember is non-nil change the cscope-id"

  ;; XXX: scope id is not the onlu criterion that cscope can work out
  ;; where to look for cscope.el. We may actually be in the correct
  ;; project.p
  (interactive (if cscope-id '(nil nil)
		 (list (ido-read-directory-name "Cscope project root: ")
		       (y-or-n-p "Remember this? "))))

  (let ((cscope-id (or cscope-id
		       default-directory
		       buffer-file-name
		       buffer-name))
	(default-directory directory))

    (if (not (assoc cscope-id cscope-master-info-table))
	(add-to-list 'cscope-master-info-table (fd/cscope-info cscope-id directory)))

    (when remember
      (with-current-buffer (current-buffer)
	(setq-local cscope-id cscope-id)))

    (push-point-stack)
    (call-interactively 'cscope-find-global-definition)))

(defun fd-c-tagging-hook ()
  (define-key c-mode-base-map (kbd "M-.") 'fd/cscope-find-global-definition)
  (define-key c-mode-base-map (kbd "M-*") 'pop-point-stack))


(add-hook 'c-mode-common-hook 'fd-c-tagging-hook)

(provide 'fd-tags)
