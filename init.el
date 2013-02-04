(require 'cl)

;; EL-GET
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/")

;; This is up here so we can define templates in personal.el
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")))

;; Ensure that personal.el exists
(cond ((not (file-readable-p "~/.emacs.d/personal.el"))
       (progn
	 (copy-file "~/.emacs.d/dummy-personal.el" "~/.emacs.d/personal.el")
	 (message "Copied dummy personal preferences to personal.el"))))

(load-file   "~/.emacs.d/personal.el")


(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(setq my:el-get-packages
      '(
	;; Python
	;; ipython
	python-mode
	python-pep8
	pylookup

	;; Auto Complete packages
	yasnippet
	auto-complete

	;; themes I like
	tomorrow-night-paradise-theme
	naquadah-theme
	obsidian-theme

	;; ido
	ido-mode-el
	ido-speed-hack
	ido-better-flex
	ido-ubiquitous
	smex

	;; Misc
	;;	slime
	smartparens
	nrepl
	vimperator-mode
	c-eldoc
	dired-sort
	hide-region
	;; gist
	org-mode
	markdown-mode
	expand-region
	etags-table
	zencoding-mode
	git-emacs
	visual-basic-mode
	bm
	goto-last-change
	find-file-in-project
	xml-rpc lisppaste
	undo-tree))

(setq
 el-get-sources
 '(el-get
   zencoding-mode
   python-pep8
   python-mode
   git-emacs
   visual-basic-mode

   (:name jdee :description "Integrated Development Environment for Java" :type emacsmirror :pkgname "jdee" :required ((("arc-mode" arc-mode) ("avl-tree" avl-tree) ("browse-url" browse-url) ("cc-mode" cc-fonts cc-mode) ("cedet" eieio speedbar) ("cl" cl) ("comint" comint) ("compile" compile) ("custom" cus-edit custom) ("easymenu" easymenu) ("eldoc" eldoc) ("elib" avltree) ("emacs-core" font-lock overlay sort) ("emacs-obsolete" lmenu) ("etags" etags) ("executable" executable) ("flymake" flymake) ("htmlize" htmlize) ("imenu" imenu) ("jdee" jde-autoload) ("regexp-opt" regexp-opt) ("reporter" reporter) ("tempo" tempo) ("thingatpt" thingatpt) ("tree-widget" tree-widget) ("widget" wid-edit widget) (nil semantic/senator))) :depends (elib cedet cc-mode))

   (:name vimperator-mode
	  :description "Edit vimperator files"
	  :type github
	  :pkgname "xcezx/vimperator-mode")

   (:name undo-tree
	  :description "Visualize undo history as a tree"
	  :type github
	  :pkgname "emacsmirror/undo-tree")

   (:name etags-table
	  :description "Etags is smart enough to look for a table in fs."
	  :type github
	  :pkgname "fakedrake/etags-table")

   (:name obsidian-theme
	  :description "My theme"
	  :type github
	  :pkgname "fakedrake/obsidian-theme")

   ;; This is temporary until the pull request is dealt with in upstream
   (:name find-file-in-project
	  :description "Find a file in the current project"
	  :type github
	  :pkgname "fakedrake/find-file-in-project")

   (:name ido-better-flex
	  :description "Better flex matching for ido"
	  :type github
	  :pkgname "orfelyus/ido-better-flex"
	  :compile "ido-better-flex.el")

   (:name ido-mode-el
	  :description "Better flex matching for ido"
	  :type github
	  :pkgname "orfelyus/ido-mode-el"
	  :compile "ido.el")

   (:name ido-speed-hack
	  :description "Better flex matching for ido"
	  :type github
	  :pkgname "orfelyus/ido-speed-hack"
	  :compile "ido-speed-hack.el")

   (:name yasnippet
	  :website "https://github.com/capitaomorte/yasnippet.git"
	  :description "YASnippet is a template system for Emacs."
	  :type github
	  :pkgname "capitaomorte/yasnippet"
	  :features "yasnippet"
	  :compile "yasnippet.el")

   (:name bm
	  :description "Simple bookmark manager"
	  :type github
	  :pkgname "joodland/bm")

   (:name smex				; a better (ido like) M-x
	  :after (progn
		   (setq smex-save-file "~/.emacs.d/.smex-items")
		   (global-set-key (kbd "M-x") 'smex)
		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
   (:name goto-last-change		; move pointer back to last change
	  :after (progn
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

;;
;; Some recipes require extra tools to be installed
;;
;; Note: el-get-install requires git, so we know we have at least that.
;;
(when (el-get-executable-find "cvs")
  (add-to-list 'el-get-sources 'emacs-goodies-el)) ; the debian addons for emacs

(when (el-get-executable-find "svn")
  (loop for p in '(psvn    		; M-x svn-status
		   yasnippet		; powerful snippet mode
		   )
	do (add-to-list 'el-get-sources p)))

(el-get 'sync my:el-get-packages)

(require 'hide-region)
;; (require 'gist)
;; on to the visual settings
(if window-system
    (let ((comment "IndianRed2"))
      (global-hl-line-mode t)
      (require 'naquadah-theme)
      (load-theme 'naquadah t)
      (custom-theme-set-faces
       'naquadah
       `(mode-line ((t (:height 1.1 :background "gray30"))))
       `(minibuffer-prompt ((t (:foreground "orange1"))))
       `(region ((t (:background "gray25"))))



       ;; Development
       `(font-lock-comment-face ((t (:foreground ,comment))))
       `(font-lock-function-name-face ((t (:foreground "orange1" :bold t))))
       `(font-lock-doc-face ((t (:foreground ,comment))))
       `(font-lock-doc-string-face ((t (:foreground ,comment))))
       `(link ((t (:foreground  "#729fcf" :underline t))))

       ;; ERC
       `(erc-prompt-face ((t (:background "#f57900" :bold t :foreground "gray10")))))))

(line-number-mode 1)	; have line numbers and
(column-number-mode 1)	; column numbers in the mode line
(mouse-avoidance-mode 'banish)
(tool-bar-mode -1)	; no tool bar with icons
(scroll-bar-mode -1)	; no scroll bars
(add-hook 'find-file-hook (lambda () (setq show-trailing-whitespace t)))
(global-linum-mode 1)	; add line numbers on the left
(which-function-mode t)

;; CLIPBOARD
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value
      x-select-enable-primary t)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;; Ido mode
(require 'ido)
(require 'ido-speed-hack)
(require 'ido-better-flex)
(require 'ido-ubiquitous)
(ido-mode t)
(ido-ubiquitous-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
;; (ido-everywhere t)
;; This is mainly for just swapped letters. It sometimes doesnt catch
;; entire words
(ido-better-flex/enable)
;; (setq ido-file-extensions-order '(".c" ".cpp" ".h" ".py" ".txt" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
(setq org-completion-use-ido t)


;; Basic stuff
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                                          ("%b - Dir:  " default-directory)))))))

(require 'server)
(if (server-running-p)
    (message "Skipping server creation, one already exists")
  (server-start))
(delete-selection-mode t)
(show-paren-mode
 t)
(smartparens-global-mode t)
(set-face-attribute 'default nil :height 90)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline 'query)
(set-input-method 'greek)
(toggle-input-method)
(setq scroll-step 1)
(global-set-key "\C-Z" 'revert-buffer)
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1))) ;; fix tabcompletion

(add-to-list 'auto-mode-alist '("[.]zcml" . nxml-mode))
(add-to-list 'auto-mode-alist '("[.]pt" . html-mode))
(add-to-list 'auto-mode-alist '("[.]sim" . asm-mode))

;; Indent buffer
(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)))
(global-set-key "\C-x\\" 'indent-buffer)

;; YASnippet
(require 'yasnippet)
(setq yas-snippet-dirs "~/.emacs.d/el-get/yasnippet/snippets/")
(yas--initialize)
(yas/load-directory yas-snippet-dirs)
(yas/load-directory "~/.emacs.d/my-snippets/")


;; Auto Complete
(require 'auto-complete)
(require 'auto-complete-config)
;; (require 'auto-complete+)
;; (require 'auto-complete-extension)
;; (require 'auto-complete-yasnippet)
;; (require 'auto-complete-etags)
;; (require 'ac-python)
;; (require 'auto-complete-emacs-lisp)
(ac-config-default)
(add-to-list 'ac-dictionary-directories (expand-file-name "dictionaries"))
(add-to-list 'ac-modes '(org-mode))
(setq ac-use-fuzzy t)
(set-default 'ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-filename ac-source-words-in-same-mode-buffers))

;; Fixed some weird error with emacs 24.3
(defalias 'cl-defsubst-expand 'cl--defsubst-expand)

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

;; Python

;; Fix docstring paragraph filling
(add-hook 'python-mode-hook (lambda ()
			      (setq paragraph-start (concat paragraph-start "\\|\\s-*\"\"\".*$"))))

(defun py-my-indent-region (&optional min max)
  "Stupidly clamp indentation to the closest multiple of 4 spaces."
  (interactive)
  (save-excursion
    (let ((top (or min (point-min)))
	  (bottom (or max (point-max)))
	  (line-move-visual nil))
      (goto-char top)
      (while (<= (point) bottom)
	(indent-line-to
	 (* 4 (round (/ (float (current-indentation)) 4))))
	(next-line) (end-of-line)))))


(setq py-mode-map python-mode-map)
;; (setq ipython-command "/usr/bin/ipython2")
;; (setq ipython-completion-command-string "print ';'.join(__IP.Completer.all_completions('%s'))\n")
;; (require 'ipython2)

(setq pylookup-dir "~/.emacs.d/el-get/pylookup/")
(require 'pylookup)
;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

(require 'python-mode)
(define-key python-mode-map "\C-cp" '(lambda () (interactive) (insert "import ipdb; ipdb.set_trace()")))
(define-key python-mode-map "\C-ch" 'pylookup-lookup)
(define-key python-mode-map "\C-x\\" 'py-my-indent-region)

(setq py-split-windows-on-execute-p nil)

(defadvice compile (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t
if breakpoints are present in `python-mode' files"
  (when (derived-mode-p major-mode 'python-mode)
    ;; set COMINT argument to `t'.
    (ad-set-arg 1 t)))

;; UNDO TREE
(require 'undo-tree)
(global-undo-tree-mode t)

;; RECENT FILES
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 100)
(defun steve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
(global-set-key [(meta f11)] 'steve-ido-choose-from-recentf)

;; ERC
;; check channels
(require 'erc)

(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; joining && autojoing

;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be be a bit different, which would screw up autoconnect
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#emacs")
	(".*\\.freenode.net" "#ngep")
	(".*\\.freenode.net" "#plone")))

(defun fakedrake-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (select-frame (make-frame '((name . "Emacs IRC")
 			      (minibuffer . t))))
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick my-freenode-nick :full-name my-freenode-fullname :password my-freenode-password))))

(defun my-destroy-erc ()
  "Kill all erc buffers!!"
  (interactive)
  (save-excursion
    (dolist (i (buffer-list))
      (with-current-buffer i
	(cond
	 ((eq major-mode 'erc-mode) (kill-buffer (current-buffer))))))))
;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c e s") 'fakedrake-erc-start-or-switch) ;; ERC
(global-set-key (kbd "C-c e k") 'my-destroy-erc)

(require 'lisppaste)

;; BOOKMARKS
(require 'bm)
(setq bm-highlight-style nil)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;; ETAGS
(require 'etags-table)
(setq etags-table-search-up-depth 10
      etags-table-generate-tags t)
(add-to-list 'ido-ubiquitous-command-exceptions 'find-tag)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)


;; Find file in project
(require 'find-file-in-project)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "C-x p") 'ffip-open-projects)
(setq ffip-full-paths t)

(put 'narrow-to-region 'disabled nil)

;; Zoom
(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height
		      (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10)))
  (message (format "Font size: %d" (face-attribute 'default :height))))

(global-set-key (kbd "M-+")      #'(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "M--")      #'(lambda nil (interactive) (djcb-zoom -1)))

;; PATH in emacs
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (car (reverse (split-string (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))

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


;; ERC scribbly scribble
(defun channel-names (channel)
  "Get a list of niknames for a particular channel."
  (when (erc-channel-p channel)
    (with-current-buffer channel
      (erc-get-channel-nickname-list))))


(defun intersect-lists (head &rest lists)
  "Intersect any number of lists."
  (if (consp lists)
      (let ((operand (car lists))
	    (rest (cdr lists)))
	(apply 'intersect-lists (intersection head operand) rest))
    head))

(defun erc-intersect-channels (head &rest channels)
  "Get a list of the nicknames that are on all channels"
  (apply 'intersect-lists (channel-names head) (mapcar 'channel-names channels)))

(defun erc-common-nicks ()
  "Interactively get the names of the common nicknames of two
channels in a tmp buffer."
  (interactive)
  (let*
      ((c1 (completing-read "First channel: "
			    (mapcar 'buffer-name (erc-channel-list nil))))
       (c2 (completing-read "Second channel: "
			    (delete c1 (mapcar 'buffer-name (erc-channel-list nil))))))
    (with-output-to-temp-buffer
	(format "*Erc users itersect:%s|%s*" (delete ?# c1) (delete ?# c2))
      (mapcar (lambda (c) (princ (format "%s\n" c)))
	      (erc-intersect-channels c1 c2)))))

(add-hook 'erc-mode-hook '(lambda() (set (make-local-variable 'global-hl-line-mode) nil)))
(add-hook 'term-mode-hook '(lambda() (set (make-local-variable 'global-hl-line-mode) nil)))

;; Elisp
(define-key emacs-lisp-mode-map "\C-c\C-e" 'eval-buffer)


;; Desktop
(require 'desktop)
(desktop-save-mode t)
(setq desktop-buffers-not-to-save
      (concat "\\("
	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	      "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(setq make-backup-files nil)

(global-set-key "\C-cdc" (lambda nil (interactive) (when (y-or-n-p "Really kill all buffers?") (desktop-clear))))

;; CC-MODE
(defun my-cc-newline-and-indent ()
  "Append a newline first if the cursor is between { and }."
  (interactive)
  (when (and (not (nth 8 (syntax-ppss)))
	     (looking-back "{\s*")
	     (looking-at "\s*}"))
    (save-excursion
      (newline)
      (indent-according-to-mode)))
  (newline-and-indent))

(defun fakedrake-cc-mode-init ()
  "Just some initializations I need for C"
  (define-key c-mode-base-map (kbd "C-M-n") 'c-end-of-defun)
  (define-key c-mode-base-map (kbd "C-M-p") 'c-beginning-of-defun)
  (define-key c-mode-base-map (kbd "M-n") 'c-end-of-statement)
  (define-key c-mode-base-map (kbd "M-p") 'c-beginning-of-statement)
  (define-key c-mode-base-map (kbd "C-j") 'my-cc-newline-and-indent)
  (define-key c-mode-base-map (kbd "C-c r") 'recompile)
  (setq c-default-style "linux" c-basic-offset 4))

(setq compilation-scroll-output t)
(add-hook 'c-mode-common-hook 'fakedrake-cc-mode-init)

;; Prolog
(add-to-list 'auto-mode-alist '("*.pl$" . prolog-mode))
(setq prolog-system 'gnu)

;; (defadvice gist-region (around su/advice/gist/gist-region/around/dirty-hack
;; 			       a c pre)
;;   "Dirty hack to prevent gist-region from choking on buffers which contain
;; `%' character"
;;   (save-window-excursion
;;     (let* ((delete-old-versions t)
;; 	   (dummy "foo")
;; 	   (beg (ad-get-arg 0))
;; 	   (end (ad-get-arg 1))
;; 	   (min-beg-end (min beg end))
;; 	   (original-text (buffer-substring beg end))
;; 	   gistid buf proc)
;;       (kill-region beg end)
;;       (insert-for-yank-1 dummy)
;;       (ad-set-arg 0 min-beg-end)
;;       (ad-set-arg 1 (point))
;;       ad-do-it
;;       (sleep-for 0.5) ;; deep magic
;;       (dolist (buf (buffer-list))
;; 	(when (string-match "^\\s-\\*http api\\.github\\.com:443\\*$" (buffer-name
;; 								       buf))
;; 	  (setq proc (get-buffer-process buf))
;; 	  (when proc (kill-process proc))
;; 	  ))
;;       (delete-region min-beg-end (point))
;;       (insert-for-yank-1 original-text)
;;       (setq gistid (car (last (split-string (car kill-ring)
;; 					    "/"))))
;;       (setq buf (gist-fetch gistid))
;;       (with-current-buffer buf
;; 	(delete-region (point-min)
;; 		       (point-max))
;; 	(insert-for-yank-1 original-text)
;; 	(gist-mode-save-buffer)
;; 	(kill-buffer))

;;       )))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(py-shell-name "ipython")
 '(safe-local-variable-values (quote ((gtags-rootdir . "~/Projects/linux-3.7-rc8/") (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'set-goal-column 'disabled nil)


(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (with-temp-buffer
	(insert filename)
	(clipboard-kill-region (point-min) (point-max)))
      (message (format "Copied '%s'" filename)))))

(global-set-key (kbd "C-x y") 'my-put-file-name-on-clipboard)

(setq ediff-split-window-function 'split-window-horizontally)

(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-to-list 'ido-ignore-buffers ".*-preprocessed\*")
(require 'vimperator-mode)
(add-to-list 'auto-mode-alist '(".*.vimperatorrc$" . vimperator-mode))

;; Slime
(require 'nrepl)

;; Aspell
(setq ispell-program-name "aspell")
