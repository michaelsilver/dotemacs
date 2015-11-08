;; OCTOPRESS
(require 'cl)

(defvar octopress-root "~/Projects/fakedrake.github.com/"
  "The root of your octopress blog.")

(defvar octopress-git-commit-on-export nil
  "Git commit on exporting of the posts.")

(defvar octopress-publishing-dir nil
  "Use this as the directory where things are published instead
  of the default.")

(defvar octopress-org-posts-dir nil
  "Use this as the dir where org posts are kept instead of the
  default.")

(defvar octopress-posts-dir nil
  "Use this as the posts directory instead of the default.")

(defvar octopress-themes-dir nil
  "Use this as the themes dir instead of the default.")

(defun proper-dir-name (dir)
  "A proper dirname without double '/' and without trailing '/'"
  (expand-file-name (replace-regexp-in-string "/+" "/" (directory-file-name dir))))

;; XXX: change all these into overridable defuns
(defun octopress-publishing-dir ()
  "This is the subdir where the published html of octopress is."
  (proper-dir-name
   (or octopress-publishing-dir (format "%s/source/" octopress-root))))

(defun octopress-org-posts-dir ()
  "Octopress org posts dir"
  (proper-dir-name
   (or octopress-org-posts-dir (format "%s/org_posts/" (octopress-publishing-dir)))))

(defun octopress-posts-dir ()
  "Get the octopress markdown posts dir"
  (proper-dir-name
   (or octopress-posts-dir (format "%s/_posts/" (octopress-publishing-dir)))))

(defun octopress-themes-dir ()
  "Get octopress themes dir"
  (proper-dir-name
   (or octopress-themes-dir (format "%s/.themes/" octopress-root))))

;; From the elisp cookbook
(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(defvar octopress-git-commit-on-export nil
  "Git commit on exporting of the posts.")

(defvar octopress-publishing-dir nil
  "Use this as the directory where things are published instead
  of the default.")

(defvar octopress-org-posts-dir nil
  "Use this as the dir where org posts are kept instead of the
  default.")

(defvar octopress-posts-dir nil
  "Use this as the posts directory instead of the default.")

(defvar octopress-themes-dir nil
  "Use this as the themes dir instead of the default.")

;; ;; XXX: change all these into overridable defuns
;; (defun octopress-publishing-dir ()
;;   "This is the subdir where the published html of octopress is."
;;   (expand-file-name
;;    (proper-dir-name
;;     (or octopress-publishing-dir (format "%s/source/" octopress-root)))))

;; (defun octopress-org-posts-dir ()
;;   "Octopress org posts dir"
;;   (expand-file-name
;;    (proper-dir-name
;;     (or octopress-org-posts-dir (format "%s/org_posts/" (octopress-publishing-dir))))))

;; (defun octopress-posts-dir ()
;;   "Get the octopress markdown posts dir"
;;   (expand-file-name
;;    (proper-dir-name
;;     (or octopress-posts-dir (format "%s/_posts/" (octopress-publishing-dir))))))

;; (defun octopress-themes-dir ()
;;   "Get octopress themes dir"
;;   (expand-file-name
;;    (proper-dir-name
;;     (or octopress-themes-dir (format "%s/.themes/" octopress-root)))))

(require 'git-emacs)
(require 'ox-publish)
(require 'ox-md)

(defmacro octopress-generic-cmd (name cmd &rest body)
  `(let ((compilation-buffer-name-function (lambda (x) (format ,name))))
     (compile ,cmd t)
     (with-current-buffer ,name
       (setq-local compilation-finish-functions
		   (list (lambda (r1 r2) ,@body))))))


(defun octopress--remove-keys (body)
  "Remove all keys from body."
  (if (null body) nil
    (let ((head (car body))
	  (rest (cdr body)))
      (if (keywordp head) (octopress--remove-keys (cdr rest))
	(cons head (octopress--remove-keys rest))))))

(defun oc--bash-cmd (cmd)
  "String that will certainly run in bash correctly."
  (format "/bin/bash -l -c \"%s\"" cmd))

(defun oc--bash-cmd-list (command)
  "Create one bash call from a list of shell commands."
  (let ((cmds (if (stringp command) (list command) command)))
    (oc--bash-cmd (format  "cd %s && %s" octopress-root
			   (mapconcat (lambda (x) (if use-bundler (format "bundle exec %s" x) x))
				      cmds " && ")))))

(defmacro* octopress-cmd (name command &rest body &key (use-bundler t) &allow-other-keys)
  "Open CMD shell command in compilation buffer NAME. When that
is finished execute body. Use :use-bundler nil to prepend bundler to all
commands."
  (let* ((clean-body (octopress--remove-keys body))
	 (rvm-ok (= 0 (shell-command (oc--bash-cmd "command -v bundle > /dev/null"))))
	 (cmd (oc--bash-cmd-list (or (and (listp command)
					  (not (functionp (car command)))
					  command)
				     (eval command)))))
    (if rvm-ok
	(macroexpand `(octopress-generic-cmd ,name ,cmd ,@body))
      `(error "There was a problem with rvm. Please install correctly."))))

;; Setup the blog

(defun octopress-clone-blog (git-repo)
  (interactive "MGit repo of your octopress: ")
  (octopress-generic-cmd "pull blog" (format "git clone %s %s" git-repo octopress-root)
			 (message "Pulled successfully")
			 (octopress-cmd :use-bundler nil "checkout source branch" "git checkout ")))

(defun octopress-github-blog (username)
  "Setup a github blog that is already in your github. Use .com
for extension. XXX: .io is also valid. Check that too."
  (interactive "MClone blog from your github repo. Your github username: ")
  (octopress-clone-blog (format "git@github.com:%s/octopress" username username)))

(defun octopress-purge-blog ()
  "Purge the local copy of the blog."
  (interactive)
  (when (yes-or-no-p "Remove local copy of the blog? ")
    (octopress-generic-cmd "purge" (format "mv %s %s.bak" octopress-root
					   (directory-file-name octopress-root))
			   (message "Remove %s by hand, I am not responsible for this shit."
				    (directory-file-name octopress-root)))))

(defun octopress-code-block (src-block contents info)
  "Transcode CODE_BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((lang (org-element-property :language src-block))
	 (name (if lang
		   (or (org-element-property :name src-block)
		       (format "%s code" (capitalize lang)))
		 ""))
	 (code (replace-regexp-in-string "^```" " ```" (org-element-property :value src-block))))
    (format "``` %s %s\n%s\n```" (or lang "") name code)))

(defun octopress-publish-to-octopress (plist filename pub-dir)
  "Publish an org file to Markdown.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'octopress filename ".markdown" plist pub-dir))

;; Deprecated n favor of `octopress-cmd'
(defun octopress-interactive-command (commands &optional no-compile)
  "Run command or list of commands. Non-nil no-compile means run
as async-shell. Use single quotes only in command."
  (let* ((cmds (if (stringp commands) (list commands) commands))
	 (cmd (read-string "Run command like so: "
			   (format "bash -cl \"cd %s && %s\"" octopress-root
				   (mapconcat (lambda (x) (format "bundle exec %s" x)) cmds " && ")))))
    (if no-compile
	(async-shell-command cmd)
      (let ((compilation-buffer-name-function (lambda (x) (format "*%s*" cmd))))
	(compile cmd t)))))

(defun octopress-preview ()
  "Run the octopress server to serve the current site locally."
  (interactive)
  (octopress-rake "preview" t))

(defun octopress-generate ()
  "Generate the octopress site."
  (interactive)
  (octopress-interactive-command "rake generate"))

(defun octopress-filename-date-update ()
  "Update the name of the current buffer according to the date
specified in metadata. Make sure the HTML block is expanded."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (search-forward "date: " nil t)
    (let* ((date (current-word))
	   (current-name (file-name-nondirectory (buffer-file-name)))
	   (new-name (concat date (substring current-name 10)))
	   (new-fname (concat (file-name-directory (buffer-file-name)) new-name)))
      (unless (equal new-name current-name)
	(rename-file (buffer-file-name) new-fname)
	(rename-buffer new-name)
	(set-visited-file-name new-fname)))))

(defun octopress-save-then-publish ()
  "Save the current org file, if the date in the filename does
not match the metadata, update the filename. Commit with
git-emacs and publish all changes you can find. Then generate the
octopress page."
  (interactive)
  (save-excursion
    (unless (and (string/starts-with (expand-file-name (buffer-file-name))
				     (octopress-org-posts-dir))
		 (eq 'org-mode major-mode))
      (error "Not an octopress post."))

    (save-buffer)
    (octopress-filename-date-update)

    ;; XXX: move this to uploading
    (unless  (or (not octopress-git-commit-on-export)
		 (eq 'uptodate (git--status-file (buffer-file-name))))
      (git-commit-file))

    (org-publish-current-project)
    (octopress-generate)
    (message "Published.")))

(defun octopress-upload ()
  "Deploy the octopress site site."
  (interactive)
  (octopress-interactive-command "rake deploy"))

;; Just change octopress-root to point to the right place
(defun file-modification-date (fpath)
  (let ((mt (nth 5 (file-attributes fpath))))
    (+ (* (car mt) 65536) (cadr mt))))

(defun octopress-org-modtime (org-file)
  "Get an integer of modification time of file."
  (file-modification-date (format "%s/%s"
				  (octopress-org-posts-dir) org-file)))

(defun octopress-org-posts ()
  "Get a list of octopress post basenames. If the dir isn't there
create it."
  (sort (directory-files (octopress-org-posts-dir) nil ".\.org$")
	(lambda (p1 p2) (< (octopress-org-modtime p1)
			   (octopress-org-modtime p2)))))

(defun octopress-open-created-post (&optional org-filename)
  "Open one of your posts."
  (interactive (list
		(completing-read "Which post should I open: "
				 (octopress-org-posts))))

  (let* ((correct-file (concat (octopress-org-posts-dir) "/" org-filename))
	 (generated-file (concat (octopress-posts-dir) "/" org-filename)))

    (unless (file-exists-p generated-file)
      (error "No generated file was found. Generate a new post and try again."))

    (if (file-exists-p correct-file)
	(message "You already have the generated file.")
      (rename-file generated-file correct-file))

    (shell-command (format "cd %s && git add %s" (octopress-org-posts-dir) org-filename))
    (find-file  correct-file)
    (message "Publish with M-x save-then-publish.")))

(defun octopress-new-post (title)
  "Create a new octopress post. This runs asyncronous interactive
commands."
  (interactive "MPost tite: ")

  ;; Create the org files
  (let ((post-cmd (format "rake new_post['%s']" title)))
    (octopress-cmd "create post" post-cmd
		   (call-interactively 'octopress-open-created-post))))

(defun octopress-activate-theme (theme)
  "Activate a theme. Themes are considered any files in
`octopress-themes-dir'. This runs asyncronous interactive
commands."
  (interactive (list
		(completing-read "Theme: "
				 (delete-if
				  (lambda (s) (string/starts-with s "."))
				  (directory-files (octopress-themes-dir))))))
  (with-cmd-finished "theme install" (format "rake install['%s']" theme)
		     (with-cmd-finished "generating" "rake generate"
					(message "%s theme is now your blog theme!"))))

(defun octopress-install-theme (git-url)
  "Given the git url, install octopress theme. No checking is
  done. This runs asyncronous interactive commands."
  (interactive "sInsert git repo url: ")
  (let ((theme-name (replace-regexp-in-string "\\.git$" "" (file-name-nondirectory git-url))))

    (octopress-interactive-command (format "git clone %s .themes/%s"))))

(defun octopress-setup ()
  "Define te derived backend and setup org-mode publishing."
  (interactive)
  (org-export-define-derived-backend 'octopress 'md
    :options-alist '((:with-toc nil "toc" nil))
    :translate-alist '((src-block . octopress-code-block)))

  (setq org-publish-project-alist
	(list (cons "blog-org"  (list :base-directory (octopress-org-posts-dir)
				      :base-extension "org"
				      :publishing-directory (octopress-posts-dir)
				      :sub-superscript ""
				      :recursive t
				      :publishing-function 'octopress-publish-to-octopress
				      :with-toc nil
				      :headline-levels 4
				      :markdown-extension "markdown"
				      :octopress-extension "markdown"
				      :body-only t))
	      (cons "blog-extra" (list :base-directory (octopress-org-posts-dir)
				       :publishing-directory (octopress-publishing-dir)
				       :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|svg"
				       :publishing-function 'org-ox-publish-attachment
				       :recursive t
				       :author nil
				       ))
	      (cons "blog" (list :components (list "blog-org" "blog-extra"))))))

(provide 'fd-octopress)
