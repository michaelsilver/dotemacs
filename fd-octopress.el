;; OCTOPRESS

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

;; XXX: change all these into overridable defuns
(defun octopress-publishing-dir ()
  "This is the subdir where the published html of octopress is."
  (expand-file-name
   (or octopress-publishing-dir (format "%s/source/" octopress-root))))

(defun octopress-org-posts-dir ()
  "Octopress org posts dir"
  (expand-file-name
   (or octopress-org-posts-dir (format "%s/org_posts/" (octopress-publishing-dir)))))

(defun octopress-posts-dir ()
  "Get the octopress markdown posts dir"
  (expand-file-name
   (or octopress-posts-dir (format "%s/_posts/" (octopress-publishing-dir)))))

(defun octopress-themes-dir ()
  "Get octopress themes dir"
  (expand-file-name
   (or octopress-themes-dir (format "%s/.themes/" octopress-root))))

(require 'fd-cookbook)

(require 'git-emacs)
(require 'ox-publish)
(require 'ox-md)

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

;; Personal config
(defun octopress-interactive-command (commands &optional no-compile)
  "Run command or list of commands. Non-nil no-compile means run
as async-shell. Use single quotes only in command."
  (let* ((cmds (if (stringp commands) (list commands) commands))
	 (cmd (read-string "Run command like so: "
			   (format "bash -c \"cd %s && %s\"" octopress-root
				   (mapconcat (lambda (x) (format "bundle exec %s" x)) cmds " && ")))))
    (if no-compile
	(async-shell-command cmd)
      (compile cmd t))))

(defun octopress-preview ()
  "Run the octopress server to serve the current site locally."
  (interactive)
  (octopress-interactive-command "rake preview" t))

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

(defun octopress-open-created-post ()
  "This is a helper run after rake has created out post to tidy
things up."
  (interactive)
  (unless (file-accessible-directory-p (octopress-org-posts-dir))
    (make-directory (octopress-org-posts-dir)))

  (let*
      ((files (directory-files (octopress-posts-dir) nil ".*\.org$"))
       (org-filename (if (> (length files) 1)
			 (completing-read "Many candidates. Which is correct: " files)
		       (car files)))
       (correct-file (concat (octopress-org-posts-dir) "/" org-filename))
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
  (octopress-interactive-command (format "rake new_post['%s']" title))
  (message "Once compilation finishes do M-x open-created-post. To tidy up and open the post."))

(defun octopress-activate-theme (theme)
  "Activate a theme. Themes are considered any files in
`octopress-themes-dir'. This runs asyncronous interactive
commands."
  (interactive (list
		(completing-read "Theme: "
				 (delete-if
				  (lambda (s) (string/starts-with s "."))
				  (directory-files (octopress-themes-dir))))))
  (octopress-interactive-command (list (format "rake install['%s']" theme) "rake generate")))

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
