;; OCTOPRESS

(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(defun string/starts-with (s arg)
  "returns non-nil if string S starts with ARG.  Else nil."
  (cond ((>= (length s) (length arg))
	 (string-equal (substring s 0 (length arg)) arg))
	(t nil)))

(require 'git-emacs)
(require 'ox-publish)
(require 'ox-md)

;; Define octopress backend
(org-export-define-derived-backend 'octopress 'md
  :translate-alist '((src-block . org-octopress-code-block)))


(defun org-octopress-code-block (src-block contents info)
  "Transcode CODE_BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "``` %s %s\n%s\n```"
	  (org-element-property :language src-block)
	  (or (org-element-property :name src-block) "Code")
	  (org-element-property :value src-block)))

(defun org-octopress-publish-to-octopress (plist filename pub-dir)
  "Publish an org file to Markdown.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'octopress filename ".markdown" plist pub-dir))

;; Personal config
(defun fd-interactive-octopress-command (command &optional no-compile)
  "Use single quotes only in command."
  (let ((cmd (read-string "Run command like so: "
			  (format "bash -c \"cd %s && bundle exec %s\"" fd-octopress-root command))))
    (if no-compile
	(async-shell-command cmd)
      (compile cmd t))))

(defun org-octopress-preview ()
  (interactive)
  (fd-interactive-octopress-command "rake preview" t))

(setq fd-org-commit-on-export nil)
(defun org-octopress-save-then-publish ()
  (interactive)
  (unless (and (string/starts-with (buffer-file-name)
				   (expand-file-name fd-org-posts))
	       (eq 'org-mode major-mode))
    (error "Not an octopress post."))

  (save-buffer)
  (unless  (or (not fd-org-commit-on-export)
	       (eq 'uptodate (git--status-file (buffer-file-name))))
    (git-commit-file))
  (org-save-all-org-buffers)
  (org-publish-current-project)
  (fd-interactive-octopress-command "rake generate")
  (message "Published."))

(defun org-octopress-upload ()
  (interactive)
  (fd-interactive-octopress-command "rake deploy"))

;; Just change fd-octopress-root to point to the right place
(setq fd-octopress-root "~/Projects/fakedrake.github.com/"
      fd-publishing-dir (format "%s/source/" fd-octopress-root)
      fd-org-posts (format "%s/org_posts/" fd-publishing-dir)
      fd-posts (format "%s/_posts/" fd-publishing-dir))

(setq org-publish-project-alist
      (list (cons "blog-org"  (list :base-directory fd-org-posts
				    :base-extension "org"
				    :publishing-directory fd-posts
				    :sub-superscript ""
				    :recursive t
				    :publishing-function 'org-octopress-publish-to-octopress
				    :headline-levels 4
				    :markdown-extension "markdown"
				    :octopress-extension "markdown"
				    :body-only t))
	    (cons "blog-extra" (list :base-directory fd-org-posts
				     :publishing-directory fd-publishing-dir
				     :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|svg"
				     :publishing-function 'org-ox-publish-attachment
				     :recursive t
				     :author nil
				     ))
	    (cons "blog" (list :components (list "blog-org" "blog-extra")))))

(defun fd-open-created-post ()
  "This is a helper run after rake has created out post to tidy
things up."
  (interactive)
  (unless (file-accessible-directory-p fd-org-posts)
    (make-directory fd-org-posts))

  (let*
      ((files (directory-files fd-posts nil ".*\.org$"))
       (org-filename (if (> (length files) 1)
			 (completing-read "Many candidates. Which is correct: " files)
		       (car files)))
       (correct-file (concat fd-org-posts "/" org-filename))
       (generated-file (concat fd-posts "/" org-filename)))

    (unless (file-exists-p generated-file)
      (error "No generated file was found. Do M-x fd-org-new-octopress to generate something and try again."))

    (if (file-exists-p correct-file)
	(message "You already have the generated file.")
      (rename-file generated-file correct-file))

    (shell-command (format "cd %s && git add %s" fd-org-posts org-filename))
    (find-file (concat fd-posts "/" org-filename))
    (message "Publish with M-x save-then-publish.")))

(defun fd-org-new-octopress-post (title)
  "Create a new octopress post."
  (interactive "MPost tite: ")

  ;; Create the org files
  (fd-interactive-octopress-command (format "rake new_post['%s']" title))
  (message "Once compilation finishes do M-x fd-open-created-post. To tidy up and open the post."))

(provide 'fd-octopress)
