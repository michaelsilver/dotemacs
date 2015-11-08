(require 'js2-mode)

(defun my-js-newline-and-indent ()
  "Append a newline first if the cursor is between { and }."
  (interactive)
  (when (and (not (nth 8 (syntax-ppss)))
	     (looking-back "{\s*")
	     (looking-at "\s*}"))
    (save-excursion
      (newline)
      (indent-according-to-mode)))
  (newline-and-indent))

(defun fd-js-mode-hook ()
  "Hooks for all clojure."
  (setq js2-basic-offset 2)
  (define-key js2-mode-map (kbd "C-j") 'my-js-newline-and-indent)
  (setq-local untabify-on-save t))
(require 'compile)
(defvar js-compilation-error-regex-alist
  ;; REGEX FILE-GROUP LINE-GROUP COLUMN-GROUP ERROR-TYPE LINK-GROUP
  '(
    ;; When selenium runs pipe it through `sed
    ;; 's_http://localhost:8080_$(CURDIR)_g'`
    (my-selenium
     "^BrowserLog: \\(\\(.*\.js\\) \\([0-9]*\\):\\([0-9]*\\)\\) "
     2 3 4 '(1) 1)

    (nodejs
     "^[ \\t]*at.* (?\\(file://\\)?\\(\\(.*?\\):\\([0-9]*\\)\\(:\\([0-9]*\\)\\)?\\))?$"
     3 4 6 nil 2)
    (mocha
     "^\\(\\(/.*\\):\\([0-9]*\\)\\)$"
     2 3 nil nil  1)
    (jasmine
     "^[ \\t]*\\(file://\\(.*\\):\\([0-9]*\\)\\)$"
     2 3 nil nil  1)
    (browserify
     "Error: .*?\\(\\(/.*\\): Line \\([0-9]*\\)\\)"
     2 3 nil nil 1)))

(setq compilation-error-regexp-alist
      (append (mapcar 'car js-compilation-error-regex-alist)
	      (or compilation-error-regexp-alist nil)))

(setq compilation-error-regexp-alist-alist
      (append js-compilation-error-regex-alist
	      compilation-error-regexp-alist-alist))

(add-hook 'js2-mode-hook 'fd-js-mode-hook)

(defvar html-beatify-cmd "js-beautify --type html -s 2 -a -p -f -")
(defun html-beautify-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end)
			   html-beatify-cmd nil t))

(defvar js2-beatify-cmd "js-beautify -s 2 -a -p -f -")
(defun js2-beautify-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end)
			   js2-beatify-cmd nil t))

(defun top-level-functions ()
  (when (search-forward-regexp "^function\s+\\([a-zA-Z0-9_]+\\)" nil t)
    (cons (match-string 1) (top-level-functions))))

(defun js2-export-top-functions ()
  (interactive)
  (mapcar (lambda (f) (insert (format "module.exports.%s = %s;\n" f f)))
          (save-excursion
            (beginning-of-buffer)
            (top-level-functions))))

(defun js2-test-this-file ()
  (interactive)
  (compilation-start (format "mocha %s" (buffer-file-name))))

(require 'ffap)
(defun js2-relative-path (&optional decorate-string)
  "Change the path of the file at point to relative"
  (interactive)
  (let* ((fname (ffap-string-at-point))
         (rg ffap-string-at-point-region)
         (start (car rg))
         (end (cadr rg)))
    (when (> end start)
      (delete-region start end)
      (insert (funcall (or decorate-string 'identity)
                       (file-relative-name fname)))
      (goto-char start))))

(defun js2-require-abs-file ()
  "Require the filename at point."
  (interactive)
  (js2-relative-path
   (lambda (rel-fname)
     (format "require('%s');" rel-fname))))

(provide 'fd-javascript)
