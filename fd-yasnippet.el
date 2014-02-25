;; YASnippet

(defun fd-prev-match (rx alt &optional grp)
  "Get the last occurance or alt. GRP tells us which rx group to
keep, defaults to 1."
  (or
   (progn (re-search-backward rx nil t) (match-string-no-properties (or grp 1)))
   alt))

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (my-expand-path "my-snippets/"))
(yas-global-mode 1)

(global-set-key (kbd "C-h y") 'yas-describe-tables)
(global-set-key (kbd "<backtab>") 'yas-expand) ; S-Tab
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(defun fd-snippet-python-arguments (text)
  (let* ((targs (replace-regexp-in-string "\\*[^,]" "" text))
	 (args (split-string targs
			     "[[:space:]]*\\(=.*\\)?[[:space:]]*,[[:space:]]*" t)))
    args))


(defun re-find-all (regex string &optional group start)
  (save-match-data
    (when (string-match regex string start)
      (cons (match-string (or group 0) string)
	    (re-find-all regex string group (match-end (or group 0)))))))

(defun fd-yas-python-methodargs ()
  "This dumb functions sees the arguments of the python function
we are in and returns a list of arguments and kwargs (:args (arg1
arg2) :kwargs ((kw1 . val1))). It does not parse python code, it
just matches regexes expect things to go south if default values
are lists, sets, strings with parens and commas or anything that
looks like argument list delim but is context sensitive."
  (save-match-data
    (save-excursion
      (let* ((kwargs-rx "[,(][[:space:]]*\\([a-zA-Z0-9_]+\\)[[:space:]]*=[[:space:]]*\\(.*?\\)[,)]")
	     (args-rx "[,(][[:space:]]*\\([a-zA-Z0-9_]+\\)[[:space:]]*[,)]")
	     (sargs (if (re-search-backward
			 "def .*\\((\\(self[[:space:]]*,\\)?[[:space:]]*.*)\\):" nil t)
			(match-string-no-properties 1)
		      (error "Function not found.")))
	     (args (re-find-all args-rx sargs 1))
	     (kwargs (mapcar* 'cons
			      (re-find-all kwargs-rx sargs 1)
			      (re-find-all kwargs-rx sargs 2))))
	(list :args (remove-if (lambda (x) (string= x "self")) args) :kwargs kwargs)))))

(defun  yas-sphinx-docstring (fnargs-plist &optional offset)
  "Gets a plust like the one fd-yas-python-methodargs and maybe
an offset for pspaces and returns a sphinx readable template for
sphinx describing the arguments."
  (let* ((spaces (make-string (or offset 0) (string-to-char " ")))
	 (args (mapconcat (lambda (p) (format "%s:param %s: " spaces p))
			  (plist-get fnargs-plist :args) "\n"))
	 (kwargs (mapconcat (lambda (p) (format "%s:param %s: "
						spaces (car p)))
			    (plist-get fnargs-plist :kwargs) "\n")))
    (format "%s%s:returns: "
	    (if (string= args "") "" (concat args "\n"))
	    (if (string= kwargs "") "" (concat kwargs "\n")))))

(provide 'fd-yasnippet)
