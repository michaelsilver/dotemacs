(defun codebender-log-action-codes ()
  (interactive)
  (find-file "/Users/drninjabatman/Projects/Codebendercc/UserResearch/README.md"))

(defun codebender-gregorian-day-diff (days &optional date)
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian
       (or date (calendar-current-date)))
      days)))

;; (defvar codebender-end-date '(05 15 2014))
(defvar codebender-end-date nil)
(defun codebender-url-browse-user (user &optional days)
  (interactive "sInsert user name to browse in metrics: ")
  (let ((d (codebender-gregorian-day-diff -8 codebender-end-date))
	(now (codebender-gregorian-day-diff 0 codebender-end-date)))
    (browse-url
     (format
      "http://metrics.codebender.cc/index.php/user?user=%s&startingDate=%d-%02d-%02d&endingDate=%d-%02d-%02d"
      user
      (calendar-extract-year d)
      (calendar-extract-month d)
      (calendar-extract-day d)
      (calendar-extract-year now)
      (calendar-extract-month now)
      (calendar-extract-day now)))))

(provide 'fd-codebender)
