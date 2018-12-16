
;; shellの環境設定
(exec-path-from-shell-initialize)

(require 'ssh)
(add-hook 'ssh-mode-hook
	  (lambda ()
	    (setq ssh-directory-tracking-mode t)
	    (shell-dirtrack-mode t)
	    (setq dirtrackp nil)))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename)))
    (message "Opening %s..." file)
    (call-process "open" nil 0 nil file)
    ;;(call-process "gnome-open" nil 0 nil file)
    (message "Opening %s done" file)))

;;(define-key dired-mode-map "o" 'dired-open-file)
