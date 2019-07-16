;;; デフォルトの設定(参考)
;; (defun ace-link-setup-default ()
;;   "Setup the defualt shortcuts."
;;   (require 'info)
;;   (define-key Info-mode-map "o" 'ace-link-info)
;;   (require 'help-mode)
;;   (define-key help-mode-map "o" 'ace-link-help)
;;   (require 'eww)
;;   (define-key eww-link-keymap "o" 'ace-link-eww)
;;   (define-key eww-mode-map "o" 'ace-link-eww))
(ace-link-setup-default)
(require 'org)
(define-key org-mode-map (kbd "C-c M-o") 'ace-link-org)

;(require 'eww)
;(define-key eww-link-kyemap (kbd "e") 'ace-link-eww)
;(define-key eww-mode-map (kbd "e") 'ace-link-eww)

;;; text-property-any -> next-single-property-change
;;; にしないと一部のリンクが辿れないので再定義
(defun ali--eww-collect-references ()
  "Collect the positions of visible links in the current `eww' buffer."
  (save-excursion
    (save-restriction
      (narrow-to-region
       (window-start)
       (window-end))
      (goto-char (point-min))
      (let ((skip (next-single-property-change (point) 'help-echo))
            candidates)
        (while (setq skip (text-property-not-all
                           skip (point-max) 'help-echo nil))
          (goto-char skip)
          (push skip candidates)
          (setq skip (next-single-property-change (point) 'help-echo)))
        (nreverse candidates)))))

;;; xwidget-webkit
(add-hook 'xwidget-webkit-mode-hook 'xwidget-webkit-mode-hook-func)
(defun xwidget-webkit-mode-hook-func ()
  (remove-hook 'kill-buffer-query-functions #'xwidget-kill-buffer-query-function)
  (local-set-key (kbd "C-f") 'xwidget-webkit-forward)
  (local-set-key (kbd "C-b") 'xwidget-webkit-back)
  (local-set-key (kbd "C-r") 'xwidget-webkit-reload)
  (local-set-key (kbd "C-l") 'xwidget-webkit-browse-url)
  (local-set-key (kbd "C-w") 'xwidget-webkit-current-url-message-kill)
  (local-set-key (kbd "C-q") 'kill-current-buffer)
  (local-unset-key (kbd "C-x 2"))
  (local-unset-key (kbd "C-x 3"))
  )

(defvar xwidget-webkit-search-word-prefix
  "https://duckduckgo.com/?q="
  "Search engine url and search request that before searching word.")
(defvar xwidget-webkit-search-word-suffix
  "&kp=-1&kl=jp-jp"
  "Search engine option parameter that after searching word.")
(defun xwidget-webkit-search-word (arg word)
  "Searching from `xwidget-webkit-search-word-prefix' search engine.
And searching with `xwidget-webkit-search-word-suffix' search engine options.
Interactively, WORD defaults to the string looking like a word around point.
If setting prefix args (C-u), reuses session(buffer). Normaly session(buffer) create."
  (interactive (list current-prefix-arg
		     (read-string "search-word: " (cond ((thing-at-point 'word)) (t "")) )))
  (let ( (encoded-url
	  (url-encode-url (concat xwidget-webkit-search-word-prefix
				  word xwidget-webkit-search-word-suffix))) )
    (xwidget-webkit-browse-url encoded-url (not arg))
    )
  )

;; (defun switch-buffer-functions-func (prev cur)
;;   (if (string= "xwidget-webkit-mode"
;;   	       (buffer-local-value 'major-mode
;; 				   (get-buffer (or prev "*scratch*"))))
;;     ))
;; (add-hook 'switch-buffer-functions #'switch-buffer-functions-func)
