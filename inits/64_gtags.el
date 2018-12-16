(add-hook 'c-mode-hook 'helm-gtags-mode)

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
              (local-set-key (kbd "C-j") 'helm-gtags-dwim-ex)
              (local-set-key (kbd "C-c t") 'helm-gtags-find-tag)
              (local-set-key (kbd "C-c r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "C-c s") 'helm-gtags-find-symbol)
	      (local-set-key (kbd "C-c h") 'helm-gtags-find-header-or-source)
              (local-set-key (kbd "C-c b") 'helm-gtags-pop-stack)))

(defun helm-gtags-dwim-ex ()
  (interactive)
  (if (bounds-of-thing-at-point 'word)
      (helm-gtags-dwim)
      (helm-gtags-pop-stack)))

(defun helm-gtags-find-header-or-source ()
  (interactive)
  (let ( (prefix (nth 0 (split-string (buffer-name) "\\.")))
	 (suffix (nth 1 (split-string (buffer-name) "\\.")))
	 (buffer (buffer-name))
 	 (target "") )
    (progn
      (cond ((string= suffix "c")   (setq target ".h"))
	    ((string= suffix "cpp") (setq target ".h"))
	    ((string= suffix "cxx") (setq target ".h"))
	    ((string= suffix "h")   (setq target ".c"))
	    ((string= suffix "hpp") (setq target ".cpp"))
	    ((string= suffix "hxx") (setq target ".cxx")) )
      ;;別windowに出ない場合は以下を有効に
      ;;(other-window 1)
      ;;(switch-to-buffer buffer)
      (helm-gtags-find-files (concat prefix target))
    ) ) )

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
