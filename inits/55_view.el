;; font 
(set-frame-font "ricty diminished-10.5")

(setq default-input-method "japanese-mozc")
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
;; theme set
(load-theme 'dracula t)

;; 関数名表示
(which-function-mode)

(setq mode-line-format (delete (assoc 'which-func-mode
                                      mode-line-format) mode-line-format)
      which-func-header-line-format '(which-func-mode ("" which-func-format)))
(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (setq mode-line-format (delete (assoc 'which-func-mode
                                          mode-line-format) mode-line-format)
          header-line-format which-func-header-line-format)))

(defun show-file-name ()
  (interactive)
  (kill-new (buffer-file-name))
  (message "add kill ring: %s" (buffer-file-name)) )

(defun show-func-name ()
  (interactive)
  (kill-new (buffer-file-name))
  (message "add kill ring: %s" (which-function)) )

;;
(setq-default truncate-lines t)

;;
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally)
    (pop-to-buffer nil))
  (other-window 1))

(global-set-key (kbd "C-t") 'other-window-or-split)

;; high light
(global-hl-line-mode t)                   ;; 現在行をハイライト
(show-paren-mode t)                       ;; 対応する括弧をハイライト
(setq show-paren-style 'mixed)            ;; 括弧のハイライトの設定。
(transient-mark-mode t)                   ;; 選択範囲をハイライト

;; pop
(setq helm-display-function #'display-buffer)
(when (require 'popwin)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config
	'(("*complitation*" :noselect t)
	  ("helm" :regexp t :height 0.4))))

;; タイトルバーに時計
(when (window-system)
  ;; display-timeより先にsetしておかないとdefaultの書式になる
  (setq display-time-string-forms
	'((format "%s/%s/%s" year month day)
	  (format "(%s:%s)" 24-hours minutes)))
  (display-time) ;; display-time-stringの有効化
  ;; タイトルバーの書式設定 global-mode-stringにdisplay-time-stringが入っている
  ;; バッファがファイルのときはフルパス、でなければバッファ名表示
  ;; if(buffer-file-name) の評価がsetq時で終わらないよう:eval
  ;;(setq battery-update-interval 1)
  ;;(setq display-time-interval 1)
  (display-battery-mode 1)
  ;; Either BATn or yeeloong-bat, basically.
  (setq battery-mode-line-format " %b%p%%")
  (setq frame-title-format '("" (:eval (if (buffer-file-name) " %f" " %b"))
			     " --- " global-mode-string) ) )
