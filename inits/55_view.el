;; font 
(set-frame-font "ricty discord-13.5")

;; theme set
(load-theme 'dracula t)

;; スクリーンの最大化
;;(custom-set-variables
;; '(initial-frame-alist (quote ((fullscreen . maximized)))))
;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)

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
(setq truncate-lines t)

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
  (display-battery-mode 1)
  (setq battery-mode-line-format " ⚡%b%p%%")
  (setq frame-title-format '("" (:eval (if (buffer-file-name) " %f" " %b"))
			     " --- " global-mode-string) ) )

;;(window-pixel-width)
;;(window-pixel-height)

;; 独自起動画面バッファ
(defun my-fancy-startup-screen (&rest args)
  (clear-image-cache)
  (let* ((pwidth (window-pixel-width))
         ;;(pheight (* pwidth 0.75))
	 (pheight (window-pixel-height))
         (file (expand-file-name "~/some-background.jpg"))
         (tfile "/tmp/emacs-splash.jpg")
         (color "white")
	 (font "/Library/Fonts/Times New Roman.ttf")
         (rt (/ pwidth 1000.0))
         (size1 (* 80.0 rt)) (size2 (* 16.0 rt)) (size3 (* 24.0 rt))
         (x1 (* 50 rt)) (y1 (- pheight (* 120 rt)))
         (x2 (* 50 rt)) (y2 (- pheight (* 60 rt)))
         (x3 (* 40 rt)) (y3 (* 40 rt))
         (qtxt (replace-regexp-in-string "\t" "    " (with-temp-buffer (call-process "fortune" nil t nil) (buffer-string))))
         (args
          (list file "-resize" (format "%dx%d" (- pwidth 1) (- pwidth 1))
                "-fill" color "-font" font
                "-pointsize" (format "%d" size1) "-annotate" (format "+%d+%d" x1 y1) "GNU Emacs"
                "-pointsize" (format "%d" size2) "-annotate" (format "+%d+%d" x2 y2) (emacs-version)
                "-pointsize" (format "%d" size3) "-annotate" (format "+%d+%d" x3 y3) qtxt
                tfile)))
    (apply 'call-process "convert" nil nil nil args)
    (let ((frame (fancy-splash-frame)))
      (save-selected-window
        (select-frame frame)
        (switch-to-buffer "*About GNU Emacs*")
        (setq buffer-undo-list t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize " " 'display (create-image tfile)) "\n\n")
          (set-buffer-modified-p nil)
          (goto-char (point-min))
          (force-mode-line-update))
        (use-local-map splash-screen-keymap)
        (setq buffer-read-only t)))))

;; fancy-about-screen 関数を乗っ取る
;;(advice-add 'fancy-about-screen :around 'my-fancy-startup-screen)
;;(advice-add 'fancy-startup-screen :around 'my-fancy-startup-screen)

;; 乗っ取り解除用
;; (advice-remove 'fancy-about-screen 'my-fancy-startup-screen)

