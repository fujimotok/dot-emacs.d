;; font 
(set-frame-font "ricty discord-13.5")

;; shellの環境設定
(exec-path-from-shell-initialize)

;; 関数名表示
(which-function-mode)

(setq mode-line-misc-info (delete (assoc 'which-func-mode
					 mode-line-misc-info) mode-line-misc-info)
      which-func-header-line-format '(which-func-mode ("" which-func-format)))
(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (setq mode-line-misc-info (delete (assoc 'which-func-mode
                                          mode-line-misc-info) mode-line-misc-info)
          header-line-format which-func-header-line-format)))

(require 'magit)

;; コントロールシーケンスを利用した色指定が使えるように
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)

(add-hook 'shell-mode-hook
          '(lambda ()
             ;; zsh のヒストリファイル名を設定
             (setq comint-input-ring-file-name "~/.histfile")
             ;; ヒストリの最大数
             (setq comint-input-ring-size 1024)
             ;; 既存の zsh ヒストリファイルを読み込み
             (comint-read-input-ring t)
             ;; zsh like completion (history-beginning-search)
             (local-set-key "\M-p" 'comint-previous-matching-input-from-input)
             (local-set-key "\M-n" 'comint-next-matching-input-from-input)
             ;; 色の設定
             (setq ansi-color-names-vector
                   ["#000000"           ; black
                    "#ff6565"           ; red
                    "#93d44f"           ; green
                    "#eab93d"           ; yellow
                    "#204a87"           ; blue
                    "#ce5c00"           ; magenta
                    "#89b6e2"           ; cyan
                    "#ffffff"]          ; white
                   )
             (ansi-color-for-comint-mode-on)
             )
          )
