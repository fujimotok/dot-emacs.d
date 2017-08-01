(require 'package)

;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; MELPA-stableを追加
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; 初期化
(package-initialize)

;; init loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/inits/")

;; 表示設定 GUIからのauto gen.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (renegade)))
 '(custom-safe-themes
   (quote
    ("2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da" "daeaa8249f0c275de9e32ed822e82ff40457dabe07347fe06afc67d962a3b1e9" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" default)))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (async avy dash epl f git-commit helm-core magit-popup pkg-info popup s with-editor magit-find-file flycheck magit ace-link google-translate init-loader helm helm-gtags navi2ch markdown-mode git ggtags exec-path-from-shell color-theme-modern auto-complete)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
 '(menu-bar-mode nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)

;; magit に path 引き継ぎ
(require 'magit)
(exec-path-from-shell-initialize)

;;
;; Auto Complete
;;
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ
