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
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (nyan-mode telephone-line switch-buffer-functions ace-jump-mode json-mode yafolding arduino-mode ssh popwin helm-rtags rtags dracula-theme powerline async avy dash epl f git-commit helm-core pkg-info s with-editor magit-find-file flycheck magit ace-link google-translate init-loader helm helm-gtags navi2ch markdown-mode git ggtags exec-path-from-shell auto-complete)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
 '(menu-bar-mode nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

