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
 '(initial-frame-alist (quote ((vertical-scroll-bars))))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (ivy-rich all-the-icons-ivy company-box ivy-hydra counsel clojure-mode-extra-font-locking all-the-icons-dired all-the-icons rainbow-mode open-in-msvs telephone-line nyan-mode emmet-mode web-mode company omnisharp csharp-mode eglot lsp-mode mozc-popup mozc-im mozc switch-buffer-functions ace-jump-mode json-mode yafolding arduino-mode ssh popwin helm-rtags rtags dracula-theme powerline async avy dash epl f git-commit helm-core pkg-info s with-editor magit-find-file flycheck magit ace-link google-translate init-loader helm helm-gtags markdown-mode git ggtags exec-path-from-shell auto-complete)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivsy-minibuffer-match-face-4 ((((class color) (background light)) :foreground "#439943" :underline t) (((class color) (background dark)) :foreground "#33bb33" :underline t)))
 '(ivy-current-match ((((class color) (background light)) :background "#FFF3F3" :distant-foreground "#000000") (((class color) (background dark)) :background "#404040" :distant-foreground "#abb2bf")))
 '(ivy-minibuffer-match-face-1 ((((class color) (background light)) :foreground "#666666") (((class color) (background dark)) :foreground "#999999")))
 '(ivy-minibuffer-match-face-2 ((((class color) (background light)) :foreground "#c03333" :underline t) (((class color) (background dark)) :foreground "#e04444" :underline t)))
 '(ivy-minibuffer-match-face-3 ((((class color) (background light)) :foreground "#8585ff" :underline t) (((class color) (background dark)) :foreground "#7777ff" :underline t)))
 '(ivy-minibuffer-match-face-4 ((((class color) (background light)) :foreground "#439943" :underline t) (((class color) (background dark)) :foreground "#33bb33" :underline t))))

;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)
(toggle-frame-maximized)
