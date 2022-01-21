;;; early-init.el --- Emacs configuration file before showing.
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Speed up startup

;; デフォルト値をバックアップ
(defvar emacs-default-file-name-handler-alist file-name-handler-alist)

;; Magic File Name を無効に
(setq file-name-handler-alist
      nil)

;; GCの閾値を最大にしてGCを発生させない
(setq gc-cons-threshold
      most-positive-fixnum)

;; init.el読み終わったら上記設定を元に戻す
(add-hook
 'emacs-startup-hook
 (lambda ()
   "Restore defalut values after startup."
   (setq file-name-handler-alist
         emacs-default-file-name-handler-alist)
   (setq gc-cons-threshold
         12800000)))


;; 最初に背景白、文字黒の画面が出るのを回避。doom-draculaの設定を引用
(custom-set-faces
 '(default
    ((t
      (:background "#282a36"
                   :foreground "#f8f8f2")))))

;; ウィンドウのスタイル指定
(push
 '(menu-bar-lines . 0)
 default-frame-alist)
(push
 '(tool-bar-lines . 0)
 default-frame-alist)
(push
 '(vertical-scroll-bars . nil)
 default-frame-alist)
(push
 '(horizontal-scroll-bars . nil)
 default-frame-alist)

;; スクリーンの最大化
;; (set-frame-parameter nil 'fullscreen 'maximized) for wsl?
;; (toggle-frame-maximized)

;; フォント表示改善など
(setq frame-inhibit-implied-resize t)
(setq inhibit-compacting-font-caches t)

;; 初期表示を空のscratchに変更
(setq initial-scratch-message "")
(setq inhibit-startup-screen t)

