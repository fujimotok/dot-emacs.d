(when (require 'ivy nil t)
  (setq ivy-height 40) ;; minibufferのサイズを拡大！（重要）
  (require 'ivy-hydra)  ;; M-o を ivy-dispatching-done-hydra に割り当てる．
  (setq ivy-use-virtual-buffers t) ;; `ivy-switch-buffer' (C-x b) のリストに recent files と bookmark を含める．
  ;; ミニバッファでコマンド発行を認める
  (when (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode 1)) ;; 何回層入ったかプロンプトに表示．
  ;; ESC連打でミニバッファを閉じる
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  ;; アクティベート
  (ivy-mode 1))

(when (require 'counsel nil t)

  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-x")   'counsel-M-x)
  (global-set-key (kbd "C-M-z") 'counsel-fzf)
  (global-set-key (kbd "C-M-r") 'counsel-recentf)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-M-f") 'counsel-rg)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../"))) ;; find-fileでaction発行を除外
  (define-key counsel-find-file-map (kbd "C-l") 'counsel-up-directory) ;; find-file C-lで上のdir
  (define-key counsel-find-file-map (kbd "<tab>") 'ivy-alt-done) ;; find-file tab一発で補完
  (setq ivy-on-del-error-function #'ignore) ;; ivy中にBSでivyから抜けないようにする
  ;; アクティベート
  (counsel-mode 1))

(when (require 'swiper nil t)
  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point))

(custom-set-faces
 '(ivy-current-match
   ((((class color) (background light))
     :background "#FFF3F3" :distant-foreground "#000000")
    (((class color) (background dark))
     :background "#404040" :distant-foreground "#abb2bf")))
 '(ivy-minibuffer-match-face-1
   ((((class color) (background light)) :foreground "#666666")
    (((class color) (background dark)) :foreground "#999999")))
 '(ivy-minibuffer-match-face-2
   ((((class color) (background light)) :foreground "#c03333" :underline t)
    (((class color) (background dark)) :foreground "#e04444" :underline t)))
 '(ivy-minibuffer-match-face-3
   ((((class color) (background light)) :foreground "#8585ff" :underline t)
    (((class color) (background dark)) :foreground "#7777ff" :underline t)))
 '(ivsy-minibuffer-match-face-4
   ((((class color) (background light)) :foreground "#439943" :underline t)
    (((class color) (background dark)) :foreground "#33bb33" :underline t))))

;; ?
(setq ivy-tab-space t)

(when (require 'all-the-icons-ivy nil t)
  (dolist (command '(counsel-projectile-switch-project
                     counsel-ibuffer))
    (add-to-list 'all-the-icons-ivy-buffer-commands command))
  (all-the-icons-ivy-setup))

(when (require 'ivy-rich nil t)
  (ivy-rich-mode 1))

(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

