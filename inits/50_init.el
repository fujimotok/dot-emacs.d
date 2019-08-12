;;(global-set-key [mouse-wheel-up-event]  'text-scale-increase)
;;(global-set-key  [mouse-wheel-down-event] 'text-scale-decrease)

(global-set-key [C-wheel-up] 'text-scale-increase)
(global-set-key [C-wheel-down] 'text-scale-decrease)

;; shellの環境設定
(when (not (eq system-type 'windows-nt))
(exec-path-from-shell-initialize))

(require 'ssh)
(add-hook 'ssh-mode-hook
	  (lambda ()
	    (setq ssh-directory-tracking-mode t)
	    (shell-dirtrack-mode t)
	    (setq dirtrackp nil)))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename)))
    (message "Opening %s..." file)
    (call-process "open" nil 0 nil file)
    ;;(call-process "gnome-open" nil 0 nil file)
    (message "Opening %s done" file)))

(defun move-beginning-alt ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
      (beginning-of-line)))

(define-key global-map (kbd "C-a") 'move-beginning-alt)
(define-key global-map (kbd "C-S-f") 'forward-word)
(define-key global-map (kbd "C-S-b") 'backward-word)

;;(set-default-coding-systems 'utf-8-dos)
(set-default-coding-systems 'utf-8-unix)
;;(define-key dired-mode-map "o" 'dired-open-file)

(with-eval-after-load 'company
      (setq company-auto-expand t) ;; 1個目を自動的に補完
      (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
      (setq company-idle-delay 0) ; 遅延なしにすぐ表示
      (setq company-minimum-prefix-length 2) ; デフォルトは4
      (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
      (setq completion-ignore-case t)
      (setq company-dabbrev-downcase nil)
      (global-set-key (kbd "TAB") 'company-indent-or-complete-common)
      ;; C-n, C-pで補完候補を次/前の候補を選択
      (define-key company-active-map (kbd "C-n") 'company-select-next)
      (define-key company-active-map (kbd "C-p") 'company-select-previous)
      ;;(define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
      (define-key company-active-map (kbd "C-h") nil) ;; C-hはバックスペース割当のため無効化
      (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer) ;; ドキュメント表示はC-Shift-h

      ;; 未選択項目
      (set-face-attribute 'company-tooltip nil
                  :foreground "#36c6b0" :background "#244f36")
      ;; 未選択項目&一致文字
      (set-face-attribute 'company-tooltip-common nil
                    :foreground "white" :background "#244f36")
      ;; 選択項目
      (set-face-attribute 'company-tooltip-selection nil
                  :foreground "#a1ffcd" :background "#007771")
      ;; 選択項目&一致文字
      (set-face-attribute 'company-tooltip-common-selection nil
                    :foreground "white" :background "#007771")
      ;; スクロールバー
      (set-face-attribute 'company-scrollbar-fg nil
                  :background "#4cd0c1")
      ;; スクロールバー背景
      (set-face-attribute 'company-scrollbar-bg nil
                  :background "#002b37")
      )
(global-company-mode)

(setq-default indent-tabs-mode nil)
(electric-pair-mode)

(add-hook 'markdown-mode-hook
	  (lambda ()
	    (markdown-mode-cyclic t)
	    ))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)
  ;(setq omnisharp-server-executable-path "C:\\Users\\fj\\.emacs.d\\.cache\\omnisharp\\server\\v1.34.1\\OmniSharp.exe")
  (setq omnisharp-server-executable-path "/mnt/c/Users/fj/.emacs.d/.cache/omnisharp/server/v1.34.1-linux/omnisharp/OmniSharp.exe")
  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25
  (local-set-key (kbd "C-j") 'omnisharp-go-to-definition-ex)
  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(defun omnisharp-go-to-definition-ex ()
  (interactive)
  (if (bounds-of-thing-at-point 'word)
      (omnisharp-go-to-definition)
    (pop-tag-mark)))

;; bug fix omnisharp process running dir
(defun omnisharp--do-server-start-advice (orig-func &rest args)
  "temporary change default-directory to path-to-project"
  (let ((default-directory (file-name-directory (car args))))
    (apply orig-func args)))

;; override toggle-frame-maximized
(advice-add 'omnisharp--do-server-start :around 'omnisharp--do-server-start-advice)

(require 'font-lock)
(require 'font-lock+)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; win環境でsvnがsjisで吐くのでbufferも追従するように与える
(add-to-list 'process-coding-system-alist '("[sS][vV][nN]" . sjis-dos))

;; 外部からの変更を自動読み込み
(global-auto-revert-mode)

