;; when use proxy, uncomment this.
;;(setq url-proxy-services
;;      '(("http" . "")
;;        ("https" . "")))

;; leafのロード
(prog1 "prepare leaf"
  (prog1 "package"
    (custom-set-variables
     '(package-archives '(("org"   . "https://orgmode.org/elpa/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("melpa-stable" . "https://stable.melpa.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")
                          ("gnu"   . "https://elpa.gnu.org/packages/"))))
    (package-initialize))

  (prog1 "leaf"
    (unless (package-installed-p 'leaf)
      (unless (assoc 'leaf package-archive-contents)
        (package-refresh-contents))
      (condition-case err
          (package-install 'leaf)
        (error
         (package-refresh-contents)       ; renew local melpa cache if fail
         (package-install 'leaf))))

    (leaf leaf-keywords
      :ensure t
      :config (leaf-keywords-init)))
)

(leaf leaf
  :config
  ;;leafのデバッグ用パッケージ
  (leaf leaf-convert :ensure t))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))


(leaf cus-start
  :doc "builtin"
  :init
  (defun move-beginning-alt ()
    (interactive)
    (if (bolp)
        (back-to-indentation)
      (beginning-of-line)))
  (defun backward-delete-word (arg)
    (interactive "p")
    (delete-region (point) (progn (backward-word arg) (point))))
  :bind (([C-wheel-up] . text-scale-increase)
         ([C-wheel-down] . text-scale-decrease)
         ((kbd "C-a") . move-beginning-alt)
         ((kbd "C-S-f") . forward-word)
         ((kbd "C-S-b") . backward-word)
         ((kbd "C-<backspace>") . backward-delete-word)
         )
  :custom
  `((menu-bar-mode . nil)
    (scroll-bar-mode . nil)
    (tool-bar-mode . nil)
    (inhibit-compacting-font-caches . t)
    (inhibit-startup-screen . t)
    (initial-scratch-message . "")
    )
  :config
  ;; スクリーンの最大化
  ;;(set-frame-parameter nil 'fullscreen 'maximized) for wsl?
  (toggle-frame-maximized)
  (set-default-coding-systems 'utf-8-unix)
  (setq-default indent-tabs-mode nil)
  (setq-default truncate-lines t);; 行末折り返ししない
  (electric-pair-mode) ;; 
  (global-auto-revert-mode)  ;; 外部からの変更を自動読み込み
  (global-hl-line-mode t)                   ;; 現在行をハイライト
  (show-paren-mode t)                       ;; 対応する括弧をハイライト
  (setq show-paren-style 'mixed)            ;; 括弧のハイライトの設定。
  (transient-mark-mode t)                   ;; 選択範囲をハイライト
  (setq backup-directory-alist '((".*" . "~/.emacs.d/auto-save")))
  (setq version-control t)
  (setq kept-new-versions 5)
  (setq kept-old-versions 1)
  (setq delete-old-versions t)
  (setq create-lockfiles nil)
  ;; for 4k
  (setq split-height-threshold nil)
  (setq split-width-threshold 320)
  )

(leaf doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t))
  (set-frame-font "ricty diminished-10.5")

(leaf *mode-line
  :config
  (leaf nyan-mode
    :ensure t
    :custom
    ((nyan-bar-length . 10))
    :config
    (nyan-mode t))
  (leaf all-the-icons
    :ensure t
    :custom
    ((all-the-icons-scale-factor . 1.0)))
  (leaf doom-modeline
    :ensure t
    :preface
    (defun my:doom-modeline-set-x-modelene ()
      "Do nothing")
    :custom
    ((doom-modeline-buffer-file-name-style . 'file-name)
     (doom-modeline-icon . t)
     (doom-modeline-major-mode-icon . t)
     (doom-modeline-major-mode-color-icon . t)
     (doom-modeline-minor-modes . nil)
     (eol-mnemonic-dos . "↲")
     (eol-mnemonic-unix . "↓")
     (eol-mnemonic-mac . "←")
     (eol-mnemonic-undecided . "・"))
    :custom-face
    ((doom-modeline-bar . `((t (:background "MediumPurple")))))
    :advice
    ;; 余計なmodeline切り替えを無効に
    ((:override doom-modeline-set-minimal-modeline
                my:doom-modeline-set-x-modelene)
     (:override doom-modeline-set-special-modeline
                my:doom-modeline-set-x-modelene)
     (:override doom-modeline-set-project-modeline
                my:doom-modeline-set-x-modelene)
     (:override doom-modeline-set-vcs-modeline
                my:doom-modeline-set-x-modelene)
     (:override doom-modeline-set-info-modeline
                my:doom-modeline-set-x-modelene)
     (:override doom-modeline-set-package-modeline
                my:doom-modeline-set-x-modelene)
     (:override doom-modeline-set-media-modeline
                my:doom-modeline-set-x-modelene)
     (:override doom-modeline-set-message-modeline
                my:doom-modeline-set-x-modelene)
     (:override doom-modeline-set-pdf-modeline
                my:doom-modeline-set-x-modelene)
     (:override doom-modeline-set-org-src-modeline
                my:doom-modeline-set-x-modelene)
     (:override doom-modeline-set-helm-modeline
                my:doom-modeline-set-x-modelene)
     (:override doom-modeline-set-timemachine-modeline
                my:doom-modeline-set-x-modelene))
    :config
    (line-number-mode 1)
    (column-number-mode 1)
    (with-eval-after-load 'doom-modeline ;; def-segment有効になるまで待つ
      (doom-modeline-def-segment buffer-mule-info
        (propertize
         (concat " %z" (mode-line-eol-desc) "%* ")
         'face (if (doom-modeline--active) `(:background "MediumPurple" :inherit) 'mode-line-inactive)))
      (doom-modeline-def-segment my/major-mode
        (concat
         (doom-modeline-spc)
         (doom-modeline--buffer-mode-icon)))
      (doom-modeline-def-segment my/major-mode-name
        (concat
         mode-name
         (doom-modeline-spc)))
      (doom-modeline-def-segment my/buffer-info
        (propertize
         (concat
          (doom-modeline-spc)
          (doom-modeline--buffer-name))
         'face (if (doom-modeline--active) 'doom-modeline-buffer-file 'mode-line-inactive)))
      (doom-modeline-def-modeline 'main
        '(bar my/major-mode my/major-mode-name buffer-mule-info my/buffer-info)
        '(input-method checker process vcs buffer-position )))
    (doom-modeline-mode t))
  )

(leaf ssh
  :ensure t
  :custom
  ((ssh-directory-tracking-mode . t)
   (dirtrackp .nil))
  :config
  (with-eval-after-load 'ssh (shell-dirtrack-mode t)))

(leaf company
  :after t
  :bind (("<tab>" . company-indent-or-complete-common)
         (company-active-map
          ("C-n" . company-select-next))
         (company-active-map
          ("C-p" . company-select-previous))
         (company-active-map
          ("C-h"))
         (company-active-map
          ("C-S-h" . company-show-doc-buffer)))
  :setq ((company-auto-expand . t)
         (company-transformers quote
                               (company-sort-by-backend-importance))
         (company-idle-delay . 0)
         (company-minimum-prefix-length . 2)
         (company-selection-wrap-around . t)
         (completion-ignore-case . t)
         (company-dabbrev-downcase))
  :config
  ;; (set-face-attribute 'company-tooltip nil :foreground "#36c6b0" :background "#244f36")
  ;; (set-face-attribute 'company-tooltip-common nil :foreground "white" :background "#244f36")
  ;; (set-face-attribute 'company-tooltip-selection nil :foreground "#a1ffcd" :background "#007771")
  ;; (set-face-attribute 'company-tooltip-common-selection nil :foreground "white" :background "#007771")
  ;; (set-face-attribute 'company-scrollbar-fg nil :background "#4cd0c1")
  ;; (set-face-attribute 'company-scrollbar-bg nil :background "#002b37")
  (global-company-mode)
  (with-eval-after-load 'company
  (add-to-list 'company-backends 'company-omnisharp))
  )

(leaf migemo
  :ensure t
  :custom
  ((migemo-command . "cmigemo")
   (migemo-options . '("-q" "--emacs" "-i" "\a"))
   (migemo-user-dictionary . nil)
   (migemo-regex-dictionary . nil)
   (migemo-use-pattern-alist . t)
   (migemo-use-frequent-pattern-alist . t)
   (migemo-pattern-alist-length . 1000)
   (migemo-coding-system . 'utf-8-unix))
  :config
  (setq migemo-dictionary (expand-file-name "~/.emacs.d/lib/dict/utf-8/migemo-dict"))
  (load-library "migemo")
  (migemo-init)
  )

(leaf ripgrep
  :ensure t 
  :custom
  ((ripgrep-executable . "rg")
   (ripgrep-arguments . '("-S"))))

(leaf *windows-nt
  :if (eq system-type 'windows-nt)
  :config
  ;; win環境でsvnがsjisで吐くのでbufferも追従するように与える
  (add-to-list 'process-coding-system-alist '("[sS][vV][nN]" . sjis-dos))
  )

(leaf *dired
  :config
  (leaf *windows
    :if (or (eq system-type 'windows-nt)
            (and (eq system-type 'gnu/linux) (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop")))
    :config
    (defun dired-open-file ()
      "In dired, open the file named on this line."
      (interactive)
      (let* ((file (if (eq system-type 'windows-nt) (dired-get-filename) (shell-command-to-string (concat "wslpath -w" " " (dired-get-filename)))))
             (open (if (eq system-type 'windows-nt) "start" "explorer.exe")))
        (message "Opening %s..." file)
        (shell-command (mapconcat #'shell-quote-argument (list open file) " "))
        ;;(call-process "gnome-open" nil 0 nil file)
        (message "Opening %s done" file)))
    (add-hook 'dired-mode-hook
              '(lambda ()
                 (define-key dired-mode-map "\C-o" 'dired-open-file)
                 (all-the-icons-dired-mode))))

  (leaf *darwin
    :if (eq system-type 'darwin)
    :config
    (defun open-mac (path)
      (start-process "dired-open-mac" nil "open" path))

    (defun quicklook-file (path)
      (interactive)
      (defvar cur nil)
      (defvar old nil)
      (setq old cur)
      (setq cur (start-process "ql-file" nil "qlmanage" "-p" path))
      (when old (delete-process old)))

    (defun my-dired-open ()
      (interactive)
      (let ((exts-ql   '("jpeg" "jpg" "png" "gif"))
            (exts-open '("avi" "mkv" "mp4" "pdf")))
        (cond ((file-accessible-directory-p (dired-get-file-for-visit))
               (call-interactively 'dired-find-alternate-file))
              ((member (downcase (file-name-extension (dired-get-file-for-visit))) exts-ql)
               (funcall 'quicklook-file (dired-get-file-for-visit)))
              ((member (downcase (file-name-extension (dired-get-file-for-visit))) exts-open)
               (funcall 'open-mac (dired-get-file-for-visit)))
              (t
               (call-interactively 'dired-find-file-other-window)))))
    (add-hook 'dired-mode-hook
              '(lambda ()
                 (define-key dired-mode-map "\C-o" 'my-dired-open))))

  (leaf *common
    :config
    ;; dired-find-alternate-file の有効化
    (put 'dired-find-alternate-file 'disabled nil)

    ;; ファイルなら別バッファで、ディレクトリなら同じバッファで開く
    (defun dired-open-in-accordance-with-situation ()
      (interactive)
      (let ((file (dired-get-filename)))
        (if (file-directory-p file)
            (dired-find-alternate-file)
          (dired-find-file))))

    (add-hook 'dired-mode-hook
              '(lambda ()
                 ;; RET 標準の dired-find-file では dired バッファが複数作られるので
                 ;; dired-find-alternate-file を代わりに使う
                 (define-key dired-mode-map (kbd "RET") 'dired-open-in-accordance-with-situation)
                 (define-key dired-mode-map (kbd "a") 'dired-find-file)))
    )
  )

(leaf *csharp
  :config
  (leaf omnisharp
    :ensure t
    )

  (leaf csharp-mode
    :ensure t
    :hook ((csharp-mode-hook . my-csharp-mode-setup))
    :config
    (defun my-csharp-mode-setup nil
      (omnisharp-mode)
      (flycheck-mode)
      (setq indent-tabs-mode nil)
      (setq c-syntactic-indentation t)
      (c-set-style "ellemtel")
      (setq c-basic-offset 4)
      (setq truncate-lines t)
      (setq tab-width 4)
      (setq evil-shift-width 4)
      ;;(setq omnisharp-server-executable-path "/mnt/c/Users/fj/.emacs.d/.cache/omnisharp/server/v1.34.1-linux/omnisharp/OmniSharp.exe")
      (local-set-key
       (kbd "C-j")
       'omnisharp-go-to-definition-ex)
      (local-set-key
       (kbd "C-c r r")
       'omnisharp-run-code-action-refactoring))

    (defun omnisharp-go-to-definition-ex nil
      (interactive)
      (if (bounds-of-thing-at-point 'word)
          (omnisharp-go-to-definition)
        (pop-tag-mark)))

    ;; csファイルからserver-startしたときにsln見失うbugfix
    (defun omnisharp--do-server-start-advice (orig-func &rest args)
      "temporary change default-directory to path-to-project"
      (let ((default-directory (file-name-directory
                                (car args))))
        (apply orig-func args)))

    (advice-add 'omnisharp--do-server-start :around 'omnisharp--do-server-start-advice)
    )
  )

(leaf nxml-mode
  :mode "\\.xaml\\'"
  )

(leaf *cpp
  :config
  ;; todo: coding style 4 tab etc...
  (leaf counsel-gtags
    :ensure t
    :config
    (add-hook 'c-mode-hook 'counsel-gtags-mode)
    ;; key bindings
    (add-hook 'counsel-gtags-mode-hook
              '(lambda ()
                 (local-set-key (kbd "C-j") 'counsel-gtags-dwim-ex)
                 (local-set-key (kbd "C-c t") 'counsel-gtags-find-definition)
                 (local-set-key (kbd "C-c r") 'counsel-gtags-find-reference)
                 (local-set-key (kbd "C-c s") 'counsel-gtags-find-symbol)
                 (local-set-key (kbd "C-c h") 'counsel-gtags-find-header-or-source)
                 (local-set-key (kbd "C-c b") 'counsel-gtags-go-backward)))

    (defun counsel-gtags-dwim-ex ()
      (interactive)
      (if (bounds-of-thing-at-point 'word)
          (counsel-gtags-dwim)
        (counsel-gtags-go-backward)))

    (defun counsel-gtags-find-header-or-source ()
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
          (counsel-gtags-find-file (concat prefix target))
          )))
    )
  )

(leaf *markdown
  :config
  (add-hook 'markdown-mode-hook
            (lambda nil
              (markdown-mode-cyclic t))))

;; shellの環境設定
(when (not (eq system-type 'windows-nt))
(exec-path-from-shell-initialize))


;; wsl fullscreen
(when (and (eq system-type 'gnu/linux)
	     (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop"))

(defvar frame-wsl-powershell-path nil)
(defvar frame-wsl-w32-sys-cmd-path "C:\\\\Users\\\\fj\\\\work\\\\w32_wm_syscmd.ps1")
(defvar frame-wsl-wm-name "vcxsrv")

;; Toggle flag. Do not touch this.
(setq frame-wsl-maximize nil)

(defun frame-maximized-wsl (maximize)
  "If MAXIMIZE is not nil, frame maximize, or frame restore
This function works only with the Windows Subsystem for Linux.
And, use powershell script for win32api::SendMessage().

You can customize these variables for your enviroment.
`frame-wsl-powershell-path' Path to powershell.exe. Maybe not need.
`frame-wsl-w32-sys-cmd-path' Path to powershell script (w32_wm_syscmd.ps1).
`frame-wsl-wm-name' Process name (e.g. vcxsrv) to get window handle.
"
  (let ((powershell (or frame-wsl-powershell-path "powershell.exe"))
	(powershell-opt "-ExecutionPolicy RemoteSigned -File")
	(w32-wm-syscmd (or frame-wsl-w32-sys-cmd-path "w32_wm_syscmd.ps1"))
	(w32-wm-syscmd-arg1 (if maximize "maximize" "restore"))
	(w32-wm-syscmd-arg2 (or frame-wsl-wm-name "")))
    (setq frame-wsl-maximize maximize)
    (shell-command
     (format "%s %s %s %s %s"
	     powershell powershell-opt
	     w32-wm-syscmd w32-wm-syscmd-arg1 w32-wm-syscmd-arg2))
    )
  )

(defun toggle-frame-maximized-advice (orig-func &rest args)
  (if frame-wsl-maximize
      (frame-maximized-wsl nil)
      (frame-maximized-wsl t) ) )

;; override toggle-frame-maximized
(advice-add 'toggle-frame-maximized :around 'toggle-frame-maximized-advice)
)

;; battery.el for wsl
(when (and (eq system-type 'gnu/linux)
	     (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop"))
(defun battery-linux-sysfs-wsl ()
  "Get ACPI status information from Linux kernel.
This function works only with the Windows Subsystem for Linux.

The following %-sequences are provided:
%c Current capacity (mAh)
%r Current rate
%B Battery status (verbose)
%b Battery status (charging:'+' discharging:'')
%d Temperature (in degrees Celsius)
%p Battery load percentage
%L AC line status (verbose)
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  (let (charging-state ac-state temperature hours
		       energy-now energy-now-rate power-now current-now voltage-now
		       (dir "/sys/class/power_supply/battery"))
    (with-temp-buffer ;; このブロック限りのバッファを生成しcurrent_bufferとする
      (erase-buffer)  ;; バッファに読み込む前にバッファを空に
      (ignore-errors (insert-file-contents
		      (expand-file-name "capacity" dir)))
      (setq energy-now-rate (or (thing-at-point 'number) "N/A"))
       
      (erase-buffer)
      (ignore-errors (insert-file-contents
		      (expand-file-name "status" dir)))
      (setq charging-state (or (thing-at-point 'word) "N/A"))

      (erase-buffer)
      (ignore-errors (insert-file-contents
		      (expand-file-name "temp" dir)))
      (setq temperature (or (thing-at-point 'number) "N/A"))
      (setq temperature (if (numberp temperature) (* temperature 0.1)))

      (erase-buffer)
      (ignore-errors (insert-file-contents
		      (expand-file-name "charge_counter" dir)))
      (setq energy-now (or (thing-at-point 'number) "N/A"))

      (erase-buffer)
      (ignore-errors (insert-file-contents
		      (expand-file-name "current_now" dir)))
      (setq current-now (or (thing-at-point 'number) "N/A"))
      (unless (or	(stringp energy-now) (stringp current-now)
			(stringp energy-now-rate) (zerop current-now))
	;; 変数がN/Aのときパス。充電完了となるとcurrent-now=0。zero-dev回避のため0のときパス
	(if (string= charging-state "Discharging")
	    (setq hours (/ energy-now current-now))
	  (setq hours (/ (* energy-now (- 100.0 energy-now-rate))
			 energy-now-rate current-now ))))

      (erase-buffer)
      (ignore-errors (insert-file-contents
		      (expand-file-name "voltage_now" dir)))
      (setq voltage-now (or (thing-at-point 'number) "N/A"))
      (setq power-now (if (and (numberp current-now) (numberp voltage-now))
			  (* (/ current-now 1000.0) (/ voltage-now 1000000.0))))
      ;; current-now[mA]->[A] voltage-now[uV]->[V]
    
      (erase-buffer)
      (setq dir "/sys/class/power_supply/ac") ;; acはpathが変わるので変更
      (ignore-errors (insert-file-contents
		      (expand-file-name "online" dir)))
      (setq ac-state (cond ((eq (thing-at-point 'number) 1) "AC")
			   ((eq (thing-at-point 'number) 0) "BAT")
			   (t "N/A")))
      )
    ;; 戻り値(リスト型)を設定
    (list (cons ?c (number-to-string energy-now))
	  (cons ?r (if hours (number-to-string power-now) "N/A"))
	  (cons ?B charging-state)
	  (cons ?b (if (string= charging-state "Charging") "+" ""))
	  (cons ?d (number-to-string temperature))
	  (cons ?p (number-to-string energy-now-rate))
	  (cons ?L ac-state)
	  (cons ?m (if hours (format "%d" (* hours 60)) "N/A"))
	  (cons ?h (if hours (format "%d" hours) "N/A"))
	  (cons ?t (if hours (format "%d:%02d" hours (* (- hours (floor hours)) 60)) "N/A")))
    )
)

(setq battery-status-function #'battery-linux-sysfs-wsl))

(leaf *which-func
  ;; 関数名表示
  :config
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
  )

;; mozcの設定 Linux環境のみ
(when (and (eq system-type 'gnu/linux)
	   (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop"))
  (setq default-input-method "japanese-mozc")
  (setq mozc-leim-title "あ")
  (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
  (require 'mozc-popup)
  (setq mozc-candidate-style 'popup)
  (set-face-background 'mozc-cand-overlay-description-face "steel blue")
  (set-face-background 'mozc-cand-overlay-even-face "steel blue")
  (set-face-background 'mozc-cand-overlay-odd-face "steel blue")
  (set-face-background 'mozc-cand-overlay-footer-face "steel blue")
  )

(leaf *window-t
  :doc "window切替関数の定義とkey-mapの設定"
  :config
  (defun other-window-or-split ()
    (interactive)
    (when (one-window-p)
      (split-window-horizontally)
      (pop-to-buffer nil))
    (other-window 1))

  ;; global-set-keyではほかのモードで上書きされてしまう ex)dired-mode
  (defvar window-t-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-t") 'other-window-or-split)
      map)
    "window-t-minor-mode keymap.")

  (define-minor-mode window-t-minor-mode
    "A minor mode that window-t key settings override annoying major modes."
    :init-value t
    :lighter "window-t")

  (window-t-minor-mode 1))

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


(leaf *xml
  :bind ((nxml-mode-map
          ("C-c C-o" . hs-toggle-hiding))
         (nxml-mode-map
          ("C-c C-l" . hs-hide-level))
         (nxml-mode-map
          ("C-c C-a" . hs-show-all)))
  :mode (("\\.xaml\\'" . nxml-mode))
  :config
  (add-hook 'nxml-mode-hook
            '(lambda nil
               (hs-minor-mode 1)
               (setq nxml-child-indent 4)))
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>" "" "<!--" nxml-forward-element nil)
               nil 'eq))


(leaf *ivy
  :config
  (leaf ivy
    :ensure t
    :bind ((ivy-minibuffer-map
            ("<escape>" . minibuffer-keyboard-quit)))
    :require ivy-hydra
    :setq ((ivy-use-virtual-buffers . t)
           (ivy-tab-space . t)
           (ivy-height-alist . '((t lambda (_caller) (/ (frame-height) 3)))))
           
    :config
    (when (setq enable-recursive-minibuffers t)
      (minibuffer-depth-indicate-mode 1))
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    (ivy-mode 1))

  (leaf counsel
    :ensure t
    :bind (("M-x" . counsel-M-x)
           ("C-M-z" . counsel-fzf)
           ("C-M-r" . counsel-recentf)
           ("C-x C-b" . counsel-ibuffer)
           ("C-c i" . counsel-imenu)
           ("C-x b" . ivy-switch-buffer)
           ("C-M-f" . counsel-rg)
           ("M-y" . counsel-yank-pop)
           ("C-x C-f" . counsel-find-file)
           (counsel-find-file-map
            ("C-l" . counsel-up-directory))
           (counsel-find-file-map
            ("<tab>" . ivy-alt-done)))
    :setq ((ivy-on-del-error-function function ignore)
           (ivy-initial-inputs-alist . nil))
    :config
    (setq counsel-find-file-ignore-regexp (regexp-opt
                                           '("./" "../")))
    (counsel-mode 1))

  (leaf swiper
    :ensure t
    :bind (("M-s M-s" . swiper-thing-at-point)))
  ;; (custom-set-faces
  ;;  '(ivy-current-match
  ;;    ((((class color) (background light))
  ;;      :background "#FFF3F3" :distant-foreground "#000000")
  ;;     (((class color) (background dark))
  ;;      :background "#404040" :distant-foreground "#abb2bf")))
  ;;  '(ivy-minibuffer-match-face-1
  ;;    ((((class color) (background light)) :foreground "#666666")
  ;;     (((class color) (background dark)) :foreground "#999999")))
  ;;  '(ivy-minibuffer-match-face-2
  ;;    ((((class color) (background light)) :foreground "#c03333" :underline t)
  ;;     (((class color) (background dark)) :foreground "#e04444" :underline t)))
  ;;  '(ivy-minibuffer-match-face-3
  ;;    ((((class color) (background light)) :foreground "#8585ff" :underline t)
  ;;     (((class color) (background dark)) :foreground "#7777ff" :underline t)))
  ;;  '(ivy-minibuffer-match-face-4
  ;;    ((((class color) (background light)) :foreground "#439943" :underline t)
  ;;     (((class color) (background dark)) :foreground "#33bb33" :underline t))))

  (leaf ivy-rich
    :ensure t
    :config
    (ivy-rich-mode 1))

  (leaf all-the-icons-ivy
    :ensure t
    :config
    (all-the-icons-ivy-setup)
    (dolist (command
             '(counsel-projectile-switch-project counsel-ibuffer))
      (add-to-list 'all-the-icons-ivy-buffer-commands command)))

  )

(leaf google-translate
  :ensure t
  :bind (("C-x t" . google-translate-enja-or-jaen))
  :config
  (defvar google-translate-english-chars "[:ascii:]’“”–" "これらの文字が含まれているときは英語とみなす")
  (defun google-translate-enja-or-jaen (&optional string)
    "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
    (interactive)
    (setq string (cond
                  ((stringp string)
                   string)
                  (current-prefix-arg
                   (read-string "Google Translate: "))
                  ((use-region-p)
                   (buffer-substring
                    (region-beginning)
                    (region-end)))
                  (t
                   (save-excursion
                     (let (s)
                       (forward-char 1)
                       (backward-sentence)
                       (setq s (point))
                       (forward-sentence)
                       (buffer-substring s
                                         (point)))))))
    (let* ((asciip (string-match
                    (format "\\`[%s]+\\'" google-translate-english-chars)
                    string)))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate
       (if asciip
           "en" "ja")
       (if asciip
           "ja" "en")
       string 'current-buffer))))


(leaf org
  :bind ((org-mode-map
          ("C-c M-o" . ace-link-org)))
  :setq ((org-plantuml-jar-path . "~/.emacs.d/lib/plantuml.jar"))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t))))
(leaf eww
  :bind ((eww-mode-map
          ("e" . ace-link-eww)))
  :config
  ;;; デフォルトの設定(参考)
;; (defun ace-link-setup-default ()
;;   "Setup the defualt shortcuts."
;;   (require 'info)
;;   (define-key Info-mode-map "o" 'ace-link-info)
;;   (require 'help-mode)
;;   (define-key help-mode-map "o" 'ace-link-help)
;;   (require 'eww)
;;   (define-key eww-link-keymap "o" 'ace-link-eww)
;;   (define-key eww-mode-map "o" 'ace-link-eww))
  (ace-link-setup-default)
  (defun ali--eww-collect-references nil
    "Collect the positions of visible links in the current `eww' buffer."
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))
        (goto-char (point-min))
        (let ((skip (next-single-property-change
                     (point)
                     'help-echo))
              candidates)
          (while (setq skip (text-property-not-all skip
                                                   (point-max)
                                                   'help-echo nil))
            (goto-char skip)
            (push skip candidates)
            (setq skip (next-single-property-change
                        (point)
                        'help-echo)))
          (nreverse candidates))))))

(leaf *xwidget-webkit
  :disabled t
  :hook ((xwidget-webkit-mode-hook . xwidget-webkit-mode-hook-func))
  :config
  (defun xwidget-webkit-mode-hook-func nil
    (remove-hook 'kill-buffer-query-functions #'xwidget-kill-buffer-query-function)
    (local-set-key
     (kbd "C-f")
     'xwidget-webkit-forward)
    (local-set-key
     (kbd "C-b")
     'xwidget-webkit-back)
    (local-set-key
     (kbd "C-r")
     'xwidget-webkit-reload)
    (local-set-key
     (kbd "C-l")
     'xwidget-webkit-browse-url)
    (local-set-key
     (kbd "C-w")
     'xwidget-webkit-current-url-message-kill)
    (local-set-key
     (kbd "C-q")
     'kill-current-buffer)
    (local-unset-key
     (kbd "C-x 2"))
    (local-unset-key
     (kbd "C-x 3")))
  (defvar xwidget-webkit-search-word-prefix "https://duckduckgo.com/?q=" "Search engine url and search request that before searching word.")
  (defvar xwidget-webkit-search-word-suffix "&kp=-1&kl=jp-jp" "Search engine option parameter that after searching word.")
  (defun xwidget-webkit-search-word (arg word)
    "Searching from `xwidget-webkit-search-word-prefix' search engine.\nAnd searching with `xwidget-webkit-search-word-suffix' search engine options.\nInteractively, WORD defaults to the string looking like a word around point.\nIf setting prefix args (C-u), reuses session(buffer). Normaly session(buffer) create."
    (interactive
     (list current-prefix-arg
           (read-string "search-word: "
                        (cond
                         ((thing-at-point 'word))
                         (t "")))))
    (let ((encoded-url (url-encode-url
                        (concat xwidget-webkit-search-word-prefix word xwidget-webkit-search-word-suffix))))
      (xwidget-webkit-browse-url encoded-url
                                 (not arg))))
;; (defun switch-buffer-functions-func (prev cur)
;;   (if (string= "xwidget-webkit-mode"
;;   	       (buffer-local-value 'major-mode
;; 				   (get-buffer (or prev "*scratch*"))))
;;     ))
;; (add-hook 'switch-buffer-functions #'switch-buffer-functions-func)
  )

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

(when (eq system-type 'darwin)
;; enable bash
(setq shell-file-name "/bin/bash")

;; magit に path 引き継ぎ
(require 'magit)
(exec-path-from-shell-initialize)

(leaf auto-complete-config
  :disabled t
  :ensure t
  :setq ((ac-use-menu-map . t)
         (ac-use-fuzzy . t))
  :config
  (ac-config-default)
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'fundamental-mode)
  (add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'yatex-mode)
  (ac-set-trigger-key "TAB")))
