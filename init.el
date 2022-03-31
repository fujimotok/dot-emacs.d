;;; init.el --- Emacs configuration file.
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Prepare configuration management package
(prog1
    "custom file setting to load aborting"
  (setq custom-file
        (locate-user-emacs-file
         "custom.el")))

(prog1
    "proxy setting"
;; when use proxy, add to ~/.emacs.d/local-custom/proxy.el
;; (setq url-proxy-services
;;       '(("http" . "proxy.example.com:8080")
;;         ("https" . "proxy.example.com:8080")))
  (push
   "~/.emacs.d/local-custom"
   load-path)
  (load "proxy" t))

(prog1
    "prepare leaf"
  (prog1
      "package"
    (custom-set-variables
     (quote
      (package-archives
       (quote
        (("melpa" . "https://melpa.org/packages/")
         ("melpa-stable" . "https://stable.melpa.org/packages/")
         ("gnu" . "https://elpa.gnu.org/packages/"))))))
    (package-initialize))
  (prog1
      "leaf"
    (unless (package-installed-p 'leaf)
      (unless (assoc 'leaf
                     package-archive-contents)
        (package-refresh-contents))
      (condition-case err
          (package-install 'leaf)
        (error
         (package-refresh-contents)
                                        ; renew local melpa cache if fail
         (package-install 'leaf))))
    (leaf
      leaf-keywords
      :ensure t
      :config (leaf el-get :ensure t)
      (leaf-keywords-init))))

;;; Basic settings
(leaf utils
  :doc "自作関数"
  :el-get fujimotok/emacs-utils
  :config (when (and (eq system-type 'gnu/linux)
                     (file-exists-p
                      "/proc/sys/fs/binfmt_misc/WSLInterop"))
            (battery-wsl-init)))

(leaf cus-start
  :doc "builtin機能の設定"
  :init
  ;; emacs 28.1から入る予定？
  (defun isearch-forward-thing-at-point ()
    "Do incremental search forward for the \"thing\" found near point.
Like ordinary incremental search except that the \"thing\" found at point
is added to the search string initially.  The \"thing\" is defined by
`bounds-of-thing-at-point'.  You can customize the variable
`isearch-forward-thing-at-point' to define a list of symbols to try
to find a \"thing\" at point.  For example, when the list contains
the symbol `region' and the region is active, then text from the
active region is added to the search string."
    (interactive)
    (isearch-forward nil 1)
    (let ((bounds (seq-some
                   (lambda (thing)
                     (bounds-of-thing-at-point
                      thing))
                   '(region url symbol))))
      (cond (bounds
             (when (use-region-p)
               (deactivate-mark))
             (when (< (car bounds) (point))
               (goto-char (car bounds)))
             (isearch-yank-string
              (buffer-substring-no-properties
               (car bounds)
               (cdr bounds))))
            (t
             (setq isearch-error
                   "No thing at point")
             (isearch-push-state)
             (isearch-update)))))
  :bind (([C-wheel-up] . text-scale-increase)
         ([C-wheel-down] . text-scale-decrease)
         ((kbd "C-a") . move-beginning-alt)
         ((kbd "C-s") . isearch-forward-thing-at-point)
         ((kbd "C-S-f") . forward-to-symbol)
         ((kbd "C-S-b") . backward-to-symbol)
         ((kbd "C-S-n") . forward-list)
         ((kbd "C-S-p") . backward-list)
         ((kbd "C-S-u") . backward-up-list)
         ((kbd "C-S-d") . down-list)
         ((kbd "C-S-a") . beginning-of-defun)
         ((kbd "C-S-e") . end-of-defun)
         ((kbd "C-<backspace>") . backward-delete-word)
         ((kbd "C-d") . forward-delete-char)
         ((kbd "C-z") . undo))
  :custom `((scroll-preserve-screen-position . t)
            (ring-bell-function . 'ignore))
  :config (set-default-coding-systems
           'utf-8-unix)
  (setq-default
   indent-tabs-mode
   nil)
  (setq-default truncate-lines t)
  (recentf-mode t)
  (savehist-mode)
  ;; 行末折り返ししない
  (electric-pair-mode)
  ;;
  (global-auto-revert-mode)
  ;; 外部からの変更を自動読み込み
  (global-hl-line-mode t)
  ;; 現在行をハイライト
  (show-paren-mode t)
  ;; 対応する括弧をハイライト
  (setq show-paren-style
        'expression)
  ;; 括弧のハイライトの設定。
  (transient-mark-mode t)
  ;; 選択範囲をハイライト
  (setq backup-directory-alist
        '((".*" . "~/.emacs.d/auto-save")))
  (setq version-control t)
  (setq kept-new-versions 5)
  (setq kept-old-versions 1)
  (setq delete-old-versions t)
  (setq create-lockfiles nil)
  (setq-default
   left-fringe-width
   20)
  (set-frame-font
   "ricty diminished-10.5")
  (set-face-attribute
   'fringe
   nil
   :background "#2a2c38"
   :foreground "#888882")
  ;; 画面分割の閾値 画面サイズが変わると更新 縦は分割させない 横は画面幅を分割閾値とすることで2分割までに制限
  (defun set-split-threshold-when-frame-size-changed (frame)
    (when (or (/= (window-pixel-width-before-size-change
                   (frame-root-window frame))
                  (window-pixel-width
                   (frame-root-window frame)))
              (/= (window-pixel-height-before-size-change
                   (frame-root-window frame))
                  (window-pixel-height
                   (frame-root-window frame))))
      (setq split-height-threshold
            nil)
      (setq split-width-threshold
            nil)))
  (add-hook
   'window-size-change-functions
   'set-split-threshold-when-frame-size-changed)
  (add-hook
   'prog-mode-hook
   #'hs-minor-mode))

(leaf *window-t
  :doc "window切替関数の定義とkey-mapの設定"
  :config (defun other-window-or-split ()
            (interactive)
            (when (one-window-p)
              (split-window-horizontally)
              (pop-to-buffer nil))
            (unless (window-minibuffer-p nil)
              (other-window 1)))
  ;; global-set-keyではほかのモードで上書きされてしまう ex)dired-mode
  (defvar window-t-minor-mode-map (let ((map (make-sparse-keymap)))
                                    (define-key map (kbd "C-t")
                                      'other-window-or-split)
                                    map)
    "window-t-minor-mode keymap.")
  (define-minor-mode window-t-minor-mode
    "A minor mode that window-t key settings override annoying major modes."
    :init-value t
    :lighter "window-t")
  (window-t-minor-mode 1))

;;; System depended settings
(leaf *windows-nt
  :doc "Windows環境のみの設定"
  :if (eq system-type 'windows-nt)
  :config ;; win環境でsvnがsjisで吐くのでbufferも追従するように与える
  (add-to-list
   'process-coding-system-alist
   '("[sS][vV][nN]" . sjis-dos))
  (add-to-list
   'process-coding-system-alist
   '("python" . sjis-dos))
  (add-hook
   'shell-mode-hook
   (lambda ()
     (set-buffer-process-coding-system
      'sjis-dos
      'sjis-dos))))

(leaf tr-ime
  :doc "NTEmacsでIMEの自動ON/OFFするためのパッケージ"
  :if (eq system-type 'windows-nt)
  :ensure t
  :custom ((default-input-method . "W32-IME")
           (w32-ime-mode-line-state-indicator . "Ａ")
           (w32-ime-mode-line-state-indicator-list . '("-" "あ" "Ａ")))
  :config (advice-add
           'w32-ime-init-mode-line-display
           :override (lambda ()))
  (tr-ime-standard-install)
  (w32-ime-initialize))

(leaf exec-path-from-shell
  :doc "MacOS環境でshell以外から起動したときにpathが引き継がれない問題の対策パッケージ"
  :if (memq
       window-system
       '(mac ns x))
  :ensure t
  :config (exec-path-from-shell-initialize))

(leaf mozc
  :doc "mozcの設定 Linux環境のみ"
  :config (when (and (eq system-type 'gnu/linux)
                     (file-exists-p
                      "/proc/sys/fs/binfmt_misc/WSLInterop"))
            (setq default-input-method
                  "japanese-mozc")
            (setq mozc-leim-title "あ")
            (global-set-key
             (kbd "<zenkaku-hankaku>")
             'toggle-input-method)
            (require 'mozc-popup)
            (setq mozc-candidate-style
                  'popup)
            (set-face-background
             'mozc-cand-overlay-description-face
             "steel blue")
            (set-face-background
             'mozc-cand-overlay-even-face
             "steel blue")
            (set-face-background
             'mozc-cand-overlay-odd-face
             "steel blue")
            (set-face-background
             'mozc-cand-overlay-footer-face
             "steel blue")))

;;; Appearance settings
(leaf doom-themes
  :doc "doomテーマのロード"
  :ensure t
  :config (load-theme 'doom-dracula t))

(leaf *mode-line
  :doc "モードラインの設定"
  :config (leaf
            nyan-mode
            :ensure t
            :custom ((nyan-bar-length . 10))
            :config (nyan-mode t))
  (leaf
    parrot
    :ensure t
    :custom :config (parrot-mode t)
    (add-hook
     'lsp-after-initialize-hook
     #'parrot-start-animation)
    (add-hook
     'lsp-after-open-hook
     #'parrot-start-animation))
  (leaf
    all-the-icons
    :ensure t
    :custom ((all-the-icons-scale-factor . 1.0)))
  (leaf
    doom-modeline
    :ensure t
    :preface (defun my:doom-modeline-set-x-modelene ()
               "Do nothing")
    :custom ((doom-modeline-buffer-file-name-style . 'buffer-name)
             (doom-modeline-icon . t)
             (doom-modeline-major-mode-icon . t)
             (doom-modeline-major-mode-color-icon . t)
             (doom-modeline-minor-modes . nil)
             (eol-mnemonic-dos . "↲")
             (eol-mnemonic-unix . "↓")
             (eol-mnemonic-mac . "←")
             (eol-mnemonic-undecided . "・"))
    :custom-face ((doom-modeline-bar . `((t
                                          (:background "MediumPurple")))))
    :advice ;; 余計なmodeline切り替えを無効に
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
    :config (line-number-mode 1)
    (column-number-mode 1)
    (with-eval-after-load
        'doom-modeline
      ;; def-segment有効になるまで待つ
      (doom-modeline-def-segment
        buffer-mule-info
        (propertize
         (concat
          " %z"
          (mode-line-eol-desc)
          "%* ")
         'face
         (if (doom-modeline--active)
             `(:background "MediumPurple"
                           :inherit)
           'mode-line-inactive)))
      (doom-modeline-def-segment
        my/major-mode
        (concat
         (doom-modeline-spc)
         (doom-modeline--buffer-mode-icon)))
      (doom-modeline-def-segment
        my/major-mode-name
        (concat
         mode-name
         (doom-modeline-spc)))
      (doom-modeline-def-segment
        my/buffer-info
        (propertize
         (concat
          (doom-modeline-spc)
          (doom-modeline--buffer-name))
         'face
         (if (doom-modeline--active)
             'doom-modeline-buffer-file
           'mode-line-inactive)))
      ;; adviceできないのでdoom-modelene-segments.elからコピーし再定義 inactive時も表示
      (doom-modeline-def-segment
        my/parrot
        "The party parrot animated icon. Requires `parrot-mode' to be enabled."
        (when (bound-and-true-p parrot-mode)
          (concat
           (parrot-create)
           (doom-modeline-spc)
           (doom-modeline-spc))))
      (doom-modeline-def-segment
        my/ime
        (concat
         ""
         w32-ime-mode-line-state-indicator))
      (doom-modeline-def-segment
        my/lsp
        (when (bound-and-true-p lsp-mode)
          (if-let
              (workspaces (lsp-workspaces))
              (concat
               (doom-modeline-lsp-icon
                "lsp:"
                'success)
               (string-join
                (--map
                 (car (split-string
                       (format
                        "%s"
                        (lsp--workspace-print it))
                       ":"))
                 workspaces)))
            (concat
             (doom-modeline-lsp-icon
              "lsp:"
              'warning)
             (propertize "!" 'face 'warning)))))
      (doom-modeline-def-segment
        my/vcs
        "Displays the current branch, colored based on its state."
        (let ((active t))
          (when-let
              ((icon doom-modeline--vcs-icon)
               (text doom-modeline--vcs-text))
            (concat
             (doom-modeline-spc)
             (propertize
              (concat
               (if active
                   icon
                 (doom-modeline-propertize-icon
                  icon
                  'mode-line-inactive))
               (doom-modeline-vspc))
              'mouse-face
              'mode-line-highlight
              'help-echo
              (get-text-property
               1
               'help-echo
               vc-mode)
              'local-map
              (get-text-property
               1
               'local-map
               vc-mode))
             (if active
                 text
               (propertize
                text
                'face
                'mode-line-inactive))
             (doom-modeline-spc)))))
      (doom-modeline-def-segment
        my/buffer-position
        "The buffer position information."
        (let* ((active t)
               (lc '(line-number-mode
                     (column-number-mode
                      (doom-modeline-column-zero-based
                       "%l:%c"
                       "%l:%C")
                      "%l")
                     (column-number-mode
                      (doom-modeline-column-zero-based
                       ":%c"
                       ":%C"))))
               (face (if active
                         'mode-line
                       'mode-line-inactive))
               (mouse-face 'mode-line-highlight)
               (local-map mode-line-column-line-number-mode-map))
          (concat
           (doom-modeline-spc)
           (doom-modeline-spc)
           (propertize
            (format-mode-line lc)
            'face
            face
            'help-echo
            "Buffer position
\
mouse-1: Display Line and Column Mode Menu"
            'mouse-face
            mouse-face
            'local-map
            local-map)
           (if (and active
                    (bound-and-true-p nyan-mode)
                    (>= (window-width)
                        nyan-minimum-window-width))
               (concat
                (doom-modeline-spc)
                (doom-modeline-spc)
                (propertize
                 (nyan-create)
                 'mouse-face
                 mouse-face))
             (when doom-modeline-percent-position
               (concat
                (doom-modeline-spc)
                (propertize
                 (format-mode-line
                  '(""
                    doom-modeline-percent-position
                    "%%"))
                 'face
                 face
                 'help-echo
                 "Buffer percentage
\
mouse-1: Display Line and Column Mode Menu"
                 'mouse-face
                 mouse-face
                 'local-map
                 local-map))))
           (when (or line-number-mode
                     column-number-mode
                     doom-modeline-percent-position)
             (doom-modeline-spc)))))
      (doom-modeline-def-modeline
        'main
        '(bar
          my/major-mode
          my/major-mode-name
          buffer-mule-info
          my/buffer-info)
        '(input-method
          my/lsp
          checker
          process
          my/ime
          my/vcs
          my/buffer-position
          my/parrot)))
    (doom-modeline-mode t)))

(leaf *titlebar
  :doc "タイトルバーに時計などを表示"
  :config (when (window-system)
            ;; display-timeより先にsetしておかないとdefaultの書式になる
            (setq display-time-string-forms
                  '((format
                     "%s/%s/%s"
                     year
                     month
                     day)
                    (format
                     "(%s:%s)"
                     24-hours
                     minutes)))
            (display-time)
            ;; display-time-stringの有効化
            (display-battery-mode 1)
            (setq battery-mode-line-format
                  " %b%p%%")
            (with-eval-after-load
                'doom-modeline
              ;; doom-modelineがbattery-mode-line-stringを更新させなくするのでremove
              (advice-remove
               'battery-update
               'doom-modeline-update-battery-status))
            ;; バッファがファイルのときはフルパス、でなければバッファ名表示
            ;; if(buffer-file-name) の評価がsetq時で終わらないよう:eval
            (setq frame-title-format
                  '(""
                    (:eval (if (buffer-file-name)
                               " %f"
                             " %b"))
                    " --- "
                    display-time-string
                    " "
                    battery-mode-line-string))))

(leaf rainbow-delimiters
  :doc "カッコに色をつけるパッケージ"
  :ensure t
  :custom-face ((rainbow-delimiters-depth-1-face . `((t (:forground "#9a4040"))))
                (rainbow-delimiters-depth-2-face . `((t (:forground "#ff5e5e"))))
                (rainbow-delimiters-depth-3-face . `((t (:forground "#ffaa77"))))
                (rainbow-delimiters-depth-4-face . `((t (:forground "#dddd77"))))
                (rainbow-delimiters-depth-5-face . `((t (:forground "#80ee80"))))
                (rainbow-delimiters-depth-6-face . `((t (:forground "#66bbff"))))
                (rainbow-delimiters-depth-7-face . `((t (:forground "#da6bda"))))
                (rainbow-delimiters-depth-8-face . `((t (:forground "#afafaf"))))
                (rainbow-delimiters-depth-9-face . `((t (:forground "#f0f0f0")))))
  :config (define-globalized-minor-mode
            global-rainbow-delimiters-mode
            rainbow-delimiters-mode
            rainbow-delimiters-mode)
  (global-rainbow-delimiters-mode
   t))

(leaf *pulse-line
  :doc "windowを切り替えた時などに現在の行をハイライトする"
  :custom ((pulse-iterations . 1))
  :config (defun pulse-line (&rest _)
            "Pulse the current line."
            (pulse-momentary-highlight-one-line
             (point)))
  (dolist (command
           '(scroll-up-command
             scroll-down-command
             recenter-top-bottom
             other-window))
    (advice-add
     command
     :after #'pulse-line)))

(leaf *which-func
  :doc "関数名表示 lsp-modeでは使わない"
  :config (which-function-mode)
  (setq which-func-header-line-format
        '(which-func-mode
          ("" which-func-format)))
  (defadvice which-func-ff-hook (after header-line activate)
    (when (and which-func-mode
               (not (bound-and-true-p lsp-mode)))
      (setq header-line-format
            which-func-header-line-format)))
  (defun show-file-name ()
    (interactive)
    (kill-new (buffer-file-name))
    (message
     "add kill ring: %s"
     (buffer-file-name)))
  (defun show-func-name ()
    (interactive)
    (kill-new (buffer-file-name))
    (message
     "add kill ring: %s"
     (which-function))))

(leaf *vertico
  :doc "emacsコマンド補完パッケージ"
  :config
  (leaf orderless
    :ensure t
    :custom ((completion-styles . '(orderless))))
  (leaf vertico
    :ensure t
    :bind ((vertico-map
            ("C-l" . my-filename-upto-parent)))
    :init
    (defun my-filename-upto-parent ()
      "Move to parent directory like \"cd ..\" in find-file."
      (interactive)
      (let ((sep (eval-when-compile (regexp-opt '("/" "\\")))))
        (save-excursion
          (left-char 1)
          (when (looking-at-p sep)
            (delete-char 1)))
        (save-match-data
          (when (search-backward-regexp sep nil t)
            (right-char 1)
            (filter-buffer-substring (point)
                                     (save-excursion (end-of-line) (point))
                                     #'delete)))))
    (vertico-mode))
  (leaf consult
    :ensure t
    :custom ((consult-preview-key . nil))
    :bind (("C-c i" . consult-imenu)
           ("C-x b" . consult-buffer)
           ("M-y" . consult-yank-pop)
           ("C-S-s" . consult-line)
           ("C-S-r" . consult-ripgrep)
           ("M-g g" . consult-goto-line)))
  (leaf embark
    :ensure t
    :bind (("M-a". embark-act))
    :custom ((embark-indicators . '(embark-minimal-indicator embark-highlight-indicator))))
  )

;;; Programming langages settings
(leaf company
  :doc "補完機能パッケージ"
  :ensure t
  :bind ((company-mode-map
          ("<tab>" . company-indent-or-complete-common))
         (company-active-map
          ("C-n" . company-select-next))
         (company-active-map
          ("C-p" . company-select-previous))
         (company-active-map ("C-h"))
         (company-active-map
          ("C-S-h" . company-show-doc-buffer)))
  :setq ((company-auto-expand . t)
         (company-transformers
          quote
          (company-sort-by-backend-importance))
         (company-idle-delay . nil)
         (company-minimum-prefix-length . 2)
         (company-selection-wrap-around . t)
         (completion-ignore-case . t)
         (company-dabbrev-downcase . nil)
         (company-dabbrev-char-regexp . "[A-Za-z_][[:alnum:]_]*"))
  :hook ((emacs-lisp-mode-hook . set-company-backend-lisp-mode)
         (omnisharp-mode-hook . set-company-backend-omnisharp-mode)
         (shell-mode-hook . set-company-backend-shell-mode)
         ;; lsp-modeがbackendsを書き換えるので、書き換え後をhookして元に戻す
         (lsp-after-initialize-hook . set-company-backend-lsp-mode))
  :config
  (with-eval-after-load
      'company
    ;; リストに直接置くと左から優先度して最初のbackendのみ使う。リストの要素にリスト渡すとグループと呼び、backendが同時に機能する。
    (setq company-backends
          '((company-dabbrev-code
             company-dabbrev
             company-files
             company-capf
             company-keywords))))
  (defun set-company-backend-lisp-mode ()
    (setq-local
     company-backends
     '((company-elisp))))
  (defun set-company-backend-omnisharp-mode ()
    (setq-local
     company-backends
     '((company-omnisharp
        company-dabbrev-code
        company-dabbrev
        company-files
        company-keywords))))
  (defun set-company-backend-lsp-mode ()
    (setq-local
     company-backends
     '((company-capf
        company-dabbrev-code
        company-dabbrev
        company-files
        company-keywords))))
  (defun set-company-backend-shell-mode ()
    (setq-local
     company-backends
     '((company-capf
        company-files))))
  (leaf company-web :ensure t)
  (leaf company-box
    :ensure t
    :custom ((company-box-scrollbar . nil))
    :hook ((company-mode-hook . company-box-mode))
    :config (defconst
              company-box-icons--omnisharp-alist
              '(("Text" . Text)
                ("Method" . Method)
                ("Function" . Function)
                ("Constructor" . Constructor)
                ("Field" . Field)
                ("Variable" . Variable)
                ("Class" . Class)
                ("interface" . Interface)
                ("Property" . Property)
                ("Module" . Module)
                ("Unit" . Unit)
                ("Value" . Value)
                ("Enum" . Enum)
                ("Keyword" . Keyword)
                ("Snippet" . Snippet)
                ("Color" . Color)
                ("File" . File)
                ("Reference" . Reference))
              "List of icon types to use with Omnisharp candidates.")
    (defun company-box-icons--omnisharp (candidate)
      (when (derived-mode-p 'csharp-mode)
        (cdr (assoc (alist-get
                     'Kind
                     (get-text-property
                      0
                      'omnisharp-item
                      candidate))
                    company-box-icons--omnisharp-alist))))
    (with-eval-after-load
        'company-box
      (add-to-list
       'company-box-icons-functions
       'company-box-icons--omnisharp)))
  (leaf company-quickhelp
    :ensure t
    :hook ((company-box-mode-hook . company-quickhelp-mode))
    :custom ((company-quickhelp-delay . 1)))
  (global-company-mode))

(leaf lsp-mode
  :ensure t
  :custom ;; debug
  ((lsp-print-io . t)
   (lsp-trace . t)
   (lsp-print-performance . t)
   (lsp-enable-snippet . nil)
   ;; general
   (lsp-auto-guess-root . nil)
   ;;(lsp-document-sync-method . 'incremental) ;; always send incremental document
   (lsp-document-sync-method . 2)
   (lsp-response-timeout . 5)
   (read-process-output-max . 10485760))
  :bind ((lsp-mode-map
          ("C-c C-r" . lsp-rename)))
  :config ;; if you are adding the support for your language server in separate repo use
  ;; (add-to-list 'lsp-language-id-configuration '(python-mode . "python"))
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "pyright")
  ;;                   :major-modes '(python-mode)
  ;;                   :server-id 'pyright))
  ;; (add-to-list 'lsp-language-id-configuration '(csharp-mode . "csharp"))
  ;;  (lsp-register-client
  ;;   (make-lsp-client :new-connection (lsp-stdio-connection '(".cache/lsp/omnisharp-roslyn/v1.37.0/OmniSharp.exe" "-lsp"))
  ;;                    :major-modes '(csharp-mode)
  ;;                    :server-id 'omnisharp))
  (add-to-list
   'lsp-language-id-configuration
   '(web-mode . "web"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("vls"))
    :major-modes '(web-mode)
    :server-id 'vls))
  ;; パンくずリスト。hookでdisableされるのを防ぐ
  (lsp-headerline-breadcrumb-mode
   1)
  (remove-hook
   'lsp-unconfigure-hook
   #'lsp-headerline--disable-breadcrumb
   t)
  ;; LSP UI tools
  (leaf
    lsp-ui
    :ensure t
    :custom ;; lsp-ui-doc
    ((lsp-ui-doc-enable . nil)
     (lsp-ui-doc-header . t)
     (lsp-ui-doc-include-signature . t)
     (lsp-ui-doc-position . 'at-point)
     ;; top, bottom, or at-point
     (lsp-ui-doc-max-width . 100)
     (lsp-ui-doc-max-height . 30)
     (lsp-ui-doc-use-childframe . t)
     (lsp-ui-doc-use-webkit . t)
     ;; lsp-ui-flycheck
     (lsp-ui-flycheck-enable . nil)
     ;; lsp-ui-sideline
     (lsp-ui-sideline-enable . nil)
     (lsp-ui-sideline-ignore-duplicate . t)
     (lsp-ui-sideline-show-symbol . nil)
     (lsp-ui-sideline-show-hover . nil)
     (lsp-ui-sideline-show-diagnostics . nil)
     (lsp-ui-sideline-show-code-actions . nil)
     ;; lsp-ui-imenu
     (lsp-ui-imenu-enable . nil)
     (lsp-ui-imenu-kind-position . 'top)
     ;; lsp-ui-peek
     (lsp-ui-peek-enable . t)
     (lsp-ui-peek-peek-height . 20)
     (lsp-ui-peek-list-width . 50)
     (lsp-ui-peek-fontify . 'on-demand))
    ;; never, on-demand, or always
    :preface (defun lsp-ui-peek-find-definitions-or-pop ()
               (interactive)
               (if (bounds-of-thing-at-point
                    'word)
                   (lsp-ui-peek-find-definitions)
                 (xref-pop-marker-stack)))
    (defun lsp-ui-peek-find-references-or-pop ()
      (interactive)
      (if (bounds-of-thing-at-point
           'word)
          (lsp-ui-peek-find-references)
        (xref-pop-marker-stack)))
    :bind ((lsp-mode-map
            ("C-j" . lsp-ui-peek-find-definitions-or-pop))
           (lsp-mode-map
            ("C-S-j" . lsp-ui-peek-find-references-or-pop))
           (lsp-mode-map
            ("C-h d" . lsp-ui-doc-show))
           (lsp-mode-map
            ("C-h a" . lsp-execute-code-action)))
    :hook ((lsp-mode-hook . lsp-ui-mode)
           (lsp-mode-hook . lsp-completion-mode))))

(leaf flycheck
  :custom ((flycheck-check-syntax-automatically . '(mode-enabled save))
           (flycheck-idle-change-delay . 2)))

(leaf *gud-mode
  :config (defun gud-print-at-symbol ()
            (interactive)
            (let ((cursor-symbol-pos (bounds-of-thing-at-point
                                      'symbol)))
              (if cursor-symbol-pos
                  (save-excursion
                    (set-mark
                     (car cursor-symbol-pos))
                    (goto-char
                     (cdr cursor-symbol-pos))
                    (gud-print
                     (car cursor-symbol-pos))
                    (deactivate-mark t)))))
  ;; watch
  (global-set-key
   (kbd "<f8>")
   'gud-print-at-symbol)
  ;; step over
  (global-set-key
   (kbd "<f10>")
   'gud-next)
  ;; step in
  (global-set-key
   (kbd "<f11>")
   'gud-step)
  ;; step out
  (global-set-key
   (kbd "S-<f11>")
   'gud-finish))

(leaf eldoc
  :hook ((emacs-lisp-mode-hook . turn-on-eldoc-mode))
  :preface (defun my:shutup-eldoc-message (f &optional string)
             (unless (active-minibuffer-window)
               (funcall f string)))
  :advice (:around eldoc-message
                   my:shutup-eldoc-message))

(leaf lispy
  :doc "lisp編集時の移動を楽にするパッケージ"
  :ensure t
  :hook ((emacs-lisp-mode-hook . lispy-mode)))

(leaf markdown-mode
  :doc "markdown用設定"
  :hook ((markdown-mode-hook . (lambda nil (outline-hide-sublevels 1))))
  :bind ((markdown-mode-map
          ("C-<return>" . markdown-insert-dwin)))
  :custom ((markdown-fontify-code-blocks-natively . t)
           (markdown-asymmetric-header . t)
           (markdown-code-lang-modes . '(("elisp" . emacs-lisp-mode)
                                         ("sqlite" . sql-mode)
                                         ("c" . c-mode)
                                         ("cpp" . c++-mode)
                                         ("js" . javascript-mode)
                                         ("json" . json-mode)
                                         ("shell" . sh-mode))))
  :config
  (defun markdown-insert-image-from-clipboard ()
    (interactive)
    (let ((filepath))
      (if (eq 1
              (call-process-shell-command
               (format
                "%s %s %s"
                "powershell.exe"
                "-Command"
                "$clip = Get-Clipboard -Format Image; if ($null -eq $clip) {exit 1}")
               nil))
          (message
           "no image on a clipboard")
        (setq filepath
              (read-file-name
               "Save file path: "))
        (call-process-shell-command
         (format
          "%s %s %s'%s'%s"
          "powershell.exe"
          "-Command"
          "$clip = Get-Clipboard -Format Image; if ($null -ne $clip) { $clip.Save("
          filepath
          ") }")
         (insert
          (format
           "![](%s)"
           (file-relative-name filepath)))
         (markdown-display-inline-images)))))
  (defun markdown-insert-dwin (&optional arg)
    (interactive "p")
    (let ((l (thing-at-point 'line)))
      (if (string-match "^[ \t]*[-]" l)
          (markdown-insert-list-item arg)
        (move-end-of-line 1)
        (markdown-insert-header-dwim)
        ))))

(leaf *csharp
  :doc "C#用設定"
  :config (leaf omnisharp :ensure t)
  ;; omniSharp lsp に移行中。問題なければ上記omnisharpは不要
  (leaf
    csharp-mode
    :ensure t
    :hook (;;(csharp-mode-hook . lsp)
           (csharp-mode-hook . my-csharp-mode-setup))
    :config ;; csharp-mode からimenuがなくなるので自前で定義
    ;; ==================================================================
    ;; imenu stuff
    (defconst
      csharp--imenu-expression
      (let* ((single-space "[ \t\n\r\f\v]")
             (optional-space (concat single-space "*"))
             (bol "^[ \t]*")
             ;; BOL shouldnt accept lineshift.
             (space (concat single-space "+"))
             (access-modifier (regexp-opt
                               '("public"
                                 "private"
                                 "protected"
                                 "internal"
                                 "static"
                                 "sealed"
                                 "partial"
                                 "override"
                                 "virtual"
                                 "abstract"
                                 "async"
                                 "new"
                                 "unsafe")))
             ;; this will allow syntactically invalid combinations of modifiers
             ;; but that's a compiler problem, not a imenu-problem
             (access-modifier-list (concat
                                    "\\(?:"
                                    access-modifier
                                    space
                                    "\\)"))
             (access-modifiers (concat
                                access-modifier-list
                                "*"))
             (basic-type (concat
                          ;; typename
                          "\\(?:[A-Za-z_][[:alnum:]_]*\\.\\)*"
                          "[A-Za-z_][[:alnum:]_]*"))
             (type (concat
                    basic-type
                    ;; simplified, optional generic constraint.
                    ;; handles generic sub-types.
                    "\\(?:<[[:alnum:],<> \t
\f\v\r]+>\\)?"))
             (return-type (concat
                           type
                           ;; optional array-specifier
                           "\\(?:\\[\\]\\)?"))
             (interface-prefix (concat "\\(?:" type "\\.\\)"))
             ;; param-list with parens
             (parameter-list "\\(?:\([^!\)]*\)\\)")
             (inheritance-clause (concat
                                  "\\(?:"
                                  optional-space
                                  ":"
                                  optional-space
                                  type
                                  "\\(?:"
                                  optional-space
                                  ","
                                  optional-space
                                  type
                                  "\\)*"
                                  "\\)?")))
        (list
         (list
          "namespace"
          (concat
           bol
           "namespace"
           space
           "\\("
           basic-type
           "\\)")
          1)
         ;; not all these are classes, but they can hold other
         ;; members, so they are treated uniformly.
         (list
          "class"
          (concat
           bol
           access-modifiers
           "\\("
           (regexp-opt
            '("class" "struct" "interface"))
           space
           type
           inheritance-clause
           "\\)")
          1)
         (list
          "enum"
          (concat
           bol
           access-modifiers
           "\\("
           "enum"
           space
           basic-type
           "\\)")
          1)
         (list
          "ctor"
          (concat
           bol
           ;; ctor MUST have access modifiers, or else we pick
           ;; every if statement in the file...
           access-modifier-list
           "+"
           "\\("
           basic-type
           optional-space
           parameter-list
           "\\)"
           "\\(?:"
           optional-space
           ":"
           optional-space
           "\\(?:this\\|base\\)"
           optional-space
           parameter-list
           "\\)?"
           optional-space
           "{")
          1)
         (list
          "method"
          (concat
           bol
           ;; we MUST require modifiers, or else we cannot reliably
           ;; identify declarations, without also dragging in lots of
           ;; if statements and what not.
           access-modifier-list
           "+"
           return-type
           space
           "\\("
           type
           optional-space
           parameter-list
           "\\)"
           ;; optional // or /* comment at end
           "\\(?:[ \t]*/[/*].*\\)?"
           optional-space
           "{")
          1)
         (list
          "method-inf"
          (concat
           bol
           return-type
           space
           "\\("
           interface-prefix
           type
           optional-space
           parameter-list
           "\\)"
           ;; optional // or /* comment at end
           "\\(?:[ \t]*/[/*].*\\)?"
           optional-space
           "{")
          1)
         (list
          "method-abs-ext"
          (concat
           bol
           access-modifier-list
           "+"
           (regexp-opt
            '("extern" "abstract"))
           space
           return-type
           space
           "\\("
           type
           optional-space
           parameter-list
           "\\)"
           optional-space
           ;; abstract/extern methods are terminated with ;
           ";")
          1)
         ;; delegates are almost like abstract methods, so pick them up here
         (list
          "delegate"
          (concat
           bol
           access-modifiers
           "delegate"
           space
           return-type
           space
           "\\("
           type
           "\\)"
           optional-space
           parameter-list
           ;; optional // or /* comment at end
           optional-space
           ";")
          1)
         (list
          "prop"
          (concat
           bol
           ;; must require access modifiers, or else we
           ;; pick up pretty much anything.
           access-modifiers
           return-type
           space
           "\\("
           type
           "\\)"
           optional-space
           "{"
           optional-space
           ;; unless we are super-specific and expect the accesors,
           ;; lots of weird things gets slurped into the name.
           ;; including the accessors themselves.
           (regexp-opt '("get" "set")))
          1)
         (list
          "prop-inf"
          (concat
           bol
           return-type
           space
           "\\("
           interface-prefix
           type
           "\\)"
           optional-space
           "{"
           optional-space
           ;; unless we are super-specific and expect the accesors,
           ;; lots of weird things gets slurped into the name.
           ;; including the accessors themselves.
           (regexp-opt '("get" "set")))
          1)
         ;; adding fields... too much?
         (list
          "field"
          (concat
           bol
           access-modifier-list
           "+"
           ;; fields can be readonly/const/volatile
           "\\(?:"
           (regexp-opt
            '("readonly" "const" "volatile"))
           space
           "\\)?"
           return-type
           space
           "\\("
           type
           "\\)"
           optional-space
           ;; optional assignment
           "\\(?:=[^;]+\\)?"
           ";")
          1)
         (list
          "indexer"
          (concat
           bol
           access-modifiers
           return-type
           space
           "this"
           optional-space
           "\\("
           ;; opening bracket
           "\\["
           optional-space
           ;; type
           "\\([^\]]+\\)"
           optional-space
           type
           ;; closing brackets
           "\\]"
           "\\)"
           optional-space
           "{"
           optional-space
           ;; unless we are super-specific and expect the accesors,
           ;; lots of weird things gets slurped into the name.
           ;; including the accessors themselves.
           (regexp-opt '("get" "set")))
          1)
         (list
          "event"
          (concat
           bol
           access-modifier-list
           "+"
           optional-space
           "event"
           optional-space
           "\\("
           return-type
           space
           type
           "\\)"
           optional-space
           ";")
          1))))
    (defun csharp--imenu-get-pos (pair)
      "Return `position' from a (title . position) cons-pair `PAIR'.

   The position may be a integer, or a marker (as returned by
   imenu-indexing).  This function ensures what is returned is an
   integer which can be used for easy comparison."
      (let ((pos (cdr pair)))
        (if (markerp pos)
            (marker-position pos)
          pos)))
    (defun csharp--imenu-get-container (item containers previous)
      "Return the container which `ITEM' belongs to.

   `ITEM' is a (title . position) cons-pair.  `CONTAINERS' is a
   list of such.  `PREVIOUS' is the name of the previous
   container found when recursing through `CONTAINERS'.

   The final result is based on item's position relative to those
   found in `CONTAINERS', or nil if none is found."
      (if (not containers)
          previous
        (let* ((item-pos (csharp--imenu-get-pos item))
               (container (car containers))
               (container-pos (csharp--imenu-get-pos
                               container))
               (rest (cdr containers)))
          (if (and container-pos
                   (< item-pos container-pos))
              previous
            (csharp--imenu-get-container
             item
             rest
             container)))))
    (defun csharp--imenu-get-container-name (item containers)
      "Return the name of the container which `ITEM' belongs to.

   `ITEM' is a (title . position) cons-pair.
   `CONTAINERS' is a list of such.

   The name is based on the results from
   `csharp--imenu-get-container'."
      (let ((container (csharp--imenu-get-container
                        item
                        containers
                        nil)))
        (if (not container)
            nil
          (let ((container-p1 (car (split-string (car container))))
                ;; namespace
                (container-p2 (cadr
                               (split-string (car container)))))
            ;; class/interface
            ;; use p1 (namespace) when there is no p2
            (if container-p2
                container-p2
              container-p1)))))
    (defun csharp--imenu-sort (items)
      "Sort an imenu-index list `ITEMS' by the string-portion."
      (sort
       items
       (lambda (item1 item2)
         (string<
          (car item1)
          (car item2)))))
    (defun csharp--imenu-get-class-name (class namespaces)
      "Gets a name for a imenu-index `CLASS'.

   Result is based on its own name and `NAMESPACES' found in the same file."
      (let ((namespace (csharp--imenu-get-container-name
                        class
                        namespaces))
            (class-name (car class)))
        (if (not namespace)
            class-name
          ;; reformat to include namespace
          (let* ((words (split-string class-name))
                 (type (car words))
                 (name (cadr words)))
            (concat
             type
             " "
             namespace
             "."
             name)))))
    (defun csharp--imenu-get-class-nodes (classes namespaces)
      "Create a new alist with CLASSES as root nodes with NAMESPACES added.

   Each class will have one imenu index-entry \"( top)\" added by
   default."
      (mapcar
       (lambda (class)
         (let ((class-name (csharp--imenu-get-class-name
                            class
                            namespaces))
               (class-pos (cdr class)))
           ;; construct a new alist-entry where value is itself
           ;; a list of alist-entries with -1- entry which the top
           ;; of the class itself.
           (cons class-name
                 (list
                  (cons "( top )" class-pos)))))
       classes))
    (defun csharp--imenu-get-class-node (result item classes namespaces)
      "Get the class-node in `RESULT' which an `ITEM' should be inserted into.

   For this calculation, the original index items `CLASSES' and `NAMESPACES'
   is needed."
      (let* ((class-item (csharp--imenu-get-container
                          item
                          classes
                          nil))
             (class-name (csharp--imenu-get-class-name
                          class-item
                          namespaces)))
        (assoc class-name result)))
    (defun csharp--imenu-format-item-node (item type)
      "Format an ITEM with a specified TYPE as an imenu item to be inserted into the index."
      (cons (concat
             "("
             type
             ") "
             (car item))
            (cdr item)))
    (defun csharp--imenu-append-items-to-menu (result key name index classes namespaces)
      "Formats the imenu-index using the provided values.

This is done by modifying the contents of `RESULT' in place."
      ;; items = all methods, all events, etc based on "type"
      (let* ((items (cdr (assoc key index))))
        (dolist (item items)
          (let ((class-node (csharp--imenu-get-class-node
                             result
                             item
                             classes
                             namespaces))
                (item-node (csharp--imenu-format-item-node
                            item
                            name)))
            (nconc
             class-node
             (list item-node))))))
    (defun csharp--imenu-transform-index (index)
      "Transform an imenu INDEX based on `IMENU-GENERIC-EXPRESSION'.

  The resulting structure should be based on full type-names, with
  type-members nested hierarchially below its parent.

  See `csharp-mode-tests.el' for examples of expected behaviour
  of such transformations."
      (let* ((result nil)
             (namespaces (cdr (assoc "namespace" index)))
             (classes (cdr (assoc "class" index)))
             (class-nodes (csharp--imenu-get-class-nodes
                           classes
                           namespaces)))
        ;; be explicit about collection variable
        (setq result class-nodes)
        (dolist (type
                 '(("ctor")
                   ("method")
                   ("method-inf" "method")
                   ("method-abs-ext" "method")
                   ("prop")
                   ("prop-inf" "prop")
                   ("field")
                   ("event")
                   ("indexer")))
          (let* ((key (car type))
                 (name (car (last type))))
            (csharp--imenu-append-items-to-menu
             result
             key
             name
             index
             classes
             namespaces)))
        ;; add enums and delegates to main result list, as own items.
        ;; We don't support nested types. EOS.
        ;;
        ;; This has the issue that they get reported as "function" in
        ;; `helm-imenu', but there's nothing we can do about that.
        ;; The alternative is making it a menu with -1- submenu which
        ;; says "( top )" but that will be very clicky...
        ;; before adding delegates, we need to pad the entry so that it
        ;; matches the "<type> <name>" signature used by all the other
        ;; imenu entries
        (let ((delegates (cdr (assoc "delegate" index))))
          (dolist (delegate delegates)
            (setf
             (car delegate)
             (concat
              "delegate "
              (car delegate)))))
        (dolist (type '("enum" "delegate"))
          (dolist (item (cdr (assoc type index)))
            (let ((item-name (csharp--imenu-get-class-name
                              item
                              namespaces)))
              (setq result
                    (cons (cons item-name (cdr item))
                          result)))))
        ;; sort individual sub-lists
        (dolist (item result)
          (when (listp (cdr item))
            (setf
             (cdr item)
             (csharp--imenu-sort (cdr item)))))
        ;; sort main list
        ;; (Enums always sort last though, because they dont have
        ;; sub-menus)
        (csharp--imenu-sort result)))
    (defun csharp--imenu-create-index-function ()
      "Create an imenu index."
      (csharp--imenu-transform-index
       (imenu--generic-function
        csharp--imenu-expression)))
    (defun csharp--setup-imenu ()
      "Set up `imenu' for `csharp-mode'."
      ;; There are two ways to do imenu indexing. One is to provide a
      ;; function, via `imenu-create-index-function'.  The other is to
      ;; provide imenu with a list of regexps via
      ;; `imenu-generic-expression'; imenu will do a "generic scan" for you.
      ;;
      ;; We use both.
      ;;
      ;; First we use the `imenu-generic-expression' to build a index for
      ;; us, but we do so inside a `imenu-create-index-function'
      ;; implementation which allows us to tweak the results slightly
      ;; before returning it to Emacs.
      (setq imenu-create-index-function
            #'csharp--imenu-create-index-function)
      (imenu-add-menubar-index))
    ;; imenuにアクセス指定子表示するアドバイス
    (defun csharp--imenu-format-item-node-advice (item type)
      "Format an ITEM with a specified TYPE as an imenu item to be inserted into the index."
      (goto-char (cdr item))
      (re-search-forward
       (regexp-opt
        '("public"
          "private"
          "protected"
          "internal"
          "static"
          "sealed"
          "partial"
          "override"
          "virtual"
          "abstract"
          "async"
          "new"
          "unsafe")))
      (cons (concat
             "("
             type
             ") "
             (match-string 0)
             " "
             (car item))
            (cdr item)))
    (advice-add
     'csharp--imenu-format-item-node
     :override #'csharp--imenu-format-item-node-advice)
    (defun my-csharp-mode-setup nil
      (omnisharp-mode)
      (flycheck-mode)
      (setq indent-tabs-mode nil)
      (setq c-syntactic-indentation
            t)
      (c-set-style "ellemtel")
      (setq c-basic-offset 4)
      (setq truncate-lines t)
      (setq tab-width 4)
      (setq evil-shift-width 4)
      ;; set your omnisharp execute path
      ;; (customize-variable 'omnisharp-server-executable-path)
      (csharp--setup-imenu)
      (local-set-key
       (kbd "C-j")
       'omnisharp-go-to-definition-ex)
      (local-set-key
       (kbd "C-S-j")
       'omnisharp-find-usages-ivy)
      (local-set-key
       (kbd "C-h a")
       'omnisharp-run-code-action-refactoring))
    (defun omnisharp-go-to-definition-ex nil
      (interactive)
      (if (bounds-of-thing-at-point
           'word)
          (omnisharp-go-to-definition)
        (pop-tag-mark)))
    (defun omnisharp--choose-and-go-to-quickfix-ivy (quickfixes &optional other-window)
      (let* ((cands (--map-indexed
                     (let ((this-quickfix-filename (cdr (assoc 'FileName it)))
                           (this-quickfix-line (cdr (assoc 'Line it)))
                           (this-quickfix-text (cdr (assoc 'Text it))))
                       (concat
                        (number-to-string it-index)
                        " "
                        this-quickfix-filename
                        ":"
                        (int-to-string
                         this-quickfix-line)
                        "\t"
                        this-quickfix-text))
                     (omnisharp--vector-to-list
                      quickfixes))))
        (ivy-read
         "Omni-Reference: "
         cands
         :action (lambda (chosen-quickfix)
                   (omnisharp-go-to-file-line-and-column
                    (nth (string-to-number (car (split-string chosen-quickfix))) (omnisharp--vector-to-list
                                                                                  quickfixes))
                    other-window))
         :caller 'omnisharp-find-usages-ivy)))
    (defun omnisharp--show-or-navigate-to-quickfixes-with-ivy (quickfix-response &optional other-window)
      (-let
          (((&alist 'QuickFixes quickfixes)
            quickfix-response))
        (cond ((equal 0 (length quickfixes))
               (omnisharp--message
                "No implementations found."))
              ((equal 1 (length quickfixes))
               (omnisharp-go-to-file-line-and-column
                (-first-item
                 (omnisharp--vector-to-list
                  quickfixes))
                other-window))
              (t
               (omnisharp--choose-and-go-to-quickfix-ivy
                quickfixes
                other-window)))))
    (defun omnisharp-find-usages-ivy (&optional other-window)
      (interactive "P")
      (omnisharp--send-command-to-server
       "findusages"
       (omnisharp--get-request-object)
       (lambda (quickfix-response)
         (omnisharp--show-or-navigate-to-quickfixes-with-ivy
          quickfix-response))))
    ;; csファイルからserver-startしたときにsln見失うbugfix
    (defun omnisharp--do-server-start-advice (orig-func &rest args)
      "temporary change default-directory to path-to-project"
      (let ((default-directory (file-name-directory
                                (car args))))
        (apply orig-func args)))
    (advice-add
     'omnisharp--do-server-start
     :around 'omnisharp--do-server-start-advice))
  (leaf open-in-msvs :ensure t))

(leaf nxml-mode
  :doc "xml, xaml用設定"
  :mode "\\.xaml\\'"
  :bind ((nxml-mode-map
          ("C-c C-o" . hs-toggle-hiding))
         (nxml-mode-map
          ("C-c C-l" . hs-hide-level))
         (nxml-mode-map
          ("C-c C-a" . hs-show-all)))
  :mode (("\\.xaml\\'" . nxml-mode))
  :config ;; エラー行にマークがつくようにadvice
  (defun rng-mark-error-advice (message beg end)
    (let ((overlays (overlays-in beg end))
          (continue t))
      (while (and overlays continue)
        (let ((o (car overlays)))
          (when (and (eq (overlay-get o 'category)
                         'rng-error)
                     (= (overlay-start o) beg)
                     (= (overlay-end o) end))
            (overlay-put
             o
             'before-string
             (propertize
              "!"
              'display
              (list
               'left-fringe
               'right-triangle)))
            (setq continue nil)))
        (setq overlays (cdr overlays)))))
  (advice-add
   'rng-mark-error
   :after 'rng-mark-error-advice)
  ;; エラー箇所に波下線を出すface設定
  (set-face-attribute
   'rng-error
   nil
   :underline '(:color "red" :style wave))
  (setq nxml-slash-auto-complete-flag
        t)
  (add-hook
   'nxml-mode-hook
   '(lambda
      nil
      (hs-minor-mode 1)
      (setq nxml-child-indent 4)))
  (add-to-list
   'hs-special-modes-alist
   '(nxml-mode
     "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
     ""
     "<!--"
     nxml-forward-element
     nil)
   nil
   'eq))

(leaf vue-mode
  :doc "vue用設定"
  :ensure t
  :mode (("\\.vue\\'" . vue-mode))
  :hook ((vue-mode-hook . setup-vue-auto-fix)
         (vue-mode-hook . lsp))
  :custom ((js-indent-level . 2))
  :advice (:around flycheck-buffer flycheck-vue-advice)
  :config
  (defun flycheck-vue-advice (org-func)
    "vue-mode(mmm-mode)でflycheckを各セクションで実行すると
実行しているセクションのmajor-modeのchekcerが発動するので
major-modeを一時的に親であるvue-modeに設定して、完了後戻す暫定対応
恒久対策はflycheck-may-use-checkerにmmm-modeやpolymodeの時に
親モードを参照する機能の追加+カスタム可能にする"
    (let ((backup-major-mode major-mode))
      (if (not (string= (file-name-extension buffer-file-name) "vue"))
          (funcall org-func)
        (setq major-mode 'vue-mode)
        (funcall org-func)
        (setq major-mode
              backup-major-mode))))
  (defun setup-vue-auto-fix ()
    (setq-local
     auto-fix-command
     "eslint")
    (setq-local
     auto-fix-option
     "--fix")
    (auto-fix-mode 1)))

(leaf add-node-modules-path
  :ensure t
  :config (add-hook
           'vue-mode-hook
           #'add-node-modules-path))

(leaf js2-mode
  :doc "javascript用設定 linter: npm i eslint"
  :ensure t
  :mode (("\\.js\\'" . js2-mode))
  :hook ((js2-mode-hook . flycheck-mode)
         (js2-mode-hook . setup-js-auto-fix))
  :config
  (defun setup-js-auto-fix ()
    (setq-local
     auto-fix-command
     "eslint")
    (setq-local
     auto-fix-option
     "--fix")
    (auto-fix-mode 1)))


(leaf json-mode
  :doc "Json用設定 linter: npm i jsonlint"
  :ensure t
  :hook ((json-mode-hook . flycheck-mode)
         (json-mode-hook . setup-json-auto-fix))
  :config
  (defun setup-json-auto-fix ()
    (setq-local
     auto-fix-command
     "jsonlint")
    (setq-local
     auto-fix-option
     "-i")
    (auto-fix-mode 1)))

(leaf *python-mode
  :doc "python用設定"
  :hook (python-mode-hook . (lambda ()
                              (local-set-key
                               (kbd "<f5>")
                               'my-pdb)
                              (lsp)))
  :preface
  (defun my-pdb ()
    (interactive)
    (pdb
     (concat
      "python -m pdb "
      (buffer-file-name
       (current-buffer)))))
  :config
  (leaf
    pyvenv
    :ensure t
    :hook (python-mode-hook . (lambda ()
                                (pyvenv-mode 1)
                                (pyvenv-activate "venv")))))

(leaf go-mode
  :doc "Go言語用設定 lsp-server: go install golang.org/x/tools/gopls@latest"
  :ensure t
  :hook ((go-mode-hook . setup-go-auto-fix)
         (go-mode-hook . lsp))
  :custom ((tab-width . 2))
  :config
  (defun setup-go-auto-fix ()
    (setq-local
     auto-fix-command
     "gofmt")
    (setq-local
     auto-fix-option
     "-w")
    (auto-fix-mode 1)))

(leaf *cpp
  :doc "C++用設定"
  :config ;; todo: coding style 4 tab etc...
  (leaf
    counsel-gtags
    :ensure t
    :config (add-hook
             'c-mode-hook
             'counsel-gtags-mode)
    ;; key bindings
    (add-hook
     'counsel-gtags-mode-hook
     '(lambda
        ()
        (local-set-key
         (kbd "C-j")
         'counsel-gtags-dwim-ex)
        (local-set-key
         (kbd "C-c t")
         'counsel-gtags-find-definition)
        (local-set-key
         (kbd "C-c r")
         'counsel-gtags-find-reference)
        (local-set-key
         (kbd "C-c s")
         'counsel-gtags-find-symbol)
        (local-set-key
         (kbd "C-c h")
         'counsel-gtags-find-header-or-source)
        (local-set-key
         (kbd "C-c b")
         'counsel-gtags-go-backward)))
    (defun counsel-gtags-dwim-ex ()
      (interactive)
      (if (bounds-of-thing-at-point
           'word)
          (counsel-gtags-dwim)
        (counsel-gtags-go-backward)))
    (defun counsel-gtags-find-header-or-source ()
      (interactive)
      (let ((prefix (nth 0 (split-string
                            (buffer-name)
                            "\\.")))
            (suffix (nth 1 (split-string
                            (buffer-name)
                            "\\.")))
            (buffer (buffer-name))
            (target ""))
        (progn
          (cond ((string= suffix "c")
                 (setq target ".h"))
                ((string= suffix "cpp")
                 (setq target ".h"))
                ((string= suffix "cxx")
                 (setq target ".h"))
                ((string= suffix "h")
                 (setq target ".c"))
                ((string= suffix "hpp")
                 (setq target ".cpp"))
                ((string= suffix "hxx")
                 (setq target ".cxx")))
          ;;別windowに出ない場合は以下を有効に
          ;;(other-window 1)
          ;;(switch-to-buffer buffer)
          (counsel-gtags-find-file
           (concat prefix target)))))))

(leaf arduino-mode
  :doc "ino(Arduino)用設定"
  :disabled t
  :custom ((arduino-executable . "arduino_debug")
           (flycheck-arduino-executable . "arduino_debug"))
  :config (defun my-arduino-mode-hook nil
            (flycheck-arduino-setup)
            (defvar-local flycheck-check-syntax-automatically '(save))
            (flycheck-mode))
  (add-hook
   'arduino-mode-hook
   'my-arduino-mode-hook))

(leaf org
  :bind ((org-mode-map
          ("C-c M-o" . ace-link-org)))
  :setq ((org-plantuml-jar-path . "~/.emacs.d/lib/plantuml.jar"))
  :config (org-babel-do-load-languages
           'org-babel-load-languages
           '((plantuml . t))))

(leaf crowi
  :el-get hirocarma/emacs-crowi)

(leaf auto-fix
  :el-get "tomoya/auto-fix.el"
  :hook ((auto-fix-mode-hook . setup-auto-fix))
  :config
  (defun setup-auto-fix ()
    (add-hook
     'before-save-hook
     #'auto-fix-before-save)))

;;; Utilities
(leaf hideshow
  :doc "折り畳み機能のパッケージ"
  :ensure t
  :hook ((c-mode-common-hook . hs-minor-mode)
         (emacs-lisp-mode-hook . hs-minor-mode)
         (java-mode-hook . hs-minor-mode)
         (lisp-mode-hook . hs-minor-mode)
         (parl-mode-hook . hs-minor-mode)
         (sh-mode-hook . hs-minor-mode))
  :bind ((hs-minor-mode-map
          ("C-i" . hs-toggle-hiding))))

(leaf *shell
  :doc "M-x shell で新しいバッファを作るようにadvice"
  :config (defun shell-advice (org-func &rest args)
             (funcall
              org-func
              (generate-new-buffer-name
               "*shell*")))
  :advice (:around shell shell-advice))

(leaf *dired
  :doc "diredでファイルオープンやディレクトリ移動で新しいバッファ開かない設定など"
  :config (leaf
            all-the-icons-dired
            :ensure t)
  (leaf
    *windows
    :if (or (eq system-type 'windows-nt)
            (and (eq system-type 'gnu/linux)
                 (file-exists-p
                  "/proc/sys/fs/binfmt_misc/WSLInterop")))
    :config (defun dired-open-file ()
              "In dired, open the file named on this line."
              (interactive)
              (let* ((file (if (eq system-type 'windows-nt)
                               (dired-get-filename)
                             (shell-command-to-string
                              (concat
                               "wslpath -w"
                               " "
                               (dired-get-filename)))))
                     (open (if (eq system-type 'windows-nt)
                               "start"
                             "explorer.exe")))
                (message "Opening %s..." file)
                (shell-command
                 (mapconcat
                  #'shell-quote-argument
                  (list open file)
                  "
"))
                ;;(call-process "gnome-open" nil 0 nil file)
                (message
                 "Opening %s done"
                 file)))
    (add-hook
     'dired-mode-hook
     '(lambda
        ()
        (define-key dired-mode-map "\C-o"
          'dired-open-file)
        (define-key dired-mode-map "\C-l"
          'dired-up-directory)
        (all-the-icons-dired-mode))))
  (leaf
    *darwin
    :if (eq system-type 'darwin)
    :config (defun open-mac (path)
              (start-process
               "dired-open-mac"
               nil
               "open"
               path))
    (defun quicklook-file (path)
      (interactive)
      (defvar cur nil)
      (defvar old nil)
      (setq old cur)
      (setq cur
            (start-process
             "ql-file"
             nil
             "qlmanage"
             "-p"
             path))
      (when old (delete-process old)))
    (defun my-dired-open ()
      (interactive)
      (let ((exts-ql '("jpeg" "jpg" "png" "gif"))
            (exts-open '("avi" "mkv" "mp4" "pdf")))
        (cond ((file-accessible-directory-p
                (dired-get-file-for-visit))
               (call-interactively
                'dired-find-alternate-file))
              ((member
                (downcase
                 (file-name-extension
                  (dired-get-file-for-visit)))
                exts-ql)
               (funcall
                'quicklook-file
                (dired-get-file-for-visit)))
              ((member
                (downcase
                 (file-name-extension
                  (dired-get-file-for-visit)))
                exts-open)
               (funcall
                'open-mac
                (dired-get-file-for-visit)))
              (t
               (call-interactively
                'dired-find-file-other-window)))))
    (add-hook
     'dired-mode-hook
     '(lambda
        ()
        (define-key dired-mode-map "\C-o"
          'my-dired-open))))
  (leaf
    *common
    :config ;; dired-find-alternate-file の有効化
    (put
     'dired-find-alternate-file
     'disabled
     nil)
    ;; ファイルなら別バッファで、ディレクトリなら同じバッファで開く
    (defun dired-open-in-accordance-with-situation ()
      (interactive)
      (let ((file (dired-get-filename)))
        (if (file-directory-p file)
            (dired-find-alternate-file)
          (dired-find-file))))
    (add-hook
     'dired-mode-hook
     '(lambda
        ()
        ;; RET 標準の dired-find-file では dired バッファが複数作られるので
        ;; dired-find-alternate-file を代わりに使う
        (define-key dired-mode-map (kbd "RET")
          'dired-open-in-accordance-with-situation)
        (define-key dired-mode-map (kbd "a")
          'dired-find-file)))))

(leaf *ediff
  :doc "ediff で水平2分割で表示し、制御用のframeを生成させない"
  :custom ((ediff-window-setup-function . 'ediff-setup-windows-plain)
           (ediff-split-window-function . 'split-window-horizontally)
           (ediff-current-diff-overlay-A . t)
           (ediff-current-diff-overlay-B . t)))

(leaf *vc-dir
  :doc "vc-dirでunregisteredをデフォルトで非表示にするadvice"
  :config (defun vc-dir-advice (&rest args)
             (vc-dir-hide-state 'unregistered))
  :advice (:after vc--process-sentinel vc-dir-advice))

(leaf magit
  :doc "magit statusでstash,untrackedセクションの非表示を設定"
  :ensure t
  :custom ((magit-section-initial-visibility-alist . '((stashes . hide)
                                                       (untracked . hide)))))

(leaf migemo
  :doc "検索の際に日本語をローマ字読みでヒットさせるパッケージ"
  :ensure t
  :custom ((migemo-command . "cmigemo")
           (migemo-options . '("-q" "--emacs" "-i" "\a"))
           (migemo-user-dictionary . nil)
           (migemo-regex-dictionary . nil)
           (migemo-use-pattern-alist . t)
           (migemo-use-frequent-pattern-alist . t)
           (migemo-pattern-alist-length . 1000)
           (migemo-coding-system . 'utf-8-unix))
  :config (setq migemo-dictionary
                (expand-file-name
                 "~/.emacs.d/lib/dict/utf-8/migemo-dict"))
  (load-library "migemo")
  (migemo-init))

(leaf ripgrep
  :doc "Grepの改良版 ripgrepのemacsクライアントパッケージ"
  :ensure t
  :custom ((ripgrep-executable . "rg")
           (ripgrep-arguments . '("-S"))))

(leaf ssh
  :doc "sshでshellを実行するためのパッケージ"
  :ensure t
  :custom ((ssh-directory-tracking-mode . t)
           (dirtrackp .nil))
  :config (with-eval-after-load
              'ssh
            (shell-dirtrack-mode t)))

(leaf search-web
  :el-get tomoya/search-web.el
  :config
  (require 'search-web))

(leaf google-translate
  :doc "Google翻訳パッケージ"
  :ensure t
  :bind (("C-x t" . google-translate-enja-or-jaen))
  :config (defvar google-translate-english-chars "[:ascii:]’“”–"
            "これらの文字が含まれているときは英語とみなす")
  (defun google-translate-enja-or-jaen (&optional string)
    "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
    (interactive)
    (setq string
          (cond ((stringp string) string)
                (current-prefix-arg
                 (read-string
                  "Google Translate: "))
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
                     (buffer-substring s (point)))))))
    (let* ((asciip (string-match
                    (format
                     "\\`[%s]+\\'"
                     google-translate-english-chars)
                    string)))
      (run-at-time
       0.1
       nil
       'deactivate-mark)
      (google-translate-translate
       (if asciip "en" "ja")
       (if asciip "ja" "en")
       string
       'current-buffer)))
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130)))

(leaf eww
  :disabled t
  :bind ((eww-mode-map
          ("e" . ace-link-eww)))
  :config ;;; デフォルトの設定(参考)
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
          (while (setq skip
                       (text-property-not-all
                        skip
                        (point-max)
                        'help-echo
                        nil))
            (goto-char skip)
            (push skip candidates)
            (setq skip
                  (next-single-property-change
                   (point)
                   'help-echo)))
          (nreverse candidates))))))

(leaf *xwidget-webkit
  :disabled t
  :hook ((xwidget-webkit-mode-hook . xwidget-webkit-mode-hook-func))
  :config (defun xwidget-webkit-mode-hook-func nil
            (remove-hook
             'kill-buffer-query-functions
             #'xwidget-kill-buffer-query-function)
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
            (local-unset-key (kbd "C-x 2"))
            (local-unset-key (kbd "C-x 3")))
  (defvar xwidget-webkit-search-word-prefix "https://duckduckgo.com/?q="
    "Search engine url and search request that before searching word.")
  (defvar xwidget-webkit-search-word-suffix "&kp=-1&kl=jp-jp"
    "Search engine option parameter that after searching word.")
  (defun xwidget-webkit-search-word (arg word)
    "Searching from `xwidget-webkit-search-word-prefix' search engine.
And searching with `xwidget-webkit-search-word-suffix' search engine options.
Interactively, WORD defaults to the string looking like a word around point.
If setting prefix args (C-u), reuses session(buffer). Normaly session(buffer) create."
    (interactive (list
                  current-prefix-arg
                  (read-string
                   "search-word: "
                   (cond ((thing-at-point 'word))
                         (t "")))))
    (let ((encoded-url (url-encode-url
                        (concat
                         xwidget-webkit-search-word-prefix
                         word
                         xwidget-webkit-search-word-suffix))))
      (xwidget-webkit-browse-url
       encoded-url
       (not arg))))
  ;; (defun switch-buffer-functions-func (prev cur)
  ;;   (if (string= "xwidget-webkit-mode"
  ;;   	       (buffer-local-value 'major-mode
  ;; 				   (get-buffer (or prev "*scratch*"))))
  ;;     ))
  ;; (add-hook 'switch-buffer-functions #'switch-buffer-functions-func)
  (add-hook
   'xwidget-webkit-mode-hook
   'xwidget-webkit-mode-hook-func)
  (defun xwidget-webkit-mode-hook-func ()
    (remove-hook
     'kill-buffer-query-functions
     #'xwidget-kill-buffer-query-function)
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
    (local-unset-key (kbd "C-x 2"))
    (local-unset-key (kbd "C-x 3")))
  (defvar xwidget-webkit-search-word-prefix "https://duckduckgo.com/?q="
    "Search engine url and search request that before searching word.")
  (defvar xwidget-webkit-search-word-suffix "&kp=-1&kl=jp-jp"
    "Search engine option parameter that after searching word.")
  (defun xwidget-webkit-search-word (arg word)
    "Searching from `xwidget-webkit-search-word-prefix' search engine.
And searching with `xwidget-webkit-search-word-suffix' search engine options.
Interactively, WORD defaults to the string looking like a word around point.
If setting prefix args (C-u), reuses session(buffer). Normaly session(buffer) create."
    (interactive (list
                  current-prefix-arg
                  (read-string
                   "search-word: "
                   (cond ((thing-at-point 'word))
                         (t "")))))
    (let ((encoded-url (url-encode-url
                        (concat
                         xwidget-webkit-search-word-prefix
                         word
                         xwidget-webkit-search-word-suffix))))
      (xwidget-webkit-browse-url
       encoded-url
       (not arg)))))

(leaf eaf
  :el-get emacs-eaf/emacs-application-framework
  :config
  (require 'eaf)
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-terminal)
  (add-hook 'eaf-mode-hook '(lambda () (company-mode -1)))
  (defun search-eaf-browser (word)
    (interactive "sSearch-word: ")
    (eaf-open-browser (format "http://www.google.com/search?q=%s" (url-hexify-string word))))
  (eaf-bind-key nil "C-t" eaf-browser-keybinding))

;;; init.el ends here
