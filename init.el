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
         ((kbd "C-;") . hs-toggle-hiding)
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
  (setq-default left-fringe-width 20)
  (set-frame-font "ricty diminished-10.5")
  (set-face-attribute
   'fringe
   nil
   :background "#2a2c38"
   :foreground "#888882")
  (add-hook
   'prog-mode-hook
   #'hs-minor-mode))

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
   '("csi" . sjis-dos))
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

(leaf all-the-icons
  :ensure t
  :custom ((all-the-icons-scale-factor . 1.0)))

(leaf *mode-line
  :doc "モードラインの設定"
  :custom
  ((eol-mnemonic-dos . "↲")
   (eol-mnemonic-unix . "↓")
   (eol-mnemonic-mac . "←")
   (eol-mnemonic-undecided . "・"))
  :config
  (require 'mode-line)
  (setq-default mode-line-format
                `(" "
                  (:propertize " " display (space . (:width (1) :height (22) :ascent (16))))
                  (:eval w32-ime-mode-line-state-indicator)
                  "%Z" ; 文字コード改行コード
                  "%* " ; 変更有無
                  mml-mode-name
                  (:propertize " %b " face (:weight bold))
                  "(%l:%c) "
                  mml-branch-icon
                  vc-mode
                  " "
                  ,@mml-checker
                  ))
  (set-face-attribute 'mode-line nil :background "#714069"))



(leaf *titlebar
  :doc "タイトルバーに時計などを表示"
  :config (when (window-system)
            ;; バッファがファイルのときはフルパス、でなければバッファ名表示
            (setq frame-title-format
                  '("" (:eval
                        (if (buffer-file-name)
                            " %f"
                          " %b"))
                    ))))

(leaf rainbow-delimiters
  :doc "カッコに色をつけるパッケージ"
  :ensure t
  :config (define-globalized-minor-mode
            global-rainbow-delimiters-mode
            rainbow-delimiters-mode
            rainbow-delimiters-mode)
  (global-rainbow-delimiters-mode t))

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
           ("C-S-r" . my-consult-ripgrep)
           ("M-g g" . consult-goto-line))
    :init
    (defun my-consult-ripgrep (dir)
        (interactive "Drgrep dir: ")
        (consult-ripgrep dir)))
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
   (lsp-document-sync-method . 2)
   (lsp-response-timeout . 5)
   (read-process-output-max . 10485760))
  :bind ((lsp-mode-map
          ("C-c C-r" . lsp-rename)))
  :config
  (leaf
    lsp-ui
    :ensure t
    :doc "LSP UI tools"
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

(leaf markdown-mode
  :doc "markdown用設定"
  :hook ((markdown-mode-hook . (lambda nil (outline-hide-sublevels 2))))
  :bind ((markdown-mode-map
          ("C-<return>" . markdown-insert-dwin)
          ("S-<return>" . markdown-insert-gfm-checkbox)))
  :custom ((markdown-unordered-list-item-prefix . "- ")
           (markdown-fontify-code-blocks-natively . t)
           (markdown-asymmetric-header . t)
           (markdown-code-lang-modes . '(("elisp" . emacs-lisp-mode)
                                         ("sqlite" . sql-mode)
                                         ("c" . c-mode)
                                         ("cpp" . c++-mode)
                                         ("js" . javascript-mode)
                                         ("json" . json-mode)
                                         ("shell" . sh-mode))))
  :config
  (defun markdown-yank-url ()
    (interactive)
    (if (string-match ".*\\(https:\\/\\/.*\\/\\).*" (current-kill 0))
        (let  ((buffer (url-retrieve-synchronously (current-kill 0) nil nil 3))
               str)
          (save-excursion
            (set-buffer buffer)
            (goto-char (point-min))
            (setq str (decode-coding-string
                       (buffer-substring-no-properties (point) (point-max))
                       'utf-8-unix))
            (string-match "<title>\\(.+\\)</title>" str))
          (insert (format "[%s](%s)" (match-string 1 str) (current-kill 0))))
      (message "No url in kill-ring.")))
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
  :config
  ;; omnisharp 1.37.0 + lsp-mode stable 8.0.0
  (leaf
    csharp-mode
    :ensure t
    :hook ((csharp-mode-hook . my-csharp-mode-setup)
           (csharp-mode-hook . lsp))
    :bind ((csharp-mode-map
            ("C-x C-e" . my-csharp-shell-send-region))
           (csharp-mode-map
            ("<C-return>" . my-csharp-shell-send-line)))
    :config 
    (defun my-csharp-mode-setup nil
      (setq indent-tabs-mode nil)
      (setq c-syntactic-indentation t)
      (c-set-style "ellemtel")
      (setq c-basic-offset 4)
      (setq truncate-lines t)
      (setq tab-width 4)
      (setq evil-shift-width 4))
    (leaf open-in-msvs :ensure t)))

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

(leaf typescript-mode
  :doc "tipescript用設定 linter: npm i eslint"
  :ensure t
  :hook ((typescript-mode-hook . lsp)
         (typescript-mode-hook . flycheck-mode)
         (typescript-mode-hook . setup-js-auto-fix))
  :custom ((typescript-indent-level . 2))
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
  :hook (python-mode-hook . my-python-mode-hook)
  :preface
  (defun my-python-mode-hook ()
    (local-set-key (kbd "<f5>") 'my-pdb)
    (local-set-key (kbd "<C-return>") 'my-python-shell-send-line)
    (local-set-key (kbd "C-x C-e") 'my-python-shell-send-region)
    (lsp)
    ;; IPythonが使えるならrun-pythonはipythonを使う
    (when (executable-find "ipython")
      (setq python-shell-interpreter
            "ipython"
            python-shell-interpreter-args
            "-i --simple-prompt --InteractiveShell.display_page=True")))
  (defun my-pdb ()
    (interactive)
    (pdb
     (concat
      "python -m pdb "
      (buffer-file-name
       (current-buffer)))))
  :config
  (leaf pyvenv
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

(leaf auto-fix
  :el-get "tomoya/auto-fix.el"
  :hook ((auto-fix-mode-hook . setup-auto-fix))
  :config
  (defun setup-auto-fix ()
    (add-hook
     'before-save-hook
     #'auto-fix-before-save)))

(leaf *powershell-mode
  :doc "powershell用設定"
  :hook (powershell-mode-hook . my-powershell-mode-hook)
  :preface
  (defun my-powershell-mode-hook ()
    (local-set-key (kbd "<C-return>") 'my-powershell-shell-send-line)
    (local-set-key (kbd "C-x C-e") 'my-powershell-shell-send-region)
    (lsp))
  :config
  (leaf powershell
    :ensure t))

;;; Utilities
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
           (ediff-current-diff-overlay-B . t))
  :config
  (leaf diffview
    :ensure t))

(leaf *vc-dir
  :doc "vc-dirでunregisteredをデフォルトで非表示にするadvice"
  :config (defun vc-dir-advice (&rest args)
             (vc-dir-hide-state 'unregistered))
  :advice (:after vc--process-sentinel vc-dir-advice))

(leaf magit
  :doc "magit statusでstash,untrackedセクションの非表示を設定"
  :ensure t
  :custom ((magit-section-initial-visibility-alist . '((stashes . hide)
                                                       (untracked . hide))))
  :hook (magit-mode-hook . (lambda ()
                             (remove-hook 'server-switch-hook 'magit-commit-diff)
                             (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff))))

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

(leaf *clojure-mode
  :doc "needs cljstyle https://github.com/greglook/cljstyle"
  :config
  (add-hook 'clojure-mode-hook 'setup-clojure-auto-fix)
  (add-hook 'clojurescript-mode-hook 'setup-clojure-auto-fix)
  (defcustom cljstyle-exec-path "cljstyle.jar" "cljstyle executable path.")
  (defun setup-clojure-auto-fix ()
    (setq-local
     auto-fix-command
     "cljstyle")
    (setq-local
     auto-fix-option
     "fix")
    (auto-fix-mode 1))

  (leaf cider
    :ensure t)

  (leaf clj-refactor
    :ensure t)

  (leaf flycheck-clj-kondo
    :doc "needs clj-kondo https://github.com/clj-kondo/clj-kondo"
    :ensure t
    :config
    (require 'flycheck-clj-kondo)
    (add-hook 'clojure-mode-hook 'my-clojure-mode-hook)
    (add-hook 'clojurescript-mode-hook 'my-clojure-mode-hook)
    (defun my-clojure-mode-hook ()
      (flycheck-mode))
    ))

(leaf atomic-chrome
  :ensure t)

(leaf treesit
  :doc "Emacs biltin tree-sitter. You need to rename tree-sitter-langs/bin/<lang> to libtree-sitter-<lang>, and set PATH env."
  ;; package install tree-sitter-langs
  ;; tree-sitter-langs は1回だけインストールしてbinをコピーした後は消す
  )

(leaf *aichat
  :doc "bing aiと対話"
  :el-get xhcoding/emacs-aichat
  :custom ((aichat-bingai-cookies-file . "~/.emacs.d/aichat-bingai-cookies-file.json")))

;;; init.el ends here
