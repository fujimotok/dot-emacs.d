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
        ; renew local melpa cache if fail
        (error
         (package-refresh-contents)
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
         ((kbd "C-<backspace>") . backward-delete-word)
         ((kbd "C-;") . hs-toggle-hiding)
         ((kbd "C-d") . forward-delete-char)
         ((kbd "C-z") . undo))
  :custom `((scroll-preserve-screen-position . t)
            (ring-bell-function . 'ignore))
  :config
  (set-default-coding-systems 'utf-8-unix) ;; 文字コードのデフォルト設定
  (setq-default indent-tabs-mode nil)      ;; インデントは絶対スペースで
  (setq-default truncate-lines t)          ;; 常に折り返し表示しない
  (recentf-mode t)                         ;; 開いたファイル履歴を有効に
  (savehist-mode)                          ;; ミニバッファの履歴を有効に
  (global-auto-revert-mode)                ;; 外部からの変更を自動読み込みを有効に
  (global-hl-line-mode t)                  ;; 現在行をハイライト
  (electric-pair-mode)                     ;; 対応する括弧を自動入力
  (show-paren-mode t)                      ;; 対応する括弧をハイライト
  (setq show-paren-style 'expression)      ;; 対応する括弧をハイライトで括弧の間もハイライト

  ;; 自動保存機能
  (setq backup-directory-alist
        '((".*" . "~/.emacs.d/auto-save")))
  (setq version-control t)
  (setq kept-new-versions 5)
  (setq kept-old-versions 1)
  (setq delete-old-versions t)
  (setq create-lockfiles nil)

  ;; フォント設定
  (set-frame-font "ricty diminished-10.5"))

(leaf *hs-minor-mode
  :doc "折り畳み・展開機能"
  :bind (((kbd "C-;") . hs-toggle-hiding))
  :hook ((prog-mode-hook . hs-minor-mode)))


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
     (set-process-coding-system
      'sjis-dos
      'sjis-dos)))
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
  (w32-ime-initialize)))


;;; Appearance settings
(leaf doom-themes
  :doc "doomテーマのロード"
  :ensure t
  :config
  (load-theme 'doom-dracula t)
  (setq-default left-fringe-width 20)
  (set-face-attribute
   'fringe
   nil
   :background "#2a2c38"
   :foreground "#888882"))

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
  :config
  (when (window-system)
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
  :hook ((prog-mode-hook . rainbow-delimiters-mode)))

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
    :config
    (defun my-consult-ripgrep (dir)
        (interactive "Drgrep dir: ")
        (consult-ripgrep dir)))
  (leaf embark
    :ensure t
    :bind (("M-a". embark-act))
    :custom ((embark-indicators . '(embark-minimal-indicator embark-highlight-indicator))))
  )


;;; Programming support settings
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
    :hook ((company-mode-hook . company-box-mode)))
  (leaf company-quickhelp
    :ensure t
    :hook ((company-box-mode-hook . company-quickhelp-mode))
    :custom ((company-quickhelp-delay . 1)))
  (global-company-mode))

(leaf flycheck
  :custom ((flycheck-check-syntax-automatically . '(mode-enabled save))
           (flycheck-idle-change-delay . 2)))

(leaf *gud-mode
  :config
  (declare-function gud-print     "gud" (arg))
  (defun gud-print-at-symbol ()
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

(leaf auto-fix
  :el-get "tomoya/auto-fix.el"
  :hook ((auto-fix-mode-hook . setup-auto-fix))
  :config
  (defun setup-auto-fix ()
    (add-hook
     'before-save-hook
     #'auto-fix-before-save)))

(leaf treesit
  :doc "Emacs biltin tree-sitter. You need to rename tree-sitter-langs/bin/<lang> to libtree-sitter-<lang>, and set PATH env."
  ;; package install tree-sitter-langs
  ;; tree-sitter-langs は1回だけインストールしてbinをコピーした後は消す
  )


;;; Programming langages settings
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
  (defun markdown-insert-dwin (&optional arg)
    (interactive "p")
    (let ((l (thing-at-point 'line)))
      (if (string-match "^[ \t]*[-]" l)
          (markdown-insert-list-item arg)
        (move-end-of-line 1)
        (markdown-insert-header-dwim)
        ))))

(leaf nxml-mode
  :doc "xml, xaml用設定"
  :mode "\\.xaml\\'"
  :hook ((nxml-mode-hook . hs-minor-mode))
  :bind ((nxml-mode-map
          ("C-c C-o" . hs-toggle-hiding)
          ("C-c C-l" . hs-hide-level)
          ("C-c C-a" . hs-show-all)))
  :custom ((nxml-child-indent . 4)
           (nxml-slash-auto-complete-flag . t))
  :custom-face
  ;; エラー箇所に波下線を出すface設定
  ((rng-error . '((t (:underline (:color "red" :style wave))))))
  :config
  ;; hs-minor-modeでxmlに対応するように設定
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

(leaf *csharp
  :doc "C#用設定"
  :config
  ;; scoop install omnisharp + eglot
  (leaf
    csharp-mode
    :ensure t
    :hook ((csharp-mode-hook . eglot-ensure))
    :bind ((csharp-mode-map
            ("C-x C-e" . my-csharp-shell-send-region)
            ("<C-return>" . my-csharp-shell-send-line))))
  (leaf open-in-msvs :ensure t))

(leaf *java-script
  :config
  (leaf add-node-modules-path
    :doc "現在のバッファでnodeのmodulesへのパスを設定"
    :ensure t
    :hook ((vue-mode-hook . add-node-modules-path)))

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
      (auto-fix-mode 1))))

(leaf python-mode
  :doc "python用設定"
  :hook (python-mode-hook . my-python-mode-hook)
  :bind ((:python-mode-map
          :package python
          ("<f5>" . my-pdb)
          ("<C-return>" . my-python-shell-send-line)
          ("C-x C-e" . my-python-shell-send-region)))
  :preface
  (defun my-python-mode-hook ()
    ;; pip install python-lsp-server が必要
    ;; pyvenv-activate した後に再度 M-x eglot が必要
    (eglot-ensure))
  (defun my-pdb ()
    (interactive)
    (pdb
     (concat
      "python -m pdb "
      (buffer-file-name
       (current-buffer)))))
  ;; IPythonが使えるならrun-pythonはipythonを使う
  (if (executable-find "ipython")
    (setq python-shell-interpreter
          "ipython"
          python-shell-interpreter-args
          "-i --simple-prompt --InteractiveShell.display_page=True")
    ;; Emacs 29 からは普通のpythonの時も指定が必要？
    (setq python-shell-interpreter
          "python"))
  :config
  (leaf pyvenv
    :ensure t
    :config
    (pyvenv-mode 1)))
              
(leaf powershell
  :doc "powershell用設定"
  :ensure t
  :hook ((powershell-mode-hook . eglot-ensure))
  :bind ((powershell-mode-map
          ("<C-return>" . my-powershell-shell-send-line)
          ("C-x C-e" . my-powershell-shell-send-region)))
  :config
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     `(powershell-mode . ,(eglot-powershell-lsp-command)))))

(leaf *clojure-mode
  :doc "needs cljstyle https://github.com/greglook/cljstyle"
  :custom ((cljstyle-exec-path . "cljstyle.jar"))
  :config
  (add-hook 'clojure-mode-hook 'setup-clojure-auto-fix)
  (add-hook 'clojurescript-mode-hook 'setup-clojure-auto-fix)
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

(leaf arduino-mode
  :doc "ino(Arduino)用設定"
  :disabled t
  :custom ((arduino-executable . "arduino_debug")
           (flycheck-arduino-executable . "arduino_debug"))
  :hook ((python-mode-hook . my-arduino-mode-hook))
  :config
  (defun my-arduino-mode-hook ()
            (flycheck-arduino-setup)
            (defvar-local flycheck-check-syntax-automatically '(save))
            (flycheck-mode)))


;;; Utilities
(leaf dired
  :doc "diredでファイルオープンやディレクトリ移動で新しいバッファ開かない設定など"
  :custom ((dired-dwim-target . t))
  :hook ((dired-mode-hook . my-dired-mode-hook))
  :bind ((dired-mode-map
          ("<return>" . dired-open-in-accordance-with-situation)
          ("/" . dired-narrow-regexp)))
  :init
  (defun dired-open-in-accordance-with-situation ()
    (interactive)
    (let ((file (condition-case err
                    (dired-get-filename)
                  (error "../"))))
      (if (file-directory-p file)
          (dired-find-alternate-file)
        (dired-find-file))))
  (defun my-dired-mode-hook ()
        (all-the-icons-dired-mode t))
  :config
  (leaf dired-narrow
    :doc "絞り込み"
    :ensure t)
  (leaf dired-subtree
    :doc "i でディレクトリを展開"
    :ensure t)
  (leaf all-the-icons-dired
    :doc "アイコン表示"
    :ensure t)
  )

(leaf *ediff
  :doc "ediff で水平2分割で表示し、制御用のframeを生成させない"
  :custom ((ediff-window-setup-function . 'ediff-setup-windows-plain)
           (ediff-split-window-function . 'split-window-horizontally)
           (ediff-current-diff-overlay-A . t)
           (ediff-current-diff-overlay-B . t)))

(leaf diffview
    :doc "パッチをWinMergeのようなside-by-side表示する"
    :ensure t)

(leaf vc-dir
  :doc "vc-dirでunregisteredをデフォルトで非表示にするadvice"
  :config
  (declare-function vc-dir-hide-state "vc-dir")
  (defun vc-dir-advice (&rest args)
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
  :doc "ブラウザで検索実行"
  :el-get tomoya/search-web.el
  :bind (("C-x s" . search-web-dwim))
  :config
  (require 'search-web)
  (add-to-list
   'search-web-engines
   '("bing" "https://www.bing.com/search?q=%s" nil)))

(leaf google-translate
  :doc "Google翻訳パッケージ"
  :ensure t
  :bind (("C-x t" . google-translate-enja-or-jaen))
  :config
  (declare-function google-translate-translate "google-translate-core-ui")
  (defvar google-translate-english-chars "[:ascii:]’“”–"
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

(leaf atomic-chrome
  :doc "chrome拡張 ghosttext入れることでブラウザの入力をEmacsから行う"
  :ensure t)

(leaf *aichat
  :doc "bing aiと対話"
  :el-get xhcoding/emacs-aichat
  :custom ((aichat-bingai-cookies-file . "~/.emacs.d/aichat-bingai-cookies-file.json")))

;;; init.el ends here
