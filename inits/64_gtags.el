(require 'helm-config)
(require 'helm-gtags)

(add-hook 'c-mode-hook 'helm-gtags-mode)

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
              (local-set-key (kbd "C-c t") 'helm-gtags-find-tag)
              (local-set-key (kbd "C-c r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "C-c s") 'helm-gtags-find-symbol)
              (local-set-key (kbd "C-c b") 'helm-gtags-pop-stack)))

