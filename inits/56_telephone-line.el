;; Telephone Line modeを読み込み
(require 'telephone-line)

;; set faces
(defface level1-active
  '((t (:foreground "black" :background "chartreuse" :inherit mode-line))) "")
(defface level1-inactive
  '((t (:foreground "white" :background "grey11" :inherit mode-line-inactive))) "")
(defface level2-active
  '((t (:foreground "black" :background "royal blue" :inherit mode-line))) "")
(defface level2-inactive
  '((t (:foreground "black" :background "grey11" :inherit mode-line-inactive))) "")
(defface level3-active
  '((t (:foreground "gray" :background "magenta" :inherit mode-line))) "")
(defface level3-inactive
  '((t (:foreground "white" :background "grey11" :inherit mode-line-inactive))) "")

(setq telephone-line-faces
      (append '((level1 . (level1-active . level1-inactive))
		(level2 . (level2-active . level2-inactive))
     		(level3 . (level3-active . level3-inactive))
		) telephone-line-faces)
)

;; セパレータが途切れる場合に調整
(setq telephone-line-height 50)

(when (eq system-type 'darwin)
  (setq telephone-line-height 20)
  (setq ns-use-srgb-colorspace nil))

;; mode-lineにnyan-mode使うため
(require 'nyan-mode)
(nyan-mode t)
;; 背景がわかりづらいのでxpm画像を差し替え
;; .emacs.d/elpa/nyan-modeXXXX/img/outspace.xpm non->#222244


;; 自作segment関数
(telephone-line-defsegment* telephone-line-mule-info-segment ()
  '("" mode-line-mule-info "%*"))

(telephone-line-defsegment* telephone-line-flycheck-segment ()
  '("" flycheck-mode-line))

(require 'all-the-icons)
(require 'all-the-icons-ivy)
(telephone-line-defsegment* telephone-line-major-icon-segment ()
  (format "%s %s"
          ;; (propertize (all-the-icons-fileicon (caddr (assoc major-mode all-the-icons-mode-icon-alist)))
          ;;             ;; 'font-face `(:family ,(all-the-icons-fileicon-family) :height 0.5)
          ;;             ;; 'font-lock-face `(:family ,(all-the-icons-fileicon-family) :height 0.5))
          ;;             'face `(:family ,(all-the-icons-fileicon-family) :height 0.5 :v-adjust -0.2))
          (if (buffer-file-name)
              (all-the-icons-ivy-icon-for-file (buffer-name))
              (all-the-icons-ivy--icon-for-mode major-mode))
          ;;(all-the-icons-ivy--icon-for-mode major-mode)
          mode-name))

;;(all-the-icons-match-to-alist)
;;(all-the-icons-ivy-buffer-transformer (buffer-name))
;;(all-the-icons-fileicon (caddr (assoc major-mode all-the-icons-mode-icon-alist)))


(telephone-line-defsegment* telephone-line-my-vc-segment ()
  (when vc-mode
    (cond
      ((string-match "Git[:-]" vc-mode)
        (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
          (concat (propertize (format "")
                              'face `(:foreground "yellow" :height 1.3)
                              'display '(raise -0.1))
                  (propertize (format " %s" branch)
                              'face `(:foreground "yellow" :height 0.9)))))
      ((string-match "SVN-" vc-mode)
        (let ((revision (cadr (split-string vc-mode "-"))))
          (concat (propertize (format "")
                              'face `(:height 1.3)
                              'display '(raise -0.1))
                  (propertize (format " %s" revision)
                              'face `(:height 0.9)))))
      (t (format "%s" vc-mode)))))

;; mule-info改行の表現方法の変更
(setq eol-mnemonic-dos "↲")
(setq eol-mnemonic-unix "↓")
(setq eol-mnemonic-mac "←")
(setq eol-mnemonic-undecided "・")

;; set mode-line contents
(setq telephone-line-lhs
      '((level1 . (telephone-line-major-icon-segment))
        (level2 . (telephone-line-mule-info-segment))
        (nil    . (telephone-line-buffer-name-segment))))
(setq telephone-line-center-rhs
      '((nil . (telephone-line-nyan-segment))))
(setq telephone-line-rhs
      '((nil . nil)
	(level2 . (telephone-line-my-vc-segment
		   telephone-line-process-segment
		   telephone-line-flycheck-segment))
	(level1 . (telephone-line-airline-position-segment))))

;; set separator shape
(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)

;; all configuration must be done before calling (telephone-line-mode 1)
(telephone-line-mode 1)

