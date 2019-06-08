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
(setq telephone-line-height 26)

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

;; mule-info改行の表現方法の変更
(setq eol-mnemonic-dos "↲")
(setq eol-mnemonic-unix "↓")
(setq eol-mnemonic-mac "←")
(setq eol-mnemonic-undecided "・")

;; set mode-line contents
(setq telephone-line-lhs
      '((level1 . (telephone-line-major-mode-segment))
        (level2 . (telephone-line-mule-info-segment))
        (nil    . (telephone-line-buffer-name-segment))))
(setq telephone-line-center-rhs
      '((nil . (telephone-line-nyan-segment))))
(setq telephone-line-rhs
      '((nil . nil)
	(level2 . (telephone-line-vc-segment
		  telephone-line-process-segment))
	(level1 . (telephone-line-airline-position-segment))))

;; set separator shape
(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)

;; all configuration must be done before calling (telephone-line-mode 1)
(telephone-line-mode 1)

