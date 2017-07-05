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
(require 'org)
(define-key org-mode-map (kbd "C-c M-o") 'ace-link-org)

(require 'eww)
(define-key eww-link-kyemap (kbd "e") 'ace-link-eww)
(define-key eww-mode-map (kbd "e") 'ace-link-eww)

;;; text-property-any -> next-single-property-change
;;; にしないと一部のリンクが辿れないので再定義
(defun ali--eww-collect-references ()
  "Collect the positions of visible links in the current `eww' buffer."
  (save-excursion
    (save-restriction
      (narrow-to-region
       (window-start)
       (window-end))
      (goto-char (point-min))
      (let ((skip (next-single-property-change (point) 'help-echo))
            candidates)
        (while (setq skip (text-property-not-all
                           skip (point-max) 'help-echo nil))
          (goto-char skip)
          (push skip candidates)
          (setq skip (next-single-property-change (point) 'help-echo)))
        (nreverse candidates)))))
