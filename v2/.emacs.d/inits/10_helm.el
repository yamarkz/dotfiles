;; helm 設定
(when (require 'helm-config nil t)
  (helm-mode 1)
  (define-key global-map (kbd "M-x")      'helm-M-x)
  (define-key global-map (kbd "C-x C-f")  'helm-find-files)
  (define-key global-map (kbd "C-x C-r")  'helm-recentf)
  (define-key global-map (kbd "M-y")      'helm-show-kill-ring)
  (define-key global-map (kbd "C-x b")    'helm-mini)
  (define-key global-map (kbd "C-x c i")  'helm-imenu)
  (define-key global-map (kbd "M-p")      'helm-eshell-history)
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
)


;; helm-descbinds設定
(when (require 'helm-descbinds nil t)
  (helm-descbinds-mode))
