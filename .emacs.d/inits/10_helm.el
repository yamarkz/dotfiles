;; helm 設定
(require 'helm-config)
(require 'helm-mode)
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

(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))


;; helm-descbinds設定
;(when (require 'helm-descbinds nil t)
;  (helm-descbinds-mode))

;; helm-ag 設定
(require 'helm-ag)
(setq helm-ag-base-command "ag --nocolor --nogrou")
(global-set-key (kbd "C-c s") 'helm-ag)

;; helm-migemo
(helm-migemo-mode 1)
