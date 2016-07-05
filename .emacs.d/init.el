				      ;-*- Mode:Emacs-Lisp;Coding: utf-8 -*-


;; -------------------------------------------------
;; 文字コード
;; -------------------------------------------------
;; ------------------------------------
;; 表:Emacsでのエンコーディング名       |
;; | 名前      | Emacsでの名前          |
;; |-----------+------------------------|
;; | UTF-8     | utf-8                  |
;; | shift_JIS | sjis, shift_jis, cp932 |
;; | EUC-JP    | euc-jp, euc-japan　　　|
;; | JIS       | junet, iso-2022-jp     |
;; -------------------------------------
;; 表:改行コードと接尾辞
;; | OS       | 改行 | 接尾辞 |
;; | Windows  | \r\n | --dos  |
;; | Mac      | \n   | --max  |
;; | Linux等  | \n   | --unix |

(set-locale-environment nil)
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq make-backup-files nil)
(setq delete-auto-save-files t)
(setq use-dialog-box nil)


;; -------------------------------------
;; emacs  package管理 Cask
;; -------------------------------------
(require 'cask "/usr/local/opt/cask/cask.el")
(cask-initialize)
(package-initialize)
(require 'use-package)
(pallet-mode t)

;; -------------------------------------
;; emacs color-theme
;; -------------------------------------
(require 'color-theme)
(color-theme-initialize)
(color-theme-clarity)
(color-theme-comidia)

;; display underline on the edit-number-line
(defface my-hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class color) (background dark))
    (:background "NavyBlue" t))
   ;; 背景がLightならば背景色を緑に
   (((class color) (background light))
    (:background "Yellow" t))
   (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;;; Environment

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.85))

;; between the lines
(setq-default line-spacing 5)

;; display emphasis end of line
(setq-default show-trailing-whitespace t)

;; create newline- next-final-line
(setq require-final-newline t)

;; no create newline end-of-buffer
(setq next-line-add-newlines nil)

;; display time on the mode-line
(display-time-mode t)

;; display recent-open-file-list
(recentf-mode t)

;; copy/pasteをclipboardで行う
(cond (window-system
       (setq x-select-enable-clipboard t)))

;; file名の補完で大小区別なし
(setq completion-ignore-case t)

;; ツールバー非表示
(if window-system
    (tool-bar-mode -1)
  (menu-bar-mode -1))

;; yes or no をy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; -------------------------------------------------
;; 基本操作設定
;; -------------------------------------------------

;; デバッグモードでの起動
(require 'cl)

;; Variables
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq delete-auto-save-files t)
(setq inhibit-startup-screen t)
(menu-bar-mode -1)


;; title-barにfile-full-path表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 行番号表示
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "yellow"
                    :height 0.2)
(set linum-format "%4d")

;; Key config ;(use-package bind-key)
(progn
(bind-key* "C-c <left>" 'windmove-left)
(bind-key* "C-c <down>" 'windmove-down)
(bind-key* "C-c <up>" 'windmove-up)
(bind-key* "C-c <right>" 'windmove-right))


;;; wdired
(use-package wdired)

;; Undo Tree
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  (bind-key "C-_" #'undo-tree-undo)
  (bind-key "C-?" #'undo-tree-redo))

;; Auto-Complete
(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (add-to-list 'ac-dictionary-directories (locate-user-emacs-file "./ac-dict"))
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t))

;; Helm config
(use-package helm :defer t
  :diminish helm-mode
  :init
  (require 'helm-config)
  (bind-key "C-x C-f" 'helm-find-files)
  (bind-key "M-x" 'helm-M-x)
  (helm-mode t))

;; helm-ag
(custom-set-variables
 '(helm-ff-file-compressed-list (quote ("epub" "gz" "bz2" "zip" "7z")))
 '(magit-log-arguments (quote ("--graph" "--decorate" "-n256"))))
(bind-key "C-x s" 'helm-ag)

;; helm-swoop
(use-package helm-swoop
  :init
  (bind-key "C-x w" 'helm-swoop)
  (bind-key "M-C-;" 'helm-multi-swoop))

;; Magit
(setq-default magit-auto-revert-mode nil)
(setq vc-handled-backends '())
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
(bind-key "C-x m" 'magit-status)
(bind-key "C-c l" 'magit-blame)

(custom-set-faces
 '(magit-diff-added ((t (:background "black" :foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "white" :foreground "green"))))
 '(magit-diff-removed ((t (:background "black" :foreground "blue"))))
 '(magit-diff-removed-hightlight ((t (:background "white" :foreground "blue"))))
 '(magit-hash ((t (:foreground "red"))))
)

;; Flycheck
(use-package flycheck
  :diminish flycheck-mode
  :init
  (global-flycheck-mode t)
  :config
  (flycheck-package-setup))

;; Ruby
(use-package enh-ruby-mode :defer t
  :mode (("\\.rb\\'". enh-ruby-mode)
	 ("\\.rake\\'". enh-ruby-mode))
  :interpreter "pry"
  :config
  (use-package robe)
  (defun my/enh-ruby-mode-hook ()
    (set (make-local-variable 'ac-ignore-case) t))
  (subword-mode t)
  (yard-mode t)
  (custom-set-variables
   '(ruby-deep-indent-paren-style nil))
  (setq-default enh-ruby-node-insert-magic-comment t)
  (add-hook 'robe-mode-hook 'ac-robe-setup))
(magic-filetype-set-auto-mode 'ruby)

;;; begin enh-ruby-mode patch
(defadvice enh-ruby-mode-set-encoding (around stop-enh-ruby-mode-set-encoding)
  "If enh-ruby-not-insert-magic-comment is true, stops enh-ruby-mode-set-encoding."
  (if (and (boundp 'enh-ruby-not-insert-magic-comment)
	   (not enh-ruby-not-insert-magic-comment))
      ad-do-it))
(ad-activate 'enh-ruby-mode-set-encoding)
(setq-default enh-ruby-not-insert-magic-comment t)
;;; enh-ruby-mode patch ends here


;; inf-ruby
(use-package inf-ruby :defer t
  :config
  (custom-set-variables
   '(inf-ruby-default-implementation "pry")
   '(inf-ruby-eval-binding "Pry.topeval_binding"))
  (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on))

(custom-set-variables '(helm-ff-file-compressed-list '("epub" "gz" "bz2" "zip" "7z")))
(bind-key "C-:" 'helm-ag)

;; parentheses highlight underline
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "underline")

;; C-Ret で矩形選択
;; 詳しいキーバインド操作: http://dev.ariel-networks.com/articles/emacs/part5/
(cua-mode t)
(setq cua-enable-cua-kets nil)

;; neotree
(global-set-key [f8] 'neotree-toggle)
(setq neo-show-hidden-files t)
(setq neo-create-file-auto-open t)
(setq neo-persist-show t)
(setq neo-keymap-style 'concise)
(setq neo-smart-open t)

;; ------------------------------------------------------------
;; @modeline

;; モードラインの割合表示を総行数表示
(defvar my-lines-page-mode t)
(defvar my-mode-line-format)

;; scratch buffer以外をまとめてタブに表示する
(setq tabbar-buffer-groups-function
      (lambda (b) (list "All Buffers")))
(setq tabbar-buffer-list-function
      (lambda()
        (remove-if
         (lambda(buffer)
           (unless (string-match (buffer-name buffer)
                                 "\\(*scratch*\\|*Apropos*\\|*shell*\\|*eshell*\\|*Customize*\\)")
             (find (aref (buffer-name buffer) 0) "*"))
           )
                    (buffer-list))))

(put 'set-goal-column 'disabled nil)
