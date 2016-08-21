;; ----------------------------------------
;; 基本操作設定
;; ----------------------------------------
;;; Common Lisp必要
(require 'cl)

;; ~ backup-fileを作らない
(setq make-backup-files nil)

;; # backup-fileを作らない
(setq auto-save-default nil)

;; backup-fileを作らない
(setq backup-inhibited t)

;; delete auto-save-file at edit ending
(setq delete-auto-save-files t)

;; file-pointerを記憶しておく
(require 'saveplace)
(setq-default save-place t)

;; set-up-screen 非表示
(setq inhibit-startup-screen t)

;; scratch first-message eraing
(setq inhibit-scratch-message "")

;; menu-bar非表示
(menu-bar-mode -1)

;; title-barにfile-full-path表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 行番号表示
(global-linum-mode t)

;; 行番号 色/高さ
(set-face-attribute 'linum nil
            :foreground "yellow"
            :height 0.2)

;; 行番号 format
(setq linum-format "%4d  ")

;; c-h => backspace
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" 'backward-delete-char)

;; M-h => 単語単位でbackspace
(global-set-key (kbd "M") 'backward-kill-word)

;; 画面中央にする
(global-unset-key (kbd "C-l"));;prefix-keyとする
(global-set-key (kbd "C-l C-l") 'recenter-top-bottom)

;; 置換key-bind
(global-set-key (kbd "C-l r") 'query-replace)
(global-set-key (kbd "C-l R") 'query-replace-regexp)

;; 別window逆 scroll
;; 別window-scroll >> C-M-v
(global-set-key (kbd "C-M-y") 'scroll-other-window-down)

;; 前後のerror箇所へjump
(global-set-key (kbd "M-}") 'next-error)
(global-set-key (kbd "M-{") 'previous-error)

;; 直前のwindow構成に戻る
(global-set-key (kbd "C-l C-u") 'winner-undo)
(winner-mode 1)

;; C-c C-SPC => rectangle-select(矩形探索)
(cua-mode t)
(setq cua-enable-cua-keys nil)
(define-key global-map (kbd "C-c C-SPC") 'cua-set-rectangle-mark)

;; cursor上のpathを開く => C-x C-f
(ffap-bindings)

;; 現在のfile-pathを開く(dired start) => C-x C-j
(load "dired-x")

;; mode-lineに行番号を表示
(line-number-mode t)

;; display row-number on the mode-line
(column-number-mode t)

;; display row-number on the mode-line
(column-number-mode t)

;; display file-rate on the mode-line
(size-indication-mode t)

;; display time-date(dayOfTheWeek/month/day)
(setq display-time-day-and-date t)

;; between the lines
(setq-default line-spacing 0.2)

;; tab-width
(setq-default tab-width 2)

;; tab convert space
(setq-default indent-tabs-mode nil)

;; display underline on the edit-number-line
(defface my-hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class color) (background dark))
      (:background "NavyBlue" t))
      (((class color) (background light))
      (:background "Yellow" t))
      (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "underline")

;; select-region-color
(set-face-background 'region "dark slate blue")

;; display emphasis end of line
(setq-default show-trailingwhitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; color-setting
(load-theme 'tsdh-dark t)

;; imput yes or no => y or n
(fset 'yes-or-no-p 'y-or-n-p )

;; ignore large lower character when completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; incremental MiniBuffer Completion
;;(icomplete-mode 1)

;; create newline next-final-line
(setq require-final-newline t)

;; display time no the mode-line
(display-time-mode t)

;; display recent-open-file-list
(recentf-mode t)

;; no bell sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; display recent-open-file-max-count
(setq recentf-max-menu-items 20)

;; display recent-open-saved-file-count
(setq recentf-max-saved-items 10000)

;; save minibuffer-history
(savehist-mode t)

;; minibuffer-history-length
(setq history-length 10000)

;; newline-and-indent(global)
(global-set-key "\C-m" 'newline-and-indent)

;; copy/pasteをclipboardで行う
(cond (window-system
       (setq x-select-enable-clipboard t)))

;; file名の補完で大小区別無し
(setq completion-ignore-case t)

;; buffer自動再読込
(global-auto-revert-mode 1)

;; 釣り合いのとれる括弧をハイライトする
(show-paren-mode 1)

;; 改行と同時にインデントも行う
(global-set-key "\C-m" 'newline-and-indent)

;; find-functionをキー割り当てする
;; - C-x Fをfind-functionに割り当て。
;; 関数定義を開く。
;; - C-x Vをfind-variableに割り当て。
;; 変数定義を開く。
;; - C-x Kをfind-function-on-keyに割り当て。
;; キーに割り当てられているコマンドの定義を開く。
(find-function-setup-keys)

;;; 日付自動挿入
(setq org-capture-templates
      '(("d" "Diary" entry (file+headline "~/memo/diary.org" "Diary")
         "** %U%?\n\n")))

;; 同名ファイルのバッファ名の識別文字列を変更する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Mac でファイルを開いたときに、新たなフレームを作らない
(setq ns-pop-up-frames nil)

;;; 最近閉じたバッファを復元
(defvar my-killed-file-name-list nil)
(defun my-push-killed-file-name-list ()
  (when (buffer-file-name)
    (push (expand-file-name (buffer-file-name)) my-killed-file-name-list)))
(defun my-pop-killed-file-name-list ()
  (interactive)
  (unless (null my-killed-file-name-list)
    (find-file (pop my-killed-file-name-list))))
