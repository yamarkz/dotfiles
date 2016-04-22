				      ;-*- Mode:Emacs-Lisp;Coding: utf-8 -*-
;; --------------------------------------------------
;; @load-path

;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp")

;; -------------------------------------------------
;; @ general

;; common lisp
(require 'cl)

;; 文字コード
(set-language-environment "Japanese")
(let ((ws window-system))
  (cond ((eq ws 'w32)
	 (prefer-coding-systems 'utf-8-unix)
	 (set-default-coding-systems 'utf-8-unix)
	 (setq file-name-coding-system 'sjis)
	 (setq locale-coding-system 'utf-8))
	((eq ws 'ns)
	 (require 'ucs-normalize)
	 (prefer-coding-system 'utf-8-hfs)
	 (setq file-name-coding-system 'utf-8-hfs)
	 (setq locale-coding-system 'utf-8-hfs))))

;; Macで英数と日本語にRictyを指定
(let ((ws window-system))
  (cond ((eq ws 'w32)
     (set-face-attribute default nil
                :family "Meiryo" ;; 変数
		:height 100)
     (set-fontset-font nil japanese-jisx0208 (font-spec :family "Meiriyo"))) ;; 日本語
    ((eq ws 'ns)
     (set-face-attribute default nil
		 :family "Ricty" ;; 英数
		 :height 140)
     (set-fontest-font nil japanese-jisx0208 (font-spec :family "Ricty"))))) ;; 日本語

;; スタートアップ非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
(tool-bar-mode -1)

;; メニューバーを非表示
;; (menu-bar-mode -1)

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; タイトルバーにファイルをフルパス表示
(setq frame-title-format
      (format "%%f -Emacs@%s" (system-name)))

;; 行番号表示
(global-linum-mode t)
(set-face-attribute 'linum nil
	    :foreground "#800"
	    :height 0.9)

;; 行番号フォーマット
;; (setq linum-format "%4d")

;; 括弧の範囲内を強調表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; 括弧の範囲色
(set-face-background 'show-paren-match-face "#500")

;; 選択領域の色
(set-face-background 'region "#555")

;; 行末の白色を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables '(tab-width 4))

;; yes or no をy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 最近使ったファイルをメニューに表示
(recentf-mode t)

;; 最近使ったファイルの表示数
(setq recentf-max-menu-items 10)

;; 最近開いたファイルの保存数を増やす
(setq recentf-max-saved-items 3000)

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; ミニバッファの履歴の保存数を増やす
(setq history-length 3000)

;; バックアップを残さない
(setq make-backup-files nil)

;; 行間
(setq-default line-spaceing 0)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.85))

;; モードラインに行番号表示
(line-number-mode t)

;; モードラインに列番号表示
(column-number-mode t)

;; C-Ret で矩形選択
;; 詳しいキーバインド操作: http://dev.ariel-networks.com/articles/emacs/part5/
(cua-mode t)
(setq cua-enable-cua-kets nil)

;; ------------------------------------------------------------
;; @modeline

;; モードラインの割合表示を総行数表示
(defvar my-lines-page-mode t)
(defvar my-mode-line-format)

;;; packages:
(when (or (require 'cask "~/.cask/cask.el" t)
          (require 'cask nil t))
  (cask-initialize))
(package-initialize)

;;; color-theme:
(require 'color-theme)
(color-theme-initialize)
(color-theme-clarity)
(color-theme-comidia)

;; メニューバー日本語化
;; http://www11.atwiki.jp/s-irie/pages/13.html
(if (and(= emacs-major-version 22)
        (eq window-system 'x))
    (setq menu-tree-coding-system 'utf-8))
(require 'menu-tree nil t)


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

;; ボタンをシンプルにする
(setq tabbar-home-button-enabled "")
(setq tabbar-scroll-right-button-enabled "")
(setq tabbar-scroll-left-button-enabled "")
(setq tabbar-scroll-right-button-disabled "")
(setq tabbar-scroll-left-button-disabled "")

;; Ctrl-Tab, Ctrl-Shift-Tab でタブを切り替える
(dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
  (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))
(defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
  '(defun ,name (arg)
     (interactive "P")
     ,do-always
     (if (equal nil arg)
         ,on-no-prefix
       ,on-prefix)))

