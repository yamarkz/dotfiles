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
(setq inhibit-startup-screeen t)

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

(when my-lines-page-mode
  (setq my-mode-line-format "%d")
  (if size-indication-mode
      (setq my-mode-line-format (concat my-mode-line-format "of %%|")))
  (cond ((and(eq line-number-mode t) (eq column-number-mode t))
         (setq my-mode-line-format (concat my-mode-line-format "(%%|,%%c)")))
        ((eq line-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format "L%%|")))
        ((eq column-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format "C%%x"))))

  (setq mode-line-position
        '(:eval (format my-mode-line-format
                        (count-lines (point-max)(point-min))))))

;; -----------------------------------------------------------
;; @initial frame maximize

;; 起動時にウィンドウ最大化
;; http://www.emacswiki.org/emacs/FullScreen#toc12
(defun jbr-init()
 "Called from term-setup-hook after the default
  terminal setup is
  done or directly from startup if term-setup-hook not
  used. The value
  0xF030 is the command for maximizing a window."
 (interactive)
 (w32-send-sys-command #xf030)
 (ecb-redraw-layout)
 (calendar))

(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-trame-position (selected-frame) 0 0)
         (setq term-setup-hook 'jbr-init)
         (setq window-setup-hook 'jbr-init))
        ((eq ws 'ns)
         ;; for MacBookAir
         (set-frame-position (selected-frame) 0 0)
         (set-frame-size (selected-frame) 95 47 ))))

;; -----------------------------------------------------------
;; @ key bind

;; バックスラッシュ
(define-key global-map (kbd "M-|") "\\")

;; globalなC-zを無効化
(global-unset-key "\C-z")


;; ----------------------------------------------------------
;; @ auto-install.el

;; パッケージのインストールを自動化
;; http://www.emacswiki.org/emacs/auto-install.el
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; ----------------------------------------------------------
;; @ color-theme.el

;; Emacsのカラーテーマ
;; http://code.google.com/p/gnuemacscolorthemetest/
(when (and (require 'color-theme nil t)(window-system))
  (color-theme-initialize)
  (color-theme-clarity)

;; ---------------------------------------------------------
;; @ redo+.el

;; redoできるようにする
;; http://www.emacswiki.org/emacs/redo+.el
(when (require 'redo+ nil t)
  (define-key global-map (kbd "C-_") 'redo))

;; ---------------------------------------------------------
;; @menu-tree.el

;; メニューバー日本語化
;; http://www11.atwiki.jp/s-irie/pages/13.html
(if (and(= emacs-major-version 22)
        (eq window-system x))
    (setq menu-tree-coding-system utf-8))
(require 'menu-tree nil t)


;; --------------------------------------------------------
;; @tabbar.el

;; タブ化
;; http://wwww.emacswiki.org/emacs/tabbar.el
;; (require 'cl)
(require 'tabbar nil t)

;; scratch buffer以外をまとめてタブに表示する
(setq tabbar-buffer-groups-function
      (lambda (b) (list "All Buffers")))
(setq tabbar-buffer-list-function
      (lambda()
        (remove-if
         (lambda(buffer)
           (unless (string-match (buffer-name buffer)
                         "\\(*scraych*\\|*Apropos*\\|*shell*\\|*eshell*\\|*Customize*\\)"
            (find (aref (buffer-name buffer) 0) "*"))
           )
           (buffer-list))))
;; tabbarを有効にする
(tabbar-mode 1)

;; ボタンをシンプルにする
(setq tabbar-home-button-enabled "")
(setq tabbar-scroll-right-button-enabled "")
(setq tabbar-scroll-left-button-enabled "")
(setq tabbar-scroll-right-button-disabled "")
(setq tabbar-scroll-left-button-disabled "")
