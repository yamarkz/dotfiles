;; ruby設定---------------------------------------------->
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '(".pryrc&" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq ruby-indent-level tab-width)
             (setq ruby-deep-indent-paren-style nil)
             (flymake-mode t)
             (define-key ruby-mode-map [return] 'ruby-reindent-then-newline-and-indent)))


;;; ruby Ver2以降はマジックコメント不要なので自動挿入OFF
(setq ruby-insert-encoding-magic-comment nil)

;;; 括弧の自動挿入
(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))
(defun ruby-mode-hook--electric-pair ()
  (dolist (k '("\"" "\'" "(" "[" "{"))
    (define-key ruby-mode-map k 'electric-pair)))
(add-hook 'ruby-mode-hook 'ruby-mode-hook--electric-pair)

;;; tab/indent設定
(defun ruby-mode-hook--indent ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (set (make-local-variable 'tab-stop-list)
       '(4 8 12 16 20 24 28 32 36 40)))
(add-hook 'ruby-mode-hook 'ruby-mode-hook--indent)

;;; fly-check
(add-hook 'ruby-mode-hook 'flycheck-mode)

;;; flymake設定
(require 'flymake)
;; I don't like the default colors :)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
;(add-hook  'ruby-mode-hook
;     '(lambda ()
;        ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
;        (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
;            (flymake-mode))
;         ))


;;; rubocop
(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

;;; ruby-electric(かっこやdo endなどの対応自動補正
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

;;; ruby-block(end対応箇所ハイライト)
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;;; ruby-hash-syntax
;;; M-x package-install ruby-hash-syntax
;;; autoloadなので設定不要
;;; M-x ruby-toggle-hash-syntax

;;; inf-ruby
;;; M-x package-install inf-ruby
;;; m-x inf-ruby
;;; region選択して、C-c C-rでrubyコードをpryで実行
;;; method定義のendの位置でC-c C-xするとpryでメソッド定義を実行できる。
(require 'inf-ruby)
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-eval-binding "Pry.toplevel_binding")
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)

;;; rspec-mode
;;; F10で行指定spec実行
;(require 'rspec-mode)
;(global-set-key [f10] 'rspec-verify-single)

;; ;; definition for flycheck
;; (flycheck-define-checker ruby-rubylint
;;   "A Ruby syntax and style checker using the rubylint tool."
;;   :command ("ruby-lint" source)
;;   :error-patterns
;;   ((warning line-start
;;             (file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
;;             line-end)
;;    (error line-start
;;           (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
;;           line-end))
;;     :modes (enh-ruby-mode ruby-mode))
