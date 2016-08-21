;; markdown-mode設定------------------------------------->
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rst" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.egg" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ini" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.log" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt" . markdown-mode) auto-mode-alist))
(setq markdown-command "/opt/local/bin/multimarkdown")
