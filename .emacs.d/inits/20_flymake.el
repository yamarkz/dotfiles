;; flymake ---------------------------------------->
(require 'flymake)
(set-face-background 'flymake-errline "red4")
(set-face-foreground 'flymake-errline "black")
(set-face-background 'flymake-warnline "dark state blue")
(set-face-foreground 'flymake-warnline "black")
(defun flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-on             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info fly-make-err-info line-no)))
         (count               (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

;; log level
;; -1 = NONE,
;;  0 = ERROR,
;;  1 = WARNING,
;;  2 = INFO,
;;  3 = DEBUG
(setq flymake-log-level 3)

;;   flycheck-------------------------------------->
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
    '(custom-set-variables
         '(flycheck-display-errors-function #'flycheck-pos-tip-error-message)))

;; flycheck-list-errors --------------------------->
;;; C-c ! l(小文字のL)
