;; cask
(require 'cask "/usr/local/opt/cask/cask.el")
(cask-initialize)
(require 'use-package)
(package-initialize)
(pallet-mode t)

;; ------------------------------------------------
;; load-path auto-install.el
;; ------------------------------------------------
(defun add-to-load-path (&rest paths)
  (let (path)
  (dolist (path paths paths)
    (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
      (add-to-list 'load-path default-directory)
      (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path))))))

;; 各種読込 dir
(add-to-load-path "elisp" "elpa")
;; init-loader読込(各種lisp読込)
(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")
