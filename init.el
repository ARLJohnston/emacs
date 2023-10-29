(setq inhibit-startup-message t)

;; set tab indent to 2 spaces
(setq-default tab-width 2)
;; apply linting on save
(setq-default flycheck-emacs-lisp-load-path 'inherit)
;;set default font to MonoLisa Nerd Font with size 14
(set-face-attribute 'default nil :font "MonoLisa Nerd Font" :height 100)


(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)

(add-to-list 'custom-theme-load-path "~/emacs.d/themes/")
(load-theme 'zenburn t)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)


(unless (package-installed-p 'editorconfig)
  (package-install 'editorconfig))

;;Install Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
  (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
    (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)

;; install org mode with straight.el
(straight-use-package 'org)
(require 'org)

(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)
