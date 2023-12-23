(straight-use-package 'org)
(setq org-src-preserve-indentation t)

(straight-use-package 'org-auto-tangle)
(add-hook 'org-mode-hook 'org-auto-tangle-mode)
(setq org-auto-tangle-default t)

(use-package org-sticky-header-mode
  :straight (:type git :host github :repo "alphapapa/org-sticky-header")
  :defer t)
(setq org-sticky-header-full-path 'full)
(setq org-sticky-header-outline-path-seperator " / ")
(add-hook 'org-mode-hook 'org-sticky-header-mode)

(straight-use-package 'org-superstar)
(add-hook 'org-mode-hook 'org-superstar-mode)
(setq org-hide-leading-stars t)
(setq org-superstar-leading-bullet ?\s)
(setq org-indent-mode-turns-on-hiding-stars nil)

(straight-use-package 'htmlize)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
  (haskell . t)))

(straight-use-package 'org-roam)
(setq org-roam-directory "~/org")

(with-eval-after-load 'evil-leader
  (evil-leader/set-key "ni" 'org-roam-node-insert))

(setq org-roam-capture-templates
  '(("d" "default" plain
    "%?"
    :if-new (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
                       "#+title: ${title}\n")
    :unnarrowed t
    :jump-to-captured t)
  ("m" "meeting" plain
    "%?"
    :if-new (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
                       "=================\n** Meeting %U\nAttendees:\n")
    :unnarrowed t
    :jump-to-captured t)))

(straight-use-package 'nord-theme)
(load-theme 'nord t)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (load-theme 'nord t)))

;; Less Jumpy scrolling
(setq scroll-step 1)
(setq scroll-margin 4)

(setq ring-bell-function 'ignore)
(setq-default tab-width 2)
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(set-face-attribute 'default nil :font "MonoLisa Nerd Font")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(unless (package-installed-p 'editorconfig)
  (package-install 'editorconfig))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)

;; Install magit 
(straight-use-package 'magit)

;;lsp mode
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(setq lsp-ui-doc-position 'bottom)

(straight-use-package 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;; haskell mode
(straight-use-package 'haskell-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq backup-directory-alist '((".*" . "~/.backups/")))

;;Evil-collection requirement
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(straight-use-package 'evil)
(evil-mode 1)

(straight-use-package 'evil-collection)
(evil-collection-init)

(straight-use-package 'evil-leader)
(global-evil-leader-mode 1)

(evil-leader/set-leader "<SPC>")

(evil-leader/set-key "." 'fzf-find-file)

(evil-leader/set-key "bi" 'fzf-switch-buffer)
(evil-leader/set-key "bk" 'kill-this-buffer)
(evil-leader/set-key "bm" 'buffer-menu)

(evil-leader/set-key "w" '(lambda () (interactive) execute-kbd-macro (read-kbd-macro "C-w")))

(evil-leader/set-key "ci" 'org-clock-in)
(evil-leader/set-key "co" 'org-clock-out)

(evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
(evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-file) ; use dired-find-file instead if not using dired-open package

(straight-use-package 'dired-preview)
(dired-preview-global-mode 1)

(straight-use-package 'fzf)

(straight-use-package 'company)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(global-company-mode t)

(straight-use-package 'pdf-tools)
(add-hook 'pdf-view-mode-hook '(lambda () (display-line-numbers-mode -1)))
(add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)

(pdf-loader-install)

(use-package image-roll
  :straight (:type git :host github :repo "dalanicolai/image-roll.el")
  :defer t)

(add-hook 'pdf-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)))

(straight-use-package 'emms)

(emms-all)
(setq emms-player-list '(emms-player-vlc)
      emms-info-functions '(emms-info-native))

;;emms-start/stop
(defun toggle-emms ()
  (interactive)
  (if (emms-player-playing-p)
      (emms-stop)
    (emms-start)))


(evil-leader/set-key "es" 'toggle-emms)
(evil-leader/set-key "el" 'emms-next)
(evil-leader/set-key "eh" 'emms-previous)
(evil-leader/set-key "ej" 'emms-volume-lower)
(evil-leader/set-key "ek" 'emms-volume-raise)

(straight-use-package 'rustic)

(setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

(straight-use-package 'lsp-mode)
;;(lsp-rust-analyzer-cargo-watch-command "clippy")
;;(lsp-eldoc-render-all t)
;;(lsp-idle-delay 0.6)
;;(lsp-inlay-hint-enable t)
;;(lsp-rust-analyzer-display-chaining-hints t)
;;(lsp-rust-analyzer-display-closure-return-type-hints t)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(straight-use-package 'lsp-ui)
(lsp-ui-peek-always-show t)
(lsp-ui-sideline-show-hover t)
(lsp-ui-doc-enable nil)
