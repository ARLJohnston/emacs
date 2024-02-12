(use-package org
  :init
  (setq org-src-preserve-indentation t)
)

(use-package org-auto-tangle
  :init
  (add-hook 'org-mode-hook 'org-auto-tangle-mode)
  (setq org-auto-tangle-default t)
)

(use-package org-sticky-header-mode
  :straight (:type git :host github :repo "alphapapa/org-sticky-header")
  :defer t
  :init
  (setq org-sticky-header-full-path 'full)
  (setq org-sticky-header-outline-path-seperator " / ")
  (add-hook 'org-mode-hook 'org-sticky-header-mode)
)

(use-package org-superstar
  :init
  (add-hook 'org-mode-hook 'org-superstar-mode)
  (setq org-hide-leading-stars t)
  (setq org-superstar-leading-bullet ?\s)
  (setq org-indent-mode-turns-on-hiding-stars nil)
)

(use-package htmlize)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
  (haskell . t)))

(use-package org-roam
  :init
  (setq org-roam-directory "~/org")
  
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
  
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "DONE")))
  (setq org-clock-in-switch-to-state "IN-PROGRESS")
)

(defun org-agenda-sort-at-point ()
    (interactive)
  (org-sort-entries nil ?o)
  (org-sort-entries nil ?o))

(use-package nord-theme
  :init
  (load-theme 'nord t)
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (select-frame frame)
      (load-theme 'nord t)))
)


;; Less Jumpy scrolling
(setq scroll-step 1)
(setq scroll-margin 4)

(set-face-attribute 'default nil :font "MonoLisa Nerd Font")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq default-tab-width 2)
(electric-indent-mode -1)
(setq tab-always-indent 'complete)

(use-package rainbow-delimiters
  :init
  (rainbow-delimiters-mode 1)
)

(global-whitespace-mode)

(winner-mode 1)
(keymap-global-set "C-c h" 'winner-undo)
(keymap-global-set "C-c l" 'winner-redo)

(keymap-global-set "C-c c" 'comment-or-uncomment-region)
(keymap-global-set "C-c /" 'comment-or-uncomment-region)

;;(keymap-global-set "g r" 'revert-buffer)

(global-visual-line-mode)

(require 'table)
(setq warning-minimum-level :error)

(setq ring-bell-function 'ignore)
(setq-default flycheck-emacs-lisp-load-path 'inherit)

(unless (package-installed-p 'editorconfig)
  (package-install 'editorconfig))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :init
;;  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
)

;;(add-hook 'prog-mode-hook 'copilot-mode)

;; Read at high WPM
(use-package spray)

(use-package flycheck)
(use-package flycheck-grammarly
  :init
  (flycheck-grammarly-setup)
  (setq flycheck-grammarly-check-time 0.8)
)
(use-package flycheck-haskell)

(use-package imenu-list
  :init
  (setq imenu-list-focus-after-activation t)
)

;;lsp mode
(use-package lsp-mode)
(use-package lsp-ui
  :init
  (setq lsp-ui-doc-position 'bottom)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
)

(use-package lsp-haskell
  :init
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
)

;; haskell mode
(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
)

(setq backup-directory-alist '((".*" . "~/.backups/")))

(use-package yaml-mode)

(use-package evil
  :init
  ;;Evil-collection requirement
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  :hook
  (after-init . evil-mode))


(use-package evil-collection
  :init
  (evil-collection-init)
)

;;(straight-use-package evil-leader)
(use-package evil-leader
  :init
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "." 'fzf-find-file
    "bi" 'fzf-switch-buffer
    "bk" 'kill-this-buffer
    "bm" 'buffer-menu
    "ci" 'org-clock-in
    "co" 'org-clock-out
    "cs" 'org-agenda-sort-at-point
    "si" 'imenu-list-smart-toggle
    "m" 'magit
    ;;EMMS
    "es" 'toggle-emms
    "el" 'emms-next
    "eh" 'emms-previous
    "ej" 'emms-volume-lower
    "ek" 'emms-volume-raise
    "ni" 'org-roam-node-insert)
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-file)
    (evil-define-key 'normal 'global "gr" 'revert-buffer)
)

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  (evil-set-undo-system 'undo-tree)
  (setq undo-tree-history-directory-alist '(("." . "~/.backups/")))
  (setq undo-tree-visualizer-timestamps t)
)



;;(evil-leader/set-key "w" '(lambda () (interactive) execute-kbd-macro (read-kbd-macro "C-w")))

(use-package dired-preview
  :init
  (dired-preview-global-mode 1)
)

(use-package fzf)

(use-package magit)

(use-package blamer
	:straight (:host github :repo "artawower/blamer.el")
  :init
  (blamer-mode 1)
	:bind (("s-i" . blamer-show-commit-info))
	:custom
	(blamer-idle-time 0.3)
	(blamer-min-offset 70)
	:custom-face
	(blamer-face ((t :foreground "#81a1c1"
										:background nil
										:height 100
										:italic t)))
;;	:config
;;	(global-blamer-mode 1))

(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (global-company-mode t)
  (setq company-dabbrev-downcase nil)
)

(use-package pdf-tools
  :init
  (pdf-loader-install)
  (add-hook 'pdf-view-mode-hook '(lambda () (display-line-numbers-mode -1)))
  (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
)


(use-package image-roll
	:straight (:type git :host github :repo "dalanicolai/image-roll.el")
	:defer t
  :init
  (add-hook 'pdf-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)))
)

(use-package emms
  :ensure t
  :config
  (setq emms-source-file-default-directory "~/Music/")
)
(require 'emms-setup)
(emms-all)

(setq emms-player-list '(emms-player-vlc)
			emms-info-functions '(emms-info-native))

;;emms-start/stop
(defun toggle-emms ()
	(interactive)
	(if (emms-player-playing-p)
			(emms-stop)
		(emms-start)))

;;(straight-use-package 'rustic)
;;
;;(setq rustic-format-on-save t)
;;  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
;;
;;(straight-use-package 'lsp-mode)
;;(lsp-rust-analyzer-cargo-watch-command "clippy")
;;(lsp-eldoc-render-all t)
;;(lsp-idle-delay 0.6)
;;(lsp-inlay-hint-enable t)
;;(lsp-rust-analyzer-display-chaining-hints t)
;;(lsp-rust-analyzer-display-closure-return-type-hints t)
(use-package rust-mode)

(use-package erlang)
(use-package erlang-mode
  :init
  (setq erlang-indent-level 2)
)
(setq erlang-indent-level 2)
