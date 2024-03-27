(setq ring-bell-function 'ignore)
(setq-default flycheck-emacs-lisp-load-path 'inherit)

(unless (package-installed-p 'editorconfig)
	(package-install 'editorconfig))

(use-package copilot
	:straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
	:ensure t
	:init
	(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
)
;;(Add-hook 'prog-mode-hook 'copilot-mode)

;; Read at high WPM
(use-package spray)

(use-package flycheck)
(use-package flycheck-haskell)

(use-package imenu-list
	:init
	(setq imenu-list-focus-after-activation t)
)

(use-package vertico
	:custom
	(vertico-count 13)
	(vertico-resize t)
	(vertico-cycle nil)
	:config
	(vertico-mode)
	:init
	(setq read-file-name-completion-ignore-case t
		read-buffer-completion-ignore-case t
		completion-ignore-case t)
)


;;lsp mode
(use-package lsp-mode
	:init
	(keymap-global-set "M-RET" 'lsp-execute-code-action)
)

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

(use-package solarized-theme
	:init
	(load-theme 'solarized-gruvbox-dark t)
	(add-hook 'after-make-frame-functions
		(lambda (frame)
			(select-frame frame)
			(load-theme 'solarized-gruvbox-dark t)))
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
(setq-default tab-width 2)
(electric-indent-mode -1)

(global-whitespace-mode)
;; Default marks, but make tabs appear as two spaces
(setq whitespace-display-mappings
	'((space-mark 32
							 [183]
							 [46])
	 (space-mark 160
							 [164]
							 [95])
	 (newline-mark 10
								 [36 10])
	 (tab-mark 9
						 [183 183]
						 [95 95]))
)
(setq whitespace-style (delq 'lines whitespace-style))
(setq whitespace-style (delq 'tabs whitespace-style))


(add-hook 'before-save-hook #'whitespace-cleanup)

(winner-mode 1)
(keymap-global-set "C-c h" 'winner-undo)
(keymap-global-set "C-c l" 'winner-redo)

(keymap-global-set "C-c c" 'comment-or-uncomment-region)
(keymap-global-set "C-c /" 'comment-or-uncomment-region)

;;(keymap-global-set "g r" 'revert-buffer)

(global-visual-line-mode)

;; (require 'table)
(setq warning-minimum-level :error)

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
		"cu" 'org-clock-update-time-maybe
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
	:bind
		(("s-i" . blamer-show-commit-info))
	:custom
		(blamer-idle-time 0.3)
		(blamer-min-offset 70)
	:custom-face
		(blamer-face ((t :foreground "#81a1c1"
	:defer t
	:background
		nil
	:height
		100
	:italic
		t)))
)

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
	:defer t
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
	:init
	(emms-all)
	(setq emms-player-list '(emms-player-vlc)
		emms-info-functions '(emms-info-native))
)

(use-package rustic
	:init
	(add-hook 'rust-mode-hook #'lsp)
	:defer t
)
(use-package rustfmt
	:defer t
)

(use-package docker)
(use-package dockerfile-mode)

(use-package go-mode
	:init
	(setq compile-command "go test -v")
	(add-hook 'before-save-hook 'gofmt-before-save)
)

(use-package yasnippet
	:init
	(setq yas-snippet-dirs
		'("~/Documents/yasnippet-golang")
	)
	(yas-reload-all)
	(yas-minor-mode-on)
	(yas-global-mode 1)

	(keymap-global-set "M-s" 'yas-insert-snippet)
)
