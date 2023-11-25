	(straight-use-package 'org)
	(setq org-src-preserve-indentation t)

	(straight-use-package 'org-auto-tangle)
	(add-hook 'org-mode-hook 'org-auto-tangle-mode)
	(setq org-auto-tangle-default t)

  (straight-use-package 'org-superstar)
	(add-hook 'org-mode-hook 'org-superstar-mode)

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
                         "=================\n** Meeting %U\n:LOGBOOK:\n:END:\n Attendees:\n")
      :unnarrowed t
      :jump-to-captured t)))

(setq ring-bell-function 'ignore)
(setq-default tab-width 2)
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(set-face-attribute 'default nil :font "MonoLisa Nerd Font")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(load-theme 'zenburn t)

(unless (package-installed-p 'editorconfig)
  (package-install 'editorconfig))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)

;; Install magit 
(straight-use-package 'magit)

;;lsp mode
(straight-use-package 'lsp-mode)

(straight-use-package 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;; haskell mode
(straight-use-package 'haskell-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;;Evil-collection requirement
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(straight-use-package 'evil)
(evil-mode 1)

(straight-use-package 'evil-collection)
(evil-collection-init)

;;(add-to-list 'load-path "~/.emacs.d/evil/")

(straight-use-package 'evil-leader)

(global-evil-leader-mode 1)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "." 'fzf-find-file)
(evil-leader/set-key "bi" 'fzf-switch-buffer)
(evil-leader/set-key "bk" 'kill-this-buffer)
(evil-leader/set-key "bm" 'buffer-menu)
(evil-leader/set-key "w" '(execute-kbd-macro (kbd "C-w")))

(straight-use-package 'fzf)

(straight-use-package 'company)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(global-company-mode t)

;;(straight-use-package '(emacs-application-framework :host github :repo "manateelazycat/emacs-application-framework" :files ("*")))

;;(straight-use-package 'pdf-tools)
;;  (add-hook 'pdf-view-mode-hook '(lambda () (display-line-numbers-mode -1))
;;
;;(pdf-loader-install)
