	(straight-use-package 'org)
	(setq org-src-preserve-indentation t)

	(straight-use-package 'org-auto-tangle)
	(add-hook 'org-mode-hook 'org-auto-tangle-mode)
	(setq org-auto-tangle-default t)

  (straight-use-package 'org-superstar)
	(add-hook 'org-mode-hook 'org-superstar-mode)

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
  (scroll-bar-mode -1)
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)
  
  ;;(require 'package)
  ;;(add-to-list 'package-archives
  ;;  '("melpa" . "https://melpa.org/packages/"))
  ;;(package-initialize)
  ;;(package-refresh-contents)
  
  (load-theme 'zenburn t)
  
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
  ;;(straight-use-package 'lsp-mode)
  ;;(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
;;
  ;;(lsp-register-client
    ;;(make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    ;;:major-modes '(nix-mode)
                    ;;:server-id 'nix))
 ;; 
  ;;(straight-use-package 'nix-mode
    ;;:mode "\\.nix\\'")
  (straight-use-package 'lsp-mode)

  (straight-use-package 'lsp-haskell)
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)

  ;;(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  ;;(lsp-register-client
  ;; (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
  ;;                  :major-modes '(nix-mode)
  ;;                  :server-id 'nix))
  
  ;; haskell mode
  (straight-use-package 'haskell-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  
  ;;(add-to-list 'load-path "~/.emacs.d/lsp/")

(straight-use-package 'evil)
(evil-mode 1)

;;(add-to-list 'load-path "~/.emacs.d/evil/")

(straight-use-package 'evil-leader)

(evil-leader-mode 1)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "." 'fzf-find-file)
(evil-leader/set-key "bi" 'fzf-switch-buffer)
(evil-leader/set-key "bk" 'kill-this-buffer)
(evil-leader/set-key "bm" 'buffer-menu)

(straight-use-package 'fzf)

(straight-use-package 'company)
	;;:config
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(global-company-mode t)


