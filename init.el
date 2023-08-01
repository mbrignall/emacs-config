(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Bootstrap straight.el
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

;; Install use-package and make it use straight.el by default
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Package setup -------------------------------------------

;; Theme
(load-theme 'leuven-dark t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Org mode
(use-package org
  :config
  (setq org-startup-indented t
        org-hide-leading-stars t)
  (add-hook 'org-mode-hook 'org-indent-mode))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")))

;; Kill the warning about cl
(setq byte-compile-warnings '(cl-functions))

;; Copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el" "*.org"))
  :ensure t)

(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; Fancy dashboard with Rubin quotes
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner image
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner 'logo)
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever image/text you would prefer

  (setq dashboard-items '((recents  . 5)
                          (projects . 5))))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
(setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package

(dashboard-modify-heading-icons '((recents . "file-text")
                                  (bookmarks . "book")))

(setq dashboard-footer-messages '("Zoom in and obsess. Zoom out and observe. We get to choose."))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(column-number-mode)

(setq mouse-autoselect-window t)

(windmove-default-keybindings)

;; Keyboard-centric user interface
(setq inhibit-startup-message t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "FiraMono Nerd Font" :height 100)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Temp helpers
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode:
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
	 (nix-mode . eglot-ensure)
	 (css-mode . eglot-ensure)
	 (html-mode . eglot-ensure)
	 (yaml-mode . eglot-ensure)))

;; Vterm config
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(global-set-key [f2] 'vterm-toggle)
(global-set-key [C-f2] 'vterm-toggle-cd)

;; Consider switching from neotree to treemacs
(use-package treemacs
  :bind ("<f8>" . treemacs))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :config
  (setq nix-indent-function #'nix-indent-line))


;; Projectile Configuration --------------------------------------

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Add prescient and ivy-prescient for better sorting and filtering
(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after (ivy prescient)
  :config
  (ivy-prescient-mode 1))

;; Magit Configuration -------------------------------------------

(use-package magit
  :init
  (message "Loading Magit!")
  :config
  (message "Loaded Magit!")
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))





