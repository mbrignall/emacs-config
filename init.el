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

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(unless (package-installed-p 'quelpa-use-package)
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git")))
(require 'quelpa-use-package)

;; Install use-package and make it use straight.el by default
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;; Keyboard-centric user interface
(setq inhibit-startup-message t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "FiraMono Nerd Font" :height 100)

;; Kill the warning about cl
(setq byte-compile-warnings '(cl-functions))
(setq mouse-autoselect-window t)
(column-number-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Package setup -------------------------------------------

;; Theme
(load-theme 'leuven-dark t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Org mode -----------------------------------------------

(use-package org
  :config
  (setq org-startup-indented t
        org-hide-leading-stars t
	org-pretty-entities t
	org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook 'org-indent-mode))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(use-package org-modern
  :ensure t
  :custom
  (org-modern-hide-stars nil); adds extra indentation
  (org-modern-table nil)
  (org-modern-list 
   '(;; (?- . "-")
     (?* . "•")
     (?+ . "‣")))
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(setq org-ellipsis "⋱")

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
		  :files (:defaults (:exclude "helm-org-ql.el"))))

(add-to-list 'load-path "~/.emacs.d/org-timeblock/")
(require 'org-timeblock)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(setq org-agenda-files '("~/org/"))

(use-package htmlize
  :ensure t)

(use-package git
  :ensure t
  :init
  (unless (file-exists-p "~/.emacs.d/reveal.js/")
    (shell-command "git clone https://github.com/hakimel/reveal.js.git ~/.emacs.d/reveal.js")))

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "file:///home/mbrignall/.emacs.d/reveal.js"))

(setq org-ditaa-jar-path "~/.emacs.d/ditaa.jar")


;; Copilot -----------------------------------------------

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el" "*.org"))
  :ensure t)

(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; Format All --------------------------------------------------

(use-package format-all
  :ensure t
  :hook (prog-mode . format-all-mode))

(setq format-all-formatters '(("Python" black) ("HTML" web-beautify) ("Nix" nixfmt)))
(setq format-all-buffer-format-on-save t)


;; Tree-sitter --------------------------------------------

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Eglot --------------------------------------------------

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (nix-mode . eglot-ensure)
         (css-mode . eglot-ensure)
         (html-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :config
  (setq nix-indent-function #'nix-indent-line))

;; Dashboard with Rubin quotes ----------------------

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "The Emacs text editor")
  ;; Set the banner image
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner 'logo)
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" or "path/to/your/text.txt"
  ;; which displays whatever image/text you would prefer

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

;; Ivy ----------------------------------------------------

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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Add prescient and ivy-prescient better sorting and filtering
(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after (ivy prescient)
  :config
  (ivy-prescient-mode 1))

;; Temp helpers -------------------------------------------

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode:
  :config
  (setq which-key-idle-delay 1))

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

;; Vterm config --------------------------------------------
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(global-set-key [f2] 'vterm-toggle)
(global-set-key [C-f2] 'vterm-toggle-cd)

;; Treemacs config ------------------------------------------
(use-package treemacs
  :bind ("<f8>" . treemacs))


;; Projectile Configuration ---------------------------------

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

;; Magit Configuration --------------------------------------

(use-package magit
  :init
  (message "Loading Magit!")
  :config
  (message "Loaded Magit!")
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))





