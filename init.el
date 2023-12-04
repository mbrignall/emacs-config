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

;; Performance tweaking for modern machines
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Keyboard-centric user interface
(setq inhibit-startup-message t)
  (tool-bar-mode -1)
  (menu-bar-mode t)
  (scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(electric-pair-mode t)
(show-paren-mode 1)
;;(setq-default indent-tabs-mode nil)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)

;; Set up the visible bell
(setq visible-bell nil)

;; Kill the warning about cl
(setq byte-compile-warnings '(cl-functions))
(setq mouse-autoselect-window t)
(column-number-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq use-dialog-box nil)

(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier nil)

;; Package setup -------------------------------------------

(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(set-face-attribute 'mode-line-active nil :inherit 'mode-line)

;; For current frame
;;(set-frame-parameter nil 'alpha-background 80)
;; For all new frames henceforth
;;(add-to-list 'default-frame-alist '(alpha-background . 80))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Backup files setup

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Org mode -----------------------------------------------

(use-package org
  :config
  (setq org-startup-indented t
        org-hide-leading-stars t
	org-pretty-entities t
	org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode))

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

(setq org-ellipsis " ▾")

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package olivetti
  :ensure t
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (olivetti-mode 1)
              (setq olivetti-body-width 100)
              (define-key org-mode-map (kbd "<C-mouse-4>") 'text-scale-increase)
              (define-key org-mode-map (kbd "<C-mouse-5>") 'text-scale-decrease))))


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Use keybindings
(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "DONE")))

(setq org-agenda-files '("~/org/"))

;; (use-package htmlize
;;   :ensure t)

(use-package git
  :ensure t
  :init
  (unless (file-exists-p "~/.emacs.d/reveal.js/")
    (shell-command "git clone https://github.com/hakimel/reveal.js.git ~/.emacs.d/reveal.js")))

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "file:///home/mbrignall/.emacs.d/reveal.js"))

;; (setq org-ditaa-jar-path "~/.emacs.d/ditaa.jar")

;; (use-package pdf-tools
;;   :load-path "site-lisp/pdf-tools/lisp"
;;   :magic ("%PDF" . pdf-view-mode)
;;   :config
;;   (pdf-tools-install :no-query))


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

(setq format-all-formatters
      '(("python" ruff)
	("HTML" prettier)
	("Nix" nixfmt)
	("json" prettier)
	("css" prettier)
	("scss" prettier)
	("yaml" prettier)
	("markdown" markdownfmt)
	("javascript" prettier)
	("typescript" prettier)
	("sh" shfmt)
	))
(setq format-all-buffer-format-on-save t)


;; ;; Tree-sitter --------------------------------------------

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

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

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
	(go-mode . go-ts-mode)
	(html-mode . html-ts-mode)
	(bash-mode . bash-ts-mode)
	(js2-mode . js-ts-mode)
	(json-mode . json-ts-mode)
	(typescript-mode . typescript-ts-mode)
	(json-mode . json-ts-mode)
	(css-mode . css-ts-mode)
	(python-mode . python-ts-mode)))

(use-package company
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Eglot --------------------------------------------------

(use-package eglot
  :hook ((python-mode . eglot-ensure)
	 (html-mode . eglot-ensure)
	 (go-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (nix-mode . eglot-ensure)
         (css-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package blacken
  :ensure t
  :init
  (setq blacken-line-length 80) ; Set your desired line length
  :hook (python-mode . blacken-mode))

;; Dashboard with Rubin quotes ----------------------

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome home")
  ;; Set the banner image
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner "~/.emacs.d/xemacs_color.svg")
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" or "path/to/your/text.txt"
  ;; which displays whatever image/text you would prefer
  
  (setq dashboard-items '((recents  . 5)
                          (projects . 5))))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(setq dashboard-display-icons-p t)
(setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
(setq dashboard-set-file-icons t)

(dashboard-modify-heading-icons '((recents . "file-text")
                                  (bookmarks . "book")))

(setq dashboard-footer-messages '("Zoom in and obsess. Zoom out and observe. We get to choose."))


;; Ivy and Counsel --------------------------------------------

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
        :init
        (ivy-mode 1))

(use-package ivy-prescient
        :after counsel
        :custom
        (ivy-prescient-enable-filtering nil)
        :config
        (prescient-persist-mode 1)
        (ivy-prescient-mode 1))

(use-package counsel
        :bind (("M-x" . counsel-M-x)
                 ("C-x b" . counsel-ibuffer)
                 ("C-x C-f" . counsel-find-file)
                 :map minibuffer-local-map
                 ("C-r" . 'counsel-minibuffer-history))
        :config
        (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq all-the-icons-scale-factor 0.8))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
(setq ivy-rich-path-style 'abbrev)

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))


;; Temp helpers -------------------------------------------

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode:
  :config
  (setq which-key-idle-delay 1))

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

(use-package vterm-toggle
  :ensure t
  :bind
  (("C-c t" . vterm-toggle)
   ("C-c T" . vterm-toggle-cd))
  :config
  ;; Customize vterm-toggle as needed
  )

;; Define a function to set the font for vterm
(defun set-vterm-font ()
  (interactive)
  (setq buffer-face-mode-face '(:family "FuraMono Nerd Font" :height 120))
  (buffer-face-mode 1))

;; Add a hook to apply the font settings when vterm is created
(add-hook 'vterm-mode-hook 'set-vterm-font)

;; Treemacs config ------------------------------------------

(use-package treemacs
  :bind ("<f1>" . treemacs))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit)
:ensure t)

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

;; Load Theme -----------------------------------------------

(use-package modus-themes
  :ensure t
  :config
  
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
	modus-themes-mixed-fonts t
	modus-themes-variable-pitch-ui nil
	modus-themes-custom-auto-reload t
	modus-themes-disable-other-themes t
	
	modus-themes-prompts '(italic bold)

	modus-themes-completions
	'((matches . (extrabold))
	  (selection . (semibold italic text-also))))

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi-tinted)
  
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))
