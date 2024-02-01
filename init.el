;;; package --- Summary

;;; Commentary:

;; My Emacs Configuration 2024

;;; init.el --- This is where all Emacs start.

;;; Code:
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

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

(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-use-package 'use-package))

;; Refresh package archives (GNU Elpa)
(unless package-archive-contents
  (package-refresh-contents))

;; Keyboard-centric user interface
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(electric-pair-mode t)
(show-paren-mode 1)
;;(setq-default indent-tabs-mode nil)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)
;; Set up the visible bell
(setq visible-bell t)
(global-hl-line-mode)
(setq ring-bell-function 'ignore)
(setq standard-indent 2)

;; Kill the warning about cl
(setq byte-compile-warnings '(cl-functions))
(setq mouse-autoselect-window t)
(column-number-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq use-dialog-box nil)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (setq use-dialog-box nil)

(fset 'insertHash "#")
(global-set-key (kbd "M-3") 'insertHash)

;; OSX Specifics ---------------------------------------------

(when (equal system-type 'darwin)
  ;; Treat option as meta and command as super
  (setq mac-option-key-is-meta t)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta))

;; Path ------------------------------------------------------

(use-package exec-path-from-shell
    :config (exec-path-from-shell-initialize))

;; zsh -------------------------------------------------------
  
'(explicit-shell-file-name "/bin/zsh")
'(explicit-zsh-args '("--interactive" "--login"))
'(comint-process-echoes 0)

;; Dashboard with Rubin quotes --------------------------------

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/.emacs.d/xemacs_color.svg"
        dashboard-banner-logo-title "Welcome home"
	dashboard-center-content t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 5) (projects . 5))
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-footer-messages '("Zoom in and obsess. Zoom out and observe. We get to choose.")))

;; Package setup -------------------------------------------

(use-package nerd-icons
  :ensure t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; (set-face-attribute 'mode-line-active nil :inherit 'mode-line)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; deal with backups and autosaves
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

;; global autorevert for buffer changes
(global-auto-revert-mode t)

;; Eshell config --------------------------------------------

(use-package eshell-toggle
  :ensure t
  :bind
  (("C-c e" . eshell-toggle)))

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :ensure t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-git-prompt
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))

(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

;; vterm config ---------------------------------------------

(use-package vterm-toggle
  :ensure t
  :bind
  (("C-c v" . vterm-toggle)
   :map vterm-mode-map
   ("C-c V" . vterm-toggle-insert-cd)))

;; Org mode --------------------------------------------------

(use-package org
  :defer t
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
  (org-modern-hide-stars nil); adds extr aindentation
  (org-modern-table nil)
  (org-modern-list
   '((?- . "-")
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
  :hook (text-mode . olivetti-mode)
  :config
  (setq-default olivetti-body-width 120))

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

(use-package htmlize
  :ensure t)

(use-package ox-reveal
  :ensure t)

(use-package pandoc-mode
  :ensure t)

(use-package ox-pandoc
  :ensure t)

 (setq org-latex-compiler "xelatex")
    (setq org-latex-pdf-process '("xelatex %f"))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; LANGUAGES -------------------------------------------------

(use-package eglot
  :ensure t
  :defer t)

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-inline
  :ensure t
  :after flycheck
  :config
  (global-flycheck-inline-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . 'mc/edit-lines)
	 ("C-c m n" . 'mc/mark-next-like-this)
	 ("C-c m p" . 'mc/mark-previous-like-this)
	 ("C-c m a" . 'mc/mark-all-like-this)))

;; Copilot ---------------------------------------------------

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config
  (setq copilot-use-childframe t)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))
(add-hook 'prog-mode-hook 'copilot-mode)

;; Format All --------------------------------------------------

(use-package format-all
  :ensure t
  :hook (prog-mode . format-all-mode))

;; Tree-sitter -------------------------------------------------

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :ensure t
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
		      :foreground 'unspecified
		      :inherit 'error))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package poetry
 :ensure t)

;; Go ---------------------------------------------------------

(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

;; webdev -----------------------------------------------------

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
	 ("\\.jsx\\'" . js2-jsx-mode))
  :config
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings t))

;; Tide for TypeScript/JSX/TSX
(use-package tide
  :ensure t
  :after (typescript-mode corfu flycheck js2-mode)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (web-mode . (lambda ()
                       (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
                                 (string-equal "jsx" (file-name-extension buffer-file-name)))
                         (tide-setup))))))

;; Web Mode for Django templates
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)) ;; For Django templates
  :config
  (setq web-mode-content-types-alist '(("html" . "\\.html\\'")))
  (setq web-mode-engines-alist '(("django" . "\\.html\\'"))))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")


;; Temp helpers -------------------------------------------

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package which-key
:demand t
:config
(progn
  (which-key-setup-side-window-bottom)
  (which-key-mode)))

;; Treemacs config ------------------------------------------

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs
  :ensure t
  :bind ("C-x t t" . treemacs))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t)

;; Projectile Configuration ---------------------------------

(use-package projectile
  :diminish projectile-mode
  :custom ((projectile-completion-system 'corfu))
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Magit Configuration --------------------------------------

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; ;; Load Theme -----------------------------------------------

(require-theme 'modus-themes)
(load-theme 'modus-vivendi)

;; Vertico and Counsel -----------------------------------------------

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 0)
  
  ;; Show more candidates
  (setq vertico-count 10)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize nil)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(advice-add #'vertico--format-candidate :around
                                        (lambda (orig cand prefix suffix index _start)
                                          (setq cand (funcall orig cand prefix suffix index _start))
                                          (concat
                                           (if (= vertico--index index)
                                               (propertize "> " 'face 'vertico-current)
                                             "  ")
                                           cand)))

;; Configure directory extension.
(use-package vertico-directory
  :straight nil
  :load-path "~/.emacs.d/straight/repos/vertico/extensions/vertico-directory.el"
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(setq completion-auto-help 'always)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package all-the-icons
  :ensure t)

(use-package terraform-mode
  :ensure t)

;; consult ---------------------------------------------------

;; Example configuration for Consult
(use-package consult
  :ensure t)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-popupinfo-mode t)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; Enable Corfu more generally for every minibuffer, as long as no other
;; completion UI is active. If you use Mct or Vertico as your main minibuffer
;; completion UI. From
;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
              (bound-and-true-p vertico--input))
    (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

(use-package kind-icon
  :ensure t
  :after corfu
  ;:custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Slime -----------------------------------------------------

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/run/current-system/sw/bin/sbcl")
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (setq slime-contribs '(slime-fancy)))

;; Elfeed ----------------------------------------------------

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
	'(("https://www.reddit.com/r/emacs.rss" emacs)
	  ("https://www.reddit.com/r/nixos.rss" nixos)
	  ("https://www.reddit.com/r/emacs.rss" emacs)
	  ("https://www.reddit.com/r/emacs.rss" emacs))))

(provide 'init)
;;; init.el ends here
