;; change the amount of garbage collection to speed up startup time
(setq gc-cons-threshold (* 50 1000 1000))

;; disable startup message
(setq inhibit-startup-message t)

;; disable menu bar
(menu-bar-mode -1)

;; disable visible scrollbar
(scroll-bar-mode -1)

;; disable the toolbar
(tool-bar-mode -1)

;; disable beeping
(setq visible-bell t)

;; disable tooltips
(tooltip-mode -1)

;; display 100 character ruler
(setq-default fill-column 100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; enable line word wrapping
;; if line is too long wrap it into next line
(global-visual-line-mode t)

;; set tab with to 4 spaces
(setq-default tab-width 4)

;; add window padding left and right
(set-fringe-mode 0)

;; set font
(set-frame-font "JetBrains Mono 12" nil t)

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; remember the position of your cursor in a file
(save-place-mode 1)

;; reopen files from previous session
;;(desktop-save-mode 1)

;; default emacs behaviour is to put backup and auto-save files
;; in the same directory as original file.
;; put backup files (~) in specific directory
(setq backup-directory-alist `(("." . "~/.cache/emacs")))
(setq backup-by-copying t)
;; set auto-save files (#) in specific directory
(setq auto-save-file-name-transforms `((".*" "~/.cache/emacs/" t)))
;; disable lockfiles
(setq create-lockfiles nil)

;; display line numbers
(global-display-line-numbers-mode t)

;; disable line numbers for some modes
(dolist (mode '(term-mode-hook
				vterm-mode-hook
				shell-mode-hook
				eshell-mode-hook
				dired-mode-hook
				))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Emacs pls don't shit auto-generated stuff in my init.el
;;(setq custom-file (concat user-emacs-directory "custom.el"))
;; hard-coded for now
(setq custom-file "/home/skmd/.emacs.d/custom.el")
(load custom-file)

;; initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
						 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; install use-package if it's not installed
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)

;; so you don't have to specify :ensure t on every package
(setq use-package-always-ensure t)

;; useful for debugging startup time
;;(setq use-package-verbose t)

;; color scheme (theme)
(use-package modus-themes
  :init
  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi))

;; org-mode
(use-package org
  :commands (org-capture org-agenda)
  :config
  (setq org-return-follows-link t))

;; org-roam settings
(use-package org-roam
  :after org
  :init
  ;; disable org-roam v2 migration warning at startup
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Code/org-roam")
  :config
  (org-roam-db-autosync-mode))

;; rainbow-delimiters colors the matching brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key shows the keybinding combo help in a minibuffer
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

;; unmap kill-sentence function
(global-set-key (kbd "M-k") nil)

;; custom function for commenting/uncommenting text
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;; package to setup keybinds in a nice way
;; (use-package general
;;   :config
;;   (general-create-definer skmd/leader-keys
;;     :keymaps '(normal insert visual emacs)
;;     :prefix "SPC"
;;     :global-prefix "C-SPC")
;;   (skmd/leader-keys
;;     "nn" '(org-roam-node-find :which-key "select org-roam file")
;;     "ni" '(org-roam-node-insert :which-key "insert org-roam link")
;;     "nt" '(org-roam-buffer-toggle :which-key "toggle org-roam buffer")
;;     "bb" '(ivy-switch-buffer :which-key "switch buffer")
;;     "bk" '(kill-current-buffer :which-key "kill current buffer")
;;     "bs" '(save-buffer :which-key "save buffer")
;;     "ff" '(counsel-find-file :which-key "find file")
;;     "fr" '(counsel-recentf :which-key "recently opened files")
;;     "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
;;     "pp" '(projectile-switch-project :which-key "switch to project - projectile")
;;     "dp" '(projectile-dired :which-key "open project directory - projectile")
;;     "ps" '(counsel-projectile-rg :which-key "search in the project - counsel+projectile+ripgrep")
;;     "SPC" '(projectile-find-file :which-key "find file in project - projectile")
;;     "gg" '(magit-status :which-key "git status page - magit")
;;     "dd" '(dired :which-key "dired")
;; 	"lr" '(lsp-find-references :which-key "LSP find references")
;; 	"ld" '(lsp-find-definition :which-key "LSP find definition")
;; 	"lc" '(lsp-rename :which-key "LSP rename")
;; 	"lt" '(dired-sidebar-toggle-sidebar :which-key "toggle project sidebar")
;; 	"ll" '(lsp-ivy-workspace-symbol :which-key "LSP ivy jump to symbol by name")
;; 	"lf" '(lsp-ui-sideline-apply-code-actions :which-key "LSP apply code action")
;; 	"db" '(dired-jump :which-key "open dired from current buffer")
;; 	"wb" '(split-window-below :which-key "split frame horizontally")
;; 	"wv" '(split-window-right :which-key "split frame vertically")
;; 	"wq" '(delete-window :which-key "close current window")
;; 	"wc" '(delete-other-windows :which-key "close other windows")
;; 	"we" '(balance-windows :which-key "resize windows to equal size")
;; 	"ww" '(other-window :which-key "cycle between opened windows")
;; 	;; "wl" '(evil-window-right :which-key "move cursor to the window on the right")
;; 	;; "wh" '(evil-window-left :which-key "move cursor to the window on the left")
;; 	;; "wj" '(evil-window-down :which-key "move cursor to the window below")
;; 	;; "wk" '(evil-window-up :which-key "move cursor to the window above")
;; 	"ss" '(yas-insert-snippet :which-key "insert snippet")
;; 	"sn" '(yas-new-snippet :which-key "create a new snippet")
;; 	"mn" '(bookmark-set :which-key "set a bookmark")
;; 	"mm" '(counsel-bookmark :which-key "open a bookmark")
;; 	"md" '(bookmark-delete :which-key "delete a bookmark")
;; 	"ml" '(bookmark-bmenu-list :which-key "list all bookmarks")
;; 	))

;; project management package
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Code")
    (setq projectile-project-search-path '("~/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; git front-end
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Language Server Protocol for ide-like features
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-headerline-breadcrumb-enable nil))

;; additional LSP UI improvements
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; code snippet system
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/Code/emacs-snippets"))
  (yas-global-mode 1))

;; Go programming language intergration
(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred))
;; add hooks to format code and organize imports on save
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;; add hooks to enable LSP and yas snippets while working with Go code
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; better terminal emulator
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;; configuration for built-in directory manager
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom
  ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (setq delete-by-moving-to-trash t))
  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;; 	"h" 'dired-single-up-directory
  ;; 	"l" 'dired-single-buffer))

;; just use 1 dired instance instead of having multiple opened for each directory
(use-package dired-single
  :after dired)
;; dired-open is another useful emacs package to open external applications by specific extension

;; package to handle showing/hiding of dotfiles
(use-package dired-hide-dotfiles
  ;; hide dotfiles by default
  :hook (dired-mode . dired-hide-dotfiles-mode))
  ;; :config
  ;; ;; set H keybind to toggle visibility
  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;; 	"H" 'dired-hide-dotfiles-mode))

;; (use-package dired-sidebar
;;   :commands (dired-sidebar-toggle-sidebar))

;; Enable undo-tree, sane undo/redo behavior
(use-package undo-tree
  :init (global-undo-tree-mode))

(use-package avy)

(global-set-key (kbd "M-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-t") 'vterm-other-window)

;; REPLACE ;;

;; ivy - suite of packages to show options in a list that you can filter through
(use-package ivy
  :diminish
  :bind (("M-s" . swiper-isearch)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
		 ("C-y" . ivy-kill-ring-save)
		 ("C-p" . yank)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; set that ivy doesn't start with ^
(setq ivy-initial-inputs-alist nil)

;; ivy-rich adds description and keybinds to ivy list items
(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-;" . counsel-M-x)
	 :map minibuffer-local-map
	 ("M-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

;; LSP ivy - useful for navigating to function/type that you know by name
(use-package lsp-ivy
  :after lsp)
;; custom C-c C- keybinds
(use-package ctrlf)
(ctrlf-mode +1)
(global-set-key (kbd "C-c C-f") 'ctrlf-forward-default)
(global-set-key (kbd "C-c C-S-f") 'ctrlf-forward-alternate)

;; auto-completion while you type
(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0))

(use-package boon)
(require 'boon-qwerty) ;; for qwerty variant
(boon-mode) ;; to enable boon everywhere

;; change the garbage collection back to normal after everything gets loaded
(setq gc-cons-threshold (* 2 1000 1000))
