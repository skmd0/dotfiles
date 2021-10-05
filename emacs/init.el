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

;; enable line word wrapping
;; if line is too long wrap it into next line
(global-visual-line-mode t)

;; set tab with to 4 spaces
(setq-default tab-width 4)

;; change line height by adding spacing to it
;;(setq-default line-spacing 0)

;; add window padding left and right
;;(set-fringe-mode 10)

;; set font
(set-frame-font "Hack 12" nil t)

;; default emacs behaviour is to put backup and auto-save files
;; in the same directory as original file.
;; put backup files (~) in specific directory
(setq backup-directory-alist `(("." . "~/.cache/emacs")))
(setq backup-by-copying t)
;; set auto-save files (#) in specific directory
(setq auto-save-file-name-transforms `((".*" "~/.cache/emacs/" t)))

;; display line numbers
(global-display-line-numbers-mode t)

;; disable line numbers for some modes
(dolist (mode '(term-mode-hook
				vterm-mode-hook
				shell-mode-hook
				eshell-mode-hook
				treemacs-mode-hook
				))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
;; so you don't have to specify :ensure t on every plugin
(setq use-package-always-ensure t)

;; ivy - suite of packages to show options in a list that you can filter through
(use-package counsel
  :bind (("M-;" . counsel-M-x)
	 :map minibuffer-local-map
	 ("M-r" . 'counsel-minibuffer-history)))
(use-package swiper)
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
  :config (ivy-mode 1))

;; ivy-rich adds description and keybinds to ivy list items
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 10))

;; org-mode
(use-package org)

;; org-roam settings
(use-package org-roam
  :init
  ;; disable org-roam v2 migration warning at startup
  (setq org-roam-v2-ack t)
  :custom
  (setq org-roam-directory "~/org-roam")
  :config
  (org-roam-db-autosync-mode))

;; doom-themes color schemes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-one") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; rainbow-delimiters colors the matching brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key shows the keybinding combo help in a minibuffer
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.5))

;; unmap kill-sentence function
(global-set-key (kbd "M-k") nil)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "M-k") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-snipe
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;;;;;;;;; keybinds ;;;;;;;;;;
(use-package general
  :config
  (general-create-definer skmd/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (skmd/leader-keys
    "nn" '(org-roam-node-find :which-key "select org-roam file")
    "ni" '(org-roam-node-insert :which-key "insert org-roam link")
    "nt" '(org-roam-buffer-toggle :which-key "toggle org-roam buffer")
    "bb" '(ivy-switch-buffer :which-key "switch buffer")
    "bk" '(kill-current-buffer :which-key "kill current buffer")
    "bs" '(save-buffer :which-key "save buffer")
    "ff" '(counsel-find-file :which-key "find file")
    "fr" '(counsel-recentf :which-key "recently opened files")
    "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
    "pp" '(projectile-switch-project :which-key "switch to project - projectile")
    "pd" '(projectile-dired :which-key "open project directory - projectile")
    "ps" '(counsel-projectile-rg :which-key "search in the project - counsel+projectile+ripgrep")
    "SPC" '(projectile-find-file :which-key "find file in project - projectile")
    "gg" '(magit-status :which-key "git status page - magit")
    "dd" '(dired :which-key "dired")
	"lr" '(lsp-find-references :which-key "LSP find references")
	"ld" '(lsp-find-definition :which-key "LSP find definition")
	"lc" '(lsp-rename :which-key "LSP rename")
	"lt" '(lsp-treemacs-symbols :which-key "LSP types in side-menu")
	"ll" '(lsp-ivy-workspace-symbol :which-key "LSP ivy jump to symbol by name")
	))

;; project management package
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; completion functionality for projectile
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; git front-end
(use-package magit
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

;; side-menu to show types
(use-package lsp-treemacs
  :after lsp)

;; LSP ivy - useful for navigating to function/type that you know by name
(use-package lsp-ivy)

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

;; better company UI - commented for maybe future use
;;(use-package company-box
;;  :hook (company-mode . company-box-mode))

;; code snippet system
(use-package yasnippet)

;; Go programming language intergration
(use-package go-mode)
;; add hooks to format code and organize imports on save
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;; add hooks to enable LSP and yas snippets while working with Go code
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; built-in terminal emulator configuration
(use-package term
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;; better terminal emulator
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;; show fancy tabs from opened buffers
(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-style "bar"
		centaur-tabs-height 32
		centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-mode t)
  :bind
  ("M-[" . centaur-tabs-backward)
  ("M-]" . centaur-tabs-forward))
;; Emacs pls don't shit auto-generated stuff in my init.el
;;(setq custom-file (concat user-emacs-directory "custom.el"))
(setq custom-file "/home/skmd/code/dotfiles/emacs/custom.el")
(load custom-file)

(general-define-key
 "M-k" 'keyboard-escape-quit
 "M-;" 'counsel-M-x
 "M-o" 'org-open-at-point
 "M-b" 'evil-jump-backward)

(general-define-key
 :keymaps 'org-mode-map
 "C-h" 'org-shiftmetaleft
 "C-j" 'org-move-subtree-down
 "C-k" 'org-move-subtree-up
 "C-l" 'org-shiftmetaright
 "C-~" 'kill-sentence)

;; for some reason I can't override emacs default keybind with general
;; that's why I am overriding it here again until I find correct solution
(global-set-key (kbd "M-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-t") 'vterm-other-window)
