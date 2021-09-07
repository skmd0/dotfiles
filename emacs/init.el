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

;; change line height by adding spacing to it
;;(setq-default line-spacing 0)

;; add window padding left and right
;;(set-fringe-mode 10)

;; disable org-roam v2 migration warning at startup
(setq org-roam-v2-ack t)

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
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-number-mode 0))))

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
  :bind (("M-s" . swiper)
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

;; org-mode configs
(use-package org)

;; org-roam settings
(setq org-roam-directory "~/org/org-roam")
(use-package org-roam)

;; doom-themes color schemes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-one-light") ; use "doom-colors" for less minimal icon theme
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
  :config (setq which-key-idle-delay 0))

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
    "dd" '(dired :which-key "dired")))

(general-define-key
 "M-k" 'keyboard-escape-quit
 "M-;" 'counsel-M-x
 :keymaps 'org-mode-map
 "C-h" 'org-shiftmetaleft
 "C-j" 'org-move-subtree-down
 "C-k" 'org-move-subtree-up
 "C-l" 'org-shiftmetaright
 "C-~" 'kill-sentence)

(global-set-key (kbd "M-/") 'comment-or-uncomment-region-or-line)

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

;;;;;;; auto-GENERATED ;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-leader evil-magit magit counsel-projectile projectile evil-snipe good-scroll evil-collection evil general counsel ivy-rich which-key rainbow-delimiters swiper use-package spinner org-roam markdown-mode lv ivy ht doom-themes doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
