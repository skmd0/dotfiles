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

;; set font - this causes issues with daemon mode
(set-frame-font "JetBrains Mono 12" nil t)

;; y/n for answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; don't show warning for opening symlinked files
(setq vc-follow-symlinks t)

;; turn on recent files
(recentf-mode 1)

;; remember the position of your cursor in a file
(save-place-mode 1)

;; reopen files from previous session
;;(desktop-save-mode 1)

;; replace selected text when you type new text
;; default behaviour is to just insert text after the cursor
;;(delete-selection mode 1)

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

;; replace dabbrev keybinds with hippie-expand
;; hippie-expand is just newer and better version of dabbrev
(global-set-key [remap dabbrev-expand] 'hippie-expand)

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
(setq custom-file "~/.config/emacs/custom.el")
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

;; custom function for commenting/uncommenting text
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;; project management package
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  ;; :custom ((projectile-completion-system 'vertico))
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

;; code snippet system
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/Code/emacs-snippets"))
  (yas-global-mode 1))

;; auto-matically close the ([{"
(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;; package for LSP support
(use-package eglot)

;; Go programming language intergration
(use-package go-mode
  :mode "\\.go\\'")
;; automatically load eglot when go-mode is active
(add-hook 'go-mode-hook 'eglot-ensure)
;; add hooks to format code and organize imports on save
(defun skmd-eglot-go-mode-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer t t)
  (add-hook 'before-save-hook #'eglot-code-action-organize-imports t t))
(add-hook 'go-mode-hook #'skmd-eglot-go-mode-save)

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

;; package to show project tree in left sidebar
(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar))

;; Enable undo-tree, sane undo/redo behavior
(use-package undo-tree
  :init (global-undo-tree-mode))

(use-package avy)

(defun skmd/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

;; minibuffer menu (alternative to ivy)
(use-package vertico
  :bind (:map vertico-map
			  ("M-l" . vertico-insert)
			  ("M-j" . vertico-next)
			  ("M-k" . vertico-previous)
			  :map minibuffer-local-map
			  ("M-h" . skmd/minibuffer-backward-kill))
  :init
  (vertico-mode))

;; packing for better result filtering in minibuffer
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; make sure vertico saves history (useful for recent file selection)
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; (setq tab-always-indent 'complete)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(defun skmd/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :custom
  (consult-project-root-function #'skmd/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

;; auto-completion while you type
(use-package company
  :after eglot
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection))
  (:map company-active-map ("M-l" . company-complete-selection))
  (:map company-active-map ("M-j" . company-select-next))
  (:map company-active-map ("M-k" . company-select-previous))
  ;; (:map eglot-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0))

;; disable selecting item from company list with RETURN
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil))

;; custom function to edit current file as sudo
(defun sudo ()
  "Use TRAMP to edit current file as sudo"
  (interactive)
  (when buffer-file-name
	(find-alternate-file
	 (concat "/sudo:root@localhost:"
			 buffer-file-name))))

;; custom function for smarter C-DEL
(defun skmd/backward-kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
  (interactive)
  (if (looking-back "[ \n]")
      ;; delete horizontal space before us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
             (while (looking-back "[ \n]")
               (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (backward-kill-word 1)))

(use-package meow)
;; (add-to-list 'load-path "/home/skmd/Code/meow")
;; (load "meow")
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . consult-yank-from-kill-ring)
   '("q" . mode-line-other-buffer)
   '("Q" . pop-global-mark)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . avy-goto-char-2)
   '("u" . undo-tree-undo)
   '("U" . undo-tree-redo)
   '("v" . consult-line)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("{" . backward-paragraph)
   '("}" . forward-paragraph)
   ))
(meow-setup)
(meow-global-mode 1)
(setq meow-use-clipboard t)

;; custom function to scroll up by 2 lines
(defun skmd/scroll-up-by-2 ()
  (interactive)
  (scroll-up 2))

;; custom function to scroll down by 2 lines
(defun skmd/scroll-down-by-2 ()
  (interactive)
  (scroll-down 2))

;; meta key custom keybinds
;; (global-set-key (kbd "M-j") 'scroll-up-line)
(global-set-key (kbd "M-j") 'skmd/scroll-up-by-2)
;; (global-set-key (kbd "M-k") 'scroll-down-line)
(global-set-key (kbd "M-k") 'skmd/scroll-down-by-2)

(global-set-key (kbd "M-;") 'meow-M-x)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-l") 'recenter)
(global-set-key (kbd "M-e") 'consult-flymake)
(global-set-key (kbd "M-p") 'projectile-find-file)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-q") 'kill-this-buffer)
(global-set-key (kbd "M-b") 'switch-to-buffer)
;; emacs' native start of line is M-m so this goes well together
;; the reason for using these commands is as a shorthand for
;; meow's mg and xg keys
(global-set-key (kbd "M-n") 'end-of-line)
(global-set-key (kbd "M-r") 'consult-register-store)
(global-set-key (kbd "M-t") 'consult-register-load)
(global-set-key (kbd "M-f") 'consult-line)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-h") 'hippie-expand)

;; control key custom keybinds
(global-set-key (kbd "C-c t") 'vterm-other-window)
(global-set-key (kbd "C-c n") 'org-roam-node-find)
(global-set-key (kbd "C-c g") 'consult-ripgrep)
(global-set-key (kbd "C-c f") 'consult-find)
(global-set-key (kbd "C-c r") 'consult-recent-file)
(global-set-key (kbd "C-c p") 'projectile-switch-project)
(global-set-key (kbd "C-c d") 'dired-jump)
(global-set-key (kbd "C-c s") 'yas-insert-snipet)
(global-set-key (kbd "C-c b") 'consult-bookmark)
(global-set-key (kbd "M-DEL") 'skmd/backward-kill-word)

;; change the garbage collection back to normal after everything gets loaded
(setq gc-cons-threshold (* 2 1000 1000))
