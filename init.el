(setq inhibit-splash-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-revert-interval 3)
(setq auto-revert-check-vc-info t)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))


(when (string= system-type "darwin")
  (setq mac-command-modifier 'meta))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)


(ido-mode t)

(menu-bar-mode -1)

(savehist-mode)

(set-language-environment "UTF-8")

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(setq enable-recursive-minibuffers nil)
(setq completion-cycle-threshold 1)
(setq completions-detailed t)
(setq tab-always-indent t)
(setq completion-styles '(basic initials substring))
(setq completion-auto-help 'always)
(setq completion-auto-select 'second-tab)
(setq completions-max-height 10)

(if (version< emacs-version "29")
    (user-error "We need at least emacs 29"))

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install t)
  (setq treesit-auto-opt-out-list '(protobuf))
  (global-treesit-auto-mode))


(fido-vertical-mode)
(setq icomplete-delay-completions-threshold 4000)
(define-key minibuffer-mode-map (kbd "TAB") 'minibuffer-complete)


(setq line-number-mode t)
(setq column-number-mode t)

(setq x-underline-at-descent-line nil)
(setq switch-to-buffer-obey-display-actions t)

(setq-default show-trailing-whitespace nil)
(setq-default indicate-buffer-boundaries 'left)

(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

(blink-cursor-mode -1)
(pixel-scroll-precision-mode)

(cua-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))


(setq tab-bar-show 0)

(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setq display-time-format "%a %F %T")
(setq display-time-interval 1)
(setq js-indent-level 2)

(display-time-mode)

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

(use-package vertico
  :ensure t
  :init
  (fido-mode -1)
  (vertico-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  (corfu-on-exact-match nil)
  :init
  (global-corfu-mode))


(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))


(use-package corfu-terminal
  :ensure t)

(unless (display-graphic-p)
  (corfu-terminal-mode +1))

(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package flx
  :ensure t)

(use-package flx-ido
  :ensure t
  :init (flx-ido-mode 1))


(use-package magit
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t))


(use-package rg
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (flycheck-add-mode 'ruby-rubocop 'ruby-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'js-ts-mode)
  (setq flycheck-ruby-rubocop-executable "~/.asdf/shims/rubocop")
  (setq flycheck-javascript-eslint-executable "~/.asdf/shims/eslint")
  :init (global-flycheck-mode))

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package vterm
  :ensure t
  :bind
  (("C-S-y" . vterm-yank)))

(use-package multi-vterm
  :ensure t
  :bind
  (("C-c v t" . multi-vterm-project)))


(use-package ruby-end
  :ensure t
  :config
  (setq ruby-end-insert-newline nil))

(use-package web-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(use-package popper
  :ensure t
  :bind
  (("C-`"   . popper-toggle)
   ("M-`"   . popper-cycle)
   ("C-M-=" . popper-toggle-type))
  :config
  (setq popper-reference-buffers
      (append popper-reference-buffers
              '("^\\*eshell.*\\*$" eshell-mode
                "^\\*shell.*\\*$"  shell-mode
                "^\\*term.*\\*$"   term-mode
                "^\\*vterm.*\\*$"  vterm-mode
		"\\*Messages\\*"
		"\\*Async-native-compile-log\\*"
		 "Output\\*$"
		 help-mode
		 compilation-mode)))
  (setq popper-window-height 0.33))

(popper-mode +1)
(popper-echo-mode +1)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("C-c t" . treemacs/toggle))
  :config
  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-fringe-indicator-mode 'always))


(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)


(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun go-to-emacs-init-file ()
  "Go to init.el under .emacs.d"
  (interactive) (find-file (f-join user-emacs-directory "init.el")))


(defun treemacs/toggle ()
  "Initialize or toggle treemacs"
  (interactive)
  (require 'treemacs)
  (pcase (treemacs-current-visibility)
    (`visible (delete-window (treemacs-get-local-window)))
    (_ (if (projectile-project-p)
           (treemacs-add-and-display-current-project-exclusively)
         (treemacs)))))

(use-package page-break-lines
  :ensure t)

(use-package org-superstar
  :ensure t
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-items '((projects . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-page-separator "\n\f\n")
  :config
  (dashboard-setup-startup-hook)
  (page-break-lines-mode))

(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-c w"))
  :init
  (persp-mode))

(use-package consult
  :ensure t
  :config
  (consult-customize
   consult-ripgrep consult-buffer :preview-key nil
   consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  :bind (("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
	 ("C-c p f" . consult-find)
	 ("C-c p s" . consult-ripgrep)
	 ("C-c p p" . projectile-switch-project)))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package zig-mode
  :ensure t)



(use-package lsp-bridge
  :vc (:fetcher "github" :repo "manateelazycat/lsp-bridge")
  :config
  ;; works only if the langserver setting is enabled
  (setq lsp-bridge-enable-inlay-hint t)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq lsp-bridge-signature-show-function 'message)
  ;; (setq lsp-bridge-user-langserver-dir "~/.emacs.d/langserver")
  ;;(setq lsp-bridge-epc-debug t)
  ;;(setq acm-enable-doc t)
  :hook ((rust-ts-mode . my/lsp-bridge-init)
	 (go-ts-mode . my/lsp-bridge-init)
	 (zig-mode . my/lsp-bridge-init)
	 (java-ts-mode . my/lsp-bridge-init)
	 (js-jsx-mode . my/jsx-lsp-bridge-init))
  :bind (("M-." . 'lsp-bridge-find-def)
	 ("M-?" . 'lsp-bridge-find-references)
	 ("M-," . 'lsp-bridge-find-def-return)
	 ("M-\\" . 'lsp-bridge-code-format)
	 ("M-[" . 'lsp-bridge-code-action)
	 ("M-]" . 'my/lsp-bridge-toggle-inlay-hints)
	 ("M-p" . 'lsp-bridge-popup-documentation)
	 ("M-n" . 'lsp-bridge-rename)
	 ("M-o" . 'lsp-bridge-diagnostic-list)
	 ("M-<up>" . 'lsp-bridge-popup-documentation-scroll-down)
	 ("M-<down>" . 'lsp-bridge-popup-documentation-scroll-up)))



(defun my/lsp-bridge-init ()
  "Initialize lsp-bridge."
  (corfu-mode -1)
  (flycheck-mode -1)
  (lsp-bridge-mode 1))

(defun my/jsx-lsp-bridge-init ()
  "Initialize JSX lsp-bridge."
  (my/lsp-bridge-init)
  (setq-local tab-width 2))

(defun my/lsp-bridge-toggle-inlay-hints ()
  "Toggle inlay hints."
  (interactive)
  (if (null lsp-bridge-enable-inlay-hint)
      (setq lsp-bridge-enable-inlay-hint t)
    (setq lsp-bridge-enable-inlay-hint nil))
  (lsp-bridge-restart-process))

(use-package dart-mode
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-M-j" . mc/mark-all-dwim)
   ("C-M-c" . mc/edit-lines)
   ("C-M-/" . mc/mark-all-like-this)
   ("C-M-," . mc/mark-previous-like-this)
   ("C-M-." . mc/mark-next-like-this)
   ("C-M-<" . mc/skip-to-previous-like-this)
   ("C-M->" . mc/skip-to-next-like-this)
   ("C-M-;" . mc/edit-beginnings-of-lines)
   ("C-M-'" . mc/edit-ends-of-lines)))

(use-package terraform-mode
  :ensure t
  :config
  (setq terraform-format-on-save t))

;; autoloads
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\go.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\Capfile\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts?\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.y?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(add-to-list 'auto-mode-alist '("\\.*Dockerfile.*\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\*dashboard*\\'" . dashboard-mode))


;; keybindings
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)
(global-set-key (kbd "C-c f p") 'go-to-emacs-init-file)
(global-set-key (kbd "C-.") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))
