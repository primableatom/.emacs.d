(setq inhibit-splash-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq make-backup-files nil)
(setq auto-revert-interval 3)
(setq auto-revert-check-vc-info t)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)


(ido-mode t)
(menu-bar-mode -1)

(savehist-mode)

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


(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(setq enable-recursive-minibuffers t)
(setq completion-cycle-threshold 1)
(setq completions-detailed t)
(setq tab-always-indent 'complete)
(setq completion-styles '(basic initials substring))

(setq completion-auto-help 'always)
(setq completion-auto-select 'second-tab)
(setq completions-max-height 10)

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
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode))

(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package consult
  :ensure t
  :config
  (consult-customize consult-ripgrep consult-buffer :preview-key nil)
  :bind (("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("C-s" . consult-line)
	 ("C-c p f" . consult-find)
	 ("C-c p s" . consult-ripgrep)
         ))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

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

(use-package projectile
  :ensure t)

(use-package rg
  :ensure t)

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-completion-provider :none)
  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  (rust-ts-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))


(use-package  lsp-ui
  :ensure t
  :commands lsp-ui-mode)


(use-package consult-lsp
  :ensure t
  :commands
  (consult-lsp-symbols consult-lsp-diagnostics consult-lsp-file-symbols))

(use-package flycheck
  :ensure t
  :config
  (flycheck-add-mode 'ruby-rubocop 'ruby-ts-mode)
  (setq flycheck-ruby-rubocop-executable "~/.asdf/shims/rubocop")
  :init (global-flycheck-mode))


(if (treesit-available-p)
    (setq treesit-extra-load-path '("~/.emacs.d/treesitter-grammers")))  

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package vterm
  :ensure t)

(use-package ruby-end
  :ensure t
  :config
  (setq ruby-end-insert-newline nil))

(use-package web-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(defun revert-buffer-no-confirm ()
1  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun go-to-emacs-init-file ()
  "Go to init.el under .emacs.d"
  (interactive) (find-file (f-join user-emacs-directory "init.el")))


;; keybindings

(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)
(global-set-key (kbd "C-c f p") 'go-to-emacs-init-file)


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))


