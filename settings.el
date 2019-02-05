(set-register ?e(cons 'file "~/.emacs.d/settings.org"))
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
         ("marmalade" . "http://marmalade-repo.org/packages/")
         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;removes the scrollbar in the minibuffer
(set-window-scroll-bars (minibuffer-window) 0 'none)
;;removes all scrollbars
(scroll-bar-mode -1)

(tool-bar-mode -1)
(menu-bar-mode -1)

(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(defvar my-term-shell "/bin/bash")
(global-set-key (kbd "<s-return>") 'shell)

(setq scroll-conservatively 100)

(delete-selection-mode)

(set-face-attribute 'default nil :height 175)

(global-subword-mode 1)

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(global-hl-line-mode t)
(global-set-key (kbd "<f5>") 'revert-buffer)

(setq display-time-24hr-format t)
(display-time-mode)

(use-package diminish
  :ensure t)

(eval-after-load "my-keys" '(diminish 'my-keys-minor-mode))
(eval-after-load "beacon" '(diminish 'beacon-mode))
(eval-after-load "org mode src" '(diminish 'org-src-mode))
(eval-after-load "subword" '(diminish 'subword-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "hungry delete" '(diminish 'hungry-delete-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "company-mode" '(diminish 'company-mode))

(setq electric-pair-pairs
      '(
	(?\" . ?\")
	(?\{ . ?\})))

(electric-pair-mode)

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package beacon
    :ensure t
    :init
    (beacon-mode 1))

(use-package projectile
  :ensure t
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

(use-package swiper
:ensure try
  :bind (("C-f" . swiper)
	 ("C-F" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))
(use-package avy
  :ensure t
  :bind ("M-f" . avy-goto-word-1))

(use-package magit
  :ensure t
  :init
  (bind-key "C-x g" 'magit-status))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(yas-reload-all)

(use-package rainbow-delimiters
  :ensure t
  :config 
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package flycheck
  :ensure t
  :init 
  (add-hook 'c++-mode-hook #'flycheck-mode))

(use-package dashboard
  :preface
  (defun my/dashboard-banner ()
     "Set a dashboard banner including information on package initialization
  time and garbage collections."""
     (setq dashboard-banner-logo-title
	   (format "Emacs ready in %.2f seconds with %d garbage collections. "
		   (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
  :config
  (setq dashboard-startup-banner "~/.emacs.d/pepe.png")
  (setq dashboard-items '((projects . 5)
			   (recents . 5)
			   (agenda . 5)
			   ))
  (dashboard-setup-startup-hook)
  :hook ((after-init     . dashboard-refresh-buffer)
	  (dashboard-mode . my/dashboard-banner)))

(use-package atom-one-dark-theme
  :ensure t)
(load-theme 'atom-one-dark t)

(use-package color-theme
  :ensure t)

(setq indo-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(defalias 'list-buffers 'ibuffer)


(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))


(use-package counsel
  :ensure t
  )

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(setq org-src-window-setup 'current-window)

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda ()
			     (org-bullets-mode 1))))

(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
			'(("^ +\\([-*]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun kill-whole-word()
  (interactive)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c w w") 'kill-whole-word)

(setq c-default-style "bsd"
      c-basic-offset 3)

(use-package cquery
  :ensure t
  :commands lsp
  :init
  (setq cquery-executable "~/.emacs.d/cquery/build/release/bin/cquery")
  (add-hook 'c-mode-hook #'cquery//enable)
  (add-hook 'c++-mode-hook #'cquery//enable))
  :config
  (add-hook 'c-mode-common-hook 'lsp)

(defun cquery//enable ()
  (condition-case nil
      (lsp)
    (user-error nil)))

(use-package lsp-mode
  :ensure t
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "H-i") 'company-select-previous)
  (define-key company-active-map (kbd "C-k") 'company-select-next))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :init
  (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
  :config
  (push 'company-lsp company-backends))

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-z") 'undo)
    (define-key map (kbd "C-s") 'save-buffer)

    ;;navigation by one
    (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
    (define-key map (kbd "H-i") 'previous-line)
    (define-key map (kbd "C-k") 'next-line)
    (define-key map (kbd "C-j") 'backward-char)
    (define-key map (kbd "C-l") 'forward-char)

    ;;navigation by one element
    (define-key map (kbd "M-i") 'backward-sentence)	
    (define-key map (kbd "M-k") 'forward-sentence)
    (define-key map (kbd "M-j") 'backward-word)
    (define-key map (kbd "M-l") 'forward-word)

    ;;move end of *
    (define-key map (kbd "C-u") 'move-beginning-of-line)
    (define-key map (kbd "C-o") 'move-end-of-line)
    (define-key map (kbd "M-u") 'beginning-of-buffer)
    (define-key map (kbd "M-o") 'end-of-buffer)

    ;;deleting stuff
    (define-key map (kbd "M-<DEL>") 'kill-line)

    ;;main keys
    (define-key map (kbd "`") 'execute-extended-command)
      map)
    "my-keys-minor-mode keymap.")

    (define-minor-mode my-keys-minor-mode
    :init-value t
    :lighter " my-keys")

    (my-keys-minor-mode 1)
