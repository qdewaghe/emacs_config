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

(setq display-time-24hr-format t)
(display-time-mode)

(setq electric-pair-pairs
       '(
  	 (?\" . ?\")
  	 (?\{ . ?\})))

	 (electric-pair-mode)

(global-auto-revert-mode t)

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(global-hl-line-mode t)

(use-package diminish
  :ensure t
  :init
  (diminish 'my-keys-minor-mode)
  (diminish 'company-mode)
  (diminish 'hungry-delete-mode))

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
  (bind-key "C-c g" 'magit-status))

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

;;(use-package flycheck
;;  :ensure t
;;  :init 
;;  (add-hook 'c++-mode-hook #'flycheck-mode))

(use-package dashboard
  :ensure t)


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
			   (agenda . 5)))
  (dashboard-setup-startup-hook)
  :hook ((after-init     . dashboard-refresh-buffer)
	  (dashboard-mode . my/dashboard-banner)))

(use-package expand-region
  :ensure t)

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcut
	'("a", "o", "e", "u", "i", "d", "h", "t", "n", "s"))
  :bind
  ([remap other-window] . switch-window))

(use-package spaceline
:ensure t
:init
(setq powerline-default-separator 'slant)
:config
(spaceline-emacs-theme)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-evil-state-on))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(setq indo-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(defalias 'list-buffers 'ibuffer)
(use-package counsel
  :ensure t)

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

;;f has to be lowercase btw
  (defun config-reload()
    (interactive)
    (org-babel-load-file (expand-file-name "~/.emacs.d/settings.org")))
  (global-set-key (kbd "<f5>") 'config-reload)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
  ;;two versions so it works anyway if I press 2 too fast
(global-set-key (kbd "C-c 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-c C-2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))
    ;;two versions so it works anyway if I press 3 too fast
(global-set-key (kbd "C-c 3") 'split-and-follow-vertically)
(global-set-key (kbd "C-c C-3") 'split-and-follow-vertically)

(defun duplicate-line ()
  (interactive)
  (let ((col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (move-to-column col)))

(global-set-key (kbd "C-c h") 'duplicate-line)

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(global-set-key (kbd "C-c t") 'move-line-down)

(defun move-line-up ()
   (interactive)
   (let ((col (current-column)))
     (save-excursion
       (forward-line)
       (transpose-lines -1))
     (forward-line -1)
     (move-to-column col)))

(global-set-key (kbd "C-c c") 'move-line-up)

(setq c-default-style "bsd"
      c-basic-offset 3)

(use-package cquery
  :ensure t
  :init
  (setq cquery-executable "~/.emacs.d/cquery/build/release/bin/cquery")
  :config
  (add-hook 'c-mode-common-hook 'lsp))

(defun cquery//enable ()
  (condition-case nil
      (lsp)
    (user-error nil)))

;; I only managed to make cquery work by putting it twice
(use-package cquery
  :ensure t
  :commands lsp
  :init (add-hook 'c-mode-hook #'cquery//enable)
  (add-hook 'c++-mode-hook #'cquery//enable))

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

    ;;Tab = C-i thing
    (define-key input-decode-map (kbd "C-i") (kbd "H-i"))

    ;;my way of avoiding emacs' pinky (caps lock is rebinded to backspace already)
    (define-key input-decode-map (kbd "C-SPC") (kbd "C-c"))

    ;;window with two versions so it works even if I press the number too fast
    (define-key map (kbd "C-c 1") 'delete-other-windows)
    (define-key map (kbd "C-c C-1") 'delete-other-windows)
    (define-key map (kbd "C-c 0") 'delete-window)
    (define-key map (kbd "C-c C-0") 'delete-window)
    (define-key map (kbd "C-c o") 'switch-window)

    ;;switch buffer
    (define-key map (kbd "C-c b") 'ivy-switch-buffer)
    (define-key map (kbd "C-c C-b") 'ivy-switch-buffer)

    ;;copy/cut paste	
    (define-key map (kbd "C-c C-y") 'kill-ring-save)
    (define-key map (kbd "C-c f") 'kill-region)

    ;;mark
    (define-key map (kbd "<C-tab>") 'set-mark-command)

    ;;navigation by one
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

    ;;expand region
    (define-key map (kbd "C-e") 'er/expand-region)
    (define-key map (kbd "C-r") 'er/contract-region)

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
