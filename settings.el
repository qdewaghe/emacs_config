(set-register ?e(cons 'file "~/.emacs.d/settings.org"))
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(set-window-scroll-bars (minibuffer-window) 0 'none)
	  (setq inhibit-startup-message t)

	  (tool-bar-mode -1)
	  (global-set-key (kbd "<f5>") 'revert-buffer)
	  (set-face-attribute 'default nil :height 150)


	  (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda ()
			   (org-bullets-mode 1))))

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
  :bind ("M-s" . avy-goto-word-1))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    (add-to-list 'ac-modes 'org-mode)
    ))

(use-package atom-one-dark-theme
  :ensure t)
(load-theme 'atom-one-dark t)

(use-package color-theme
  :ensure t)

(use-package magit
  :ensure t
  :init
  (bind-key "C-x g" 'magit-status))

(setq c-default-style "bsd"
      c-basic-offset 3)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1))))
)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(yas-reload-all)

(use-package projectile
:ensure t
:config
(projectile-global-mode)
(setq projectile-completion-system 'ivy))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-z") 'undo)

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
    "A minor mode that overrides default keys of major modes."
    :init-value t
    :lighter " my-keys")

    (my-keys-minor-mode 1)
