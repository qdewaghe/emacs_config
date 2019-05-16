(require 'org) 
(org-babel-load-file (expand-file-name (concat user-emacs-directory "settings.org")))

;;Generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" default)))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (C . t))))
 '(package-selected-packages
   (quote
    (ccls dired+ lua-mode doom-themes doom-modeline spaceline switch-window expand-region h diminish dashboard hungry-delete rainbow-delimiters rainbow-delimiters-mode rainbow-mode beacon eglot lsp-ui cquery counsel-projectile projectile yasnippet-snippets flycheck magit zenburn-theme which-key use-package try org-bullets org counsel color-theme auto-org-md auto-complete atom-one-dark-theme ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
