(require 'org) 
(org-babel-load-file (expand-file-name (concat user-emacs-directory "settings.org")))

(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (clang-format yasnippet which-key use-package try switch-window spaceline rainbow-delimiters org-bullets modern-cpp-font-lock magit-popup magit lua-mode lsp-ui hungry-delete htmlize expand-region eglot doom-themes diminish dashboard cquery counsel company-lsp ccls beacon avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
