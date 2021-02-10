(require 'org) 
(org-babel-load-file (expand-file-name (concat user-emacs-directory "settings.org")))

(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" default))
 '(org-agenda-files '("~/Documents/eng.org"))
 '(package-selected-packages
   '(lsp-pyright treemacs-projectile treemacs flycheck dracula-theme clang-format yasnippet which-key use-package try switch-window spaceline rainbow-delimiters org-bullets modern-cpp-font-lock magit-popup magit lua-mode lsp-ui hungry-delete htmlize expand-region eglot doom-themes diminish dashboard cquery counsel company-lsp ccls beacon avy))
 '(projectile-project-root-files-top-down-recurring '("compile_commands.json" ".ccls" ".svn" "CVS" "Makefile"))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))
(put 'erase-buffer 'disabled nil)
