(setq config-directory
      (concat user-emacs-directory (file-name-as-directory "init")))
(load-file (concat config-directory "init.el"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 '(display-line-numbers-type 'relative)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(gruber-darker-theme doom-themes typescript-mode meson-mode nim-mode cmake-mode rainbow-mode go-mode lua-mode rust-mode nasm-mode yaml-mode gdscript-mode evil elcord whitespace-cleanup-mode magit yasnippet-snippets multiple-cursors dap-mode lsp-ivy helm-lsp lsp-treemacs company flycheck lsp-ui lsp-mode dash-functional dash))
 '(warning-minimum-level :error))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
