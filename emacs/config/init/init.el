;
; "Imports"
;

;; Functions
(load-file (concat config-directory "pkg.el"))
(load-file (concat config-directory "util.el"))


;; Themes/Autoload
(add-to-list 'load-path
             (concat config-directory
                     (file-name-as-directory "autoload")))

(mapc 'load (file-expand-wildcards
             (concat config-directory
                     (file-name-as-directory "autoload") "*.el")))

(add-to-list 'custom-theme-load-path
             (concat config-directory
                     (file-name-as-directory "themes")))

;
; Custom variables
;

(custom-set-variables
 ;; Stop asking about themes
 '(custom-safe-themes t)

 ;; Remove warnings
 '(warning-minimum-level :error)

 ;; Enable line numbers
 '(display-line-numbers-type 'relative)

 ;; Remove startup screen
 '(inhibit-startup-screen t))

;
; Packages
;

;; LSP
(pkg/require
 'lsp-mode
 'lsp-ui
 'flycheck
 'company
 'lsp-treemacs
 'helm-lsp
 'lsp-ivy
 'dap-mode)

;; Misc packages
(pkg/require
 'multiple-cursors
 'yasnippet-snippets
 'yasnippet
 'magit
 'whitespace-cleanup-mode
 'elcord
 'evil)

;; Language packages
(pkg/require
 'gdscript-mode
 'yaml-mode
 'nasm-mode
 'rust-mode
 'markdown-mode
 'lua-mode
 'csharp-mode
 'go-mode
 'rainbow-mode
 'cmake-mode
 'nim-mode
 'meson-mode
 'typescript-mode)

;; Themes
(pkg/require
 'doom-themes
 'gruber-darker-theme)

;
; GUI stuff/UX
;

;; Set font
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font Mono 14"))

;; Set modes
(menu-bar-mode                    0)
(tool-bar-mode                    0)
(scroll-bar-mode                  0)
(column-number-mode               1)
(show-paren-mode                  1)
(global-display-line-numbers-mode 1)

;; Set theme
(util/set-theme 'sonokai-shusia)

;; Tabs and spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Formatting style
(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)
                         (local-set-key (kbd "C-c C-f") 'clang-format-buffer)))

(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

;; Whitespace
(defconst USE-WHITESPACE 1)

(defun set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode USE-WHITESPACE)
  (whitespace-cleanup-mode))

(add-hook 'prog-mode-hook 'set-up-whitespace-handling)

(setq whitespace-style
      (quote
       (face tabs spaces trailing
             space-before-tab newline indentation empty
             space-after-tab space-mark tab-mark)))

;; Dired
(setq dired-listing-switches "-lah --group-directories-first")

;
; Configure packages
;

;; Elcord
(elcord-mode)

;; Yasnippets
(yas-global-mode 1)
(yas-load-directory (concat config-directory
                            (file-name-as-directory "snippets")))

;; Multiple cursors
(setq mc/always-run-for-all t)
(global-set-key (kbd "C-x RET RET") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Company mode
(global-company-mode)
(add-hook 'tuareg-mode-hook
          (lambda ()
            (interactive)
            (company-mode 0)))

;; lsp-mode
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-position 'bottom)

(setq-default lsp-keymap-prefix "C-c l")

(add-hook 'prog-mode-hook #'lsp-deferred)

;; NASM mode
(add-to-list 'auto-mode-alist '("\\.asm?\\'" . nasm-mode))

;; EditorConfig
(editorconfig-mode 1)

(defun editorconfig-exists ()
  (interactive)
  (eval-and-compile (require 'editorconfig-core))
  (let ((result (editorconfig-core-get-nearest-editorconfig
                 default-directory)))
    (when result t)))

(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda()
                        (interactive)
                        (unless (not (editorconfig-exists))
                          (editorconfig-format-buffer))))))

;
; Keybindings
;

;; Duplicate line
(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'duplicate-line)

;; Compile
(global-set-key (kbd "C-c c") 'compile)

;; Terminal
(global-set-key (kbd "C-`") (lambda()
                              (interactive)
                              (split-window-below)
                              (other-window 1)
                              (term "/bin/zsh")))
