(setq user-full-name    "Adalberto Garcia"
      user-mail-address "mistergamer37@gmail.com")
(setq default-directory "C:/Users/rusty/Desktop/prog/")
(setq visible-bell 1)
(set-face-attribute 'default nil :font "Fixedsys-9" )
(load-theme 'modus-operandi t)
(global-set-key "\C-x\C-m" 'compile)

(setq inhibit-startup-message t
      initial-scratch-message "Caution, this pc has been caught with ilegal content."
      cursor-type 'hollow)
(setq frame-title-format '("" "%b - Living The Dream (•̀ᴗ•́)و"))
(setq display-time-format "%a %b %d ╱ %r") ;; E.g.,:  Fri Mar 04 ╱ 03:42:08 pm
(setq display-time-interval 1) ;; Please update the time every second.
(display-time-mode)
(global-linum-mode 1)
(global-hl-line-mode t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
1  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)
(electric-pair-mode 1)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-use-faces t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background nil
                    :foreground "orange")
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background nil
                    :foreground nil)
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground nil)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(use-package ido-vertical-mode)

(use-package smex
  :bind ("M-x" . smex))

(use-package rainbow-delimiters
  :hook ((org-mode prog-mode text-mode) . rainbow-delimiters-mode))

(use-package web-mode
    :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.tpl\\'" "\\.blade\\.php\\'" "\\.jsp\\'" "\\.as[cp]x\\'"
         "\\.erb\\'" "\\.html.?\\'" "/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'"
         "\\.jinja2?\\'" "\\.mako\\'" "\\.vue\\'" "_template\\.txt" "\\.ftl\\'"))

(use-package emmet-mode
  :hook (web-mode sgml-mode css-mode)
  :bind (:map emmet-mode-keymap
         ("<backtab>" . emmet-expand-line)
         ("\C-c TAB" . emmet-expand-line)
         ("C-M-p" . emmet-prev-edit-point)
         ("C-M-n" . emmet-next-edit-point))
  :config
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-move-cursor-after-expanding t))
