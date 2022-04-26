;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq frame-title-format "%b")
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key [f9] 'code-compile)
(electric-pair-mode t)
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(setq inhibit-splash-screen nil)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'c-mode-hook #'tree-sitter-hl-mode)
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(set-face-attribute 'default nil :font "Iosevka Nerd Font-14")
(setq inhibit-startup-message nil)
(setq-default tab-width 4)
(setq tab-width 4)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq backup-directory-alist '(("." . "D:/backuptempemacs")))
(when (file-exists-p custom-file)
  (load custom-file))
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(delete-selection-mode t)
(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)
(tool-bar-mode -1)                   
(menu-bar-mode -1)                   
(scroll-bar-mode -1)                 
(set-fringe-mode 10)                 
(tooltip-mode -1)                    
(global-visual-line-mode t)          

(defalias 'yes-or-no-p 'y-or-n-p)
(setq default-directory "C:\\Users\\rusty\\.emacs.d\\")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
(setq scroll-step 1
      scroll-conservatively  10000)
(add-hook 'comint-mode-hook #'(lambda () (setq-local show-trailing-whitespace nil)))

(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

(defun code-compile ()
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
     (let ((file (file-name-nondirectory buffer-file-name)))
       (format "%s -o %s %s"
           (if  (equal (file-name-extension file) "cpp") "g++" "gcc" )
           (file-name-sans-extension file)
           file)))
    (compile compile-command)))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

(use-package gcmh
  :ensure t
  :demand t
  :config
  (gcmh-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :defer t)

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use home row for selecting.")
  (aw-scope 'frame "Highlight only current frame.")
  :bind
  ("C-x o" . ace-window))

(use-package ace-jump-mode
  :bind
  ("C-x c" . ace-jump-char-mode)
  ("C-x w" . ace-jump-word-mode)
  ("C-x l" . ace-jump-line-mode)
  :ensure t)

(use-package elcord
  :ensure t
  :commands (elcord-mode))

(use-package highlight-indent-guides
  :ensure t)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(use-package smex
  :ensure t
  :bind
  ("M-x" . smex))

(use-package neotree
  :ensure t
  :bind
  ("C-c o t" . neotree))

(use-package ido
  :config (ido-mode 1)
  (setq ido-everywhere t))

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode-hook . highlight-numbers-mode))

(use-package auto-complete
  :ensure t
  :hook (prog-mode-hook . auto-complete-mode))

(use-package org
  :ensure t
  :hook (org-mode-hook . org-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))


(use-package keycast
  :ensure t
  :init (keycast-mode))

(use-package deadgrep
  :ensure t
  :bind
  ("<f5>" . deadgrep))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package seoul256-theme
  :ensure t
  :config
  (load-theme 'seoul256 t))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(use-package ibuffer
  :ensure t
  :bind
  ("C-x C-b" . ibuffer))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
