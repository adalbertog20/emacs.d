(setq user-full-name    "Adalberto Garcia"
      user-mail-address "mistergamer37@gmail.com")
(setq default-directory "C:/Users/rusty/Desktop/prog/")
(setq visible-bell 1)
(pending-delete-mode t)
(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :font "JetBrainsMonoNL NFM-10" )
(global-set-key "\C-x\C-m" 'compile)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
		dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      vc-make-backup-files t
      version-control t
      kept-old-versions 0
      kept-new-versions 10
      delete-old-versions t
      backup-by-copying t)

(setq inhibit-startup-message t
      initial-scratch-message "Caution, this pc has been caught with ilegal content."
      cursor-type 'hollow)
(setq frame-title-format '("" "%b - Living The Dream (•̀ᴗ•́)و"))
(display-time-mode)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
;; Initialize package sources
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(electric-pair-mode 1)
(ido-mode 1)
(ido-everywhere 1)
(require 'smex)
(global-set-key (kbd "M-x") 'smex)

(use-package mood-line
  :init (mood-line-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package elcord
  :hook (prog-mode . elcord-mode))

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
(use-package js2-mode
  :hook (javascript-mode . js2-mode))

;;;; Code Completion
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                  ; Allows cycling through candidates
  (corfu-auto t)                   ; Enable auto completion
  (corfu-auto-prefix 2)            ; Enable auto completion
  (corfu-auto-delay 0.0)           ; Enable auto completion
  (corfu-quit-at-boundary t)
  (corfu-echo-documentation 0.25)   ; Enable auto completion
  (corfu-preview-current 'insert)   ; Do not preview current candidate
  (corfu-preselect-first nil)

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("RET"     . nil) ;; leave my enter alone!
              ("TAB"     . corfu-next)
              ([tab]     . corfu-next)
              ("S-TAB"   . corfu-previous)
              ([backtab] . corfu-previous)
              ("S-<return>" . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

;; Add extensions
(use-package cape
  :defer 10
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
				   corfu-quit-no-match t
				   corfu-auto nil)
              (corfu-mode))))

;;; BUFFER MANAGMENT
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . hl-line-mode)
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 40 40 :left :elide)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))
  (setq ibuffer-saved-filter-groups
	'(("home"
           ("Windows" (and (mode . exwm-mode)
                           (not (name . "qutebrowser"))))
           ("Qutebrowser" (name . "qutebrowser"))
           ("Shells" (mode . shell-mode))
           ("emacs-config" (or (filename . ".emacs.d")
                               (filename . "emacs-config")))

           ("Web Dev" (or (mode . html-mode)
                          (mode . css-mode)))
           ("Magit" (name . "\*magit"))
           ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*")))
           ("Browser" (mode . eaf-mode))
           ("Ement" (name . "\*Ement *"))
           ("Org" (or (mode . org-mode)
                      (filename . "OrgMode"))))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "home")))) ; [built-in] Powerful interface for managing buffers

(use-package isearch
  :ensure nil
  :bind (("C-s"     . isearch-forward)
         ("M-s M-%" . isearch-query-replace)
         ("C-r"     . isearch-backward)
         (:map isearch-mode-map
               ("\M-w" . isearch-save-and-exit)))
  :config
  (defun isearch-save-and-exit ()
    "Exit search normally. and save the `search-string' on kill-ring."
    (interactive)
    (isearch-done)
    (isearch-clean-overlays)
    (kill-new isearch-string))

  ;; Avoid typing - and _ during searches
  (setq search-whitespace-regexp "[-_ \t\n]+")

  ;; Place cursor at the start of the match similar to vim's t
  ;; C-g will return the cursor to it's orignal position
  (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
  (defun my-goto-match-beginning ()
    (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
      (goto-char isearch-other-end))))

(use-package yasnippet)
(use-package yasnippet-snippets)
(add-hook 'prog-mode-hook 'yas-global-mode)

(use-package flycheck
  :ensure t
  :hook (prog-mode . global-flycheck-mode))

;; SQL SHIT
(setq sql-mysql-options '("-C" "-f" "-t" "-n")) ; for windows
(add-to-list 'same-window-buffer-names "*SQL*")

(use-package sqlup-mode
  :hook (sql-mode . sqlup-mode))

;;end sql

(use-package ace-jump-mode
  :bind ("C-c t" . ace-jump-mode)
  ("C-c l" . ace-jump-line-mode))

;;DIRED
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package dired-single)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
