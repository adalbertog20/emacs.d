(server-start)
(setq visible-bell 1)
(line-number-mode 1)
(column-number-mode 1)
(setq enable-recursive-minibuffers t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-x C-d") 'dired)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
(setq frame-title-format "%b")
(electric-pair-mode t)
(set-face-attribute 'default nil :font "JetBrainsMono NF-12")
(setq default-directory "C:\\Users\\rusty\\.emacs.d\\")
(delete-selection-mode t)
(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(tooltip-mode -1)
(global-visual-line-mode t)

(defvar after-load-theme-hook nil
  "Hook run after the color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil
      straight-use-package-by-default t)
(straight-use-package 'use-package)

;; PACKAGES
(straight-use-package 'gcmh)
(gcmh-mode 1)

(use-package shackle
  :commands shackle-display-buffer
  :hook (after-init . shackle-mode)
  :config
  (setq shackle-default-size 0.3
        shackle-default-alignment 'below
        shackle-rules
        '((("*Help*" "*Apropos*") :select t :size 0.3 :align 'below :autoclose t)
          (("*compilation*" "*Compile-Log*") :select t :size 0.3 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align 'below)
          (("*Warnings*" "*Messages*") :size 0.3 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          (("*shell*" "*eshell*" "*ielm*") :popup t :align 'below)
          ("*gud-debug*" :select t :size 0.4 :align 'below :autoclose t)
          ("\\*ivy-occur .*\\*" :regexp t :select t :align 'below)
          (" *undo-tree*" :select t)
          ("*quickrun*" :select t :size 15 :align 'below)
          ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
          (("*lsp-help*" "*lsp session*") :size 0.3 :align 'below :autoclose t)
          (("*Paradox Report*" "*package update results*") :size 0.2 :align 'below :autoclose t)
          ("*Package-Lint*" :size 0.4 :align 'below :autoclose t)
          (("*Gofmt Errors*" "*Go Test*") :select t :size 0.3 :align 'below :autoclose t)
          ("*How Do You*" :select t :size 0.5 :align 'below :autoclose t)

          (" *Flycheck checkers*" :select t :size 0.3 :align 'below :autoclose t)
          ((cider-inspector-mode flycheck-error-list-mode point-history-show-mode flymake-diagnostics-buffer-mode) :select t :size 0.3 :align 'below :autoclose t)

          ((grep-mode rg-mode deadgrep-mode ag-mode pt-mode) :select t :align 'below)
          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (helpful-mode :select t :size 0.3 :align 'below :autoclose t)
          ((process-menu-mode cargo-process-mode) :select t :size 0.3 :align 'below :autoclose t))))

(use-package anzu :config (global-anzu-mode))

(use-package solaire-mode
  :config (solaire-global-mode 1)
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (after-load-theme . solaire-mode-swap-bg)))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package focus
  :bind (("H-f" . focus-mode)
         :map change-view-map
         ("f" . focus-mode)))

(use-package olivetti
  :init
  (defvar olivetti--display-linum nil)
  (defvar olivetti--hide-mode-line nil)
  (defvar olivetti-enter-mode-hook nil "Hook run when entering `olivetti-mode'.")
  (defvar olivetti-exit-mode-hook nil "Hook run when exiting `olivetti-mode'.")
  :config
  (advice-add 'olivetti-mode
              :after (lambda (&rest _) (if olivetti-mode
                                           (run-hooks 'olivetti-enter-mode-hook)
                                         (run-hooks 'olivetti-exit-mode-hook))))

  :hook ((olivetti-exit-mode
          . (lambda ()
              (hide-mode-line-mode (or olivetti--hide-mode-line 0))
              (display-line-numbers-mode olivetti--display-linum)))
         (olivetti-enter-mode
          . (lambda ()
              (setq-local olivetti--display-linum display-line-numbers-mode)
              (setq-local olivetti--hide-mode-line hide-mode-line-mode)
              (display-line-numbers-mode 0)
              (hide-mode-line-mode 1))))
  :bind (("H-w" . olivetti-mode)
         :map change-view-map
         ("w" . olivetti-mode)))

(use-package rainbow-delimiters :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode :defer t
  :hook ((prog-mode . rainbow-mode)
         (help-mode . rainbow-mode)))

(use-package beacon
  :after doom-modeline
  :bind (:map change-view-map
              ("v" . beacon-blink)
              ("b" . beacon-mode))
  :config (setq beacon-color (face-background 'doom-modeline-bar)))


(use-package prog-mode
  :straight
  :ensure nil
  :hook ((prog-mode . prettify-symbols-mode)))

(use-package display-line-numbers
  :ensure nil
  :commands (display-line-numbers-scale-linum)
  :hook ((prog-mode . display-line-numbers-mode))
  :config
  (defun display-line-numbers-scale-linum ()
    (set-face-attribute 'line-number nil :height 0.6 :background (face-background 'solaire-default-face))
    (set-face-attribute 'line-number-current-line nil :height 0.6 :background (face-background 'solaire-default-face)))
  (display-line-numbers-scale-linum)
  (setq display-line-numbers-width 3))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))


(use-package lsp-mode
  :disabled t
  :hook (((c-mode
           java-mode) . lsp-deferred))
  :config (setq
	   lsp-keymap-prefix "C-c l"
           lsp-auto-configure t
           lsp-enable-snippet t)
  :commands (lsp))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package kaolin-themes :defer t
  :custom (kaolin-themes-underline-wave nil))

(use-package frame
  :straight
  :after solaire-mode
  :ensure nil
  :hook (after-load-theme
         . (lambda ()
             (set-face-foreground 'window-divider (face-background 'default))
             (set-face-foreground 'vertical-border (face-background 'default))))
  :config
  (set-face-foreground 'window-divider (face-background 'default))
  (set-face-foreground 'vertical-border (face-background 'default))
  (setq window-divider-default-right-width 0
        window-divider-default-bottom-width 0))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-bar-width 5
        doom-modeline-buffer-file-name-style 'truncate-except-project
        doom-modeline-lsp t
        doom-modeline-env-version t
        doom-modeline-indent-info nil
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 20
        doom-modeline-major-mode-color-icon nil)

  :config
  (set-face-attribute 'region nil
                      :foreground (face-background 'doom-modeline-bar)
                      :background (face-background 'default))
  (set-face-attribute 'highlight nil
                      :foreground (face-background 'doom-modeline-bar)
                      :background (face-background 'default))

  (eval-when-compile
    'company
    (doom-modeline-def-segment company-backend
      "Display the current company backend. `company-backend'."
      (when (company--active-p)
        (format "%s"
                (--map (s-replace "company-" "" (format "%s" it))
                       (if (listp company-backend) company-backend (list company-backend)))))))

  (doom-modeline-def-segment
    buffer-info
    "Overwrite of buffer info to not include the icon"
    (concat
     (doom-modeline--buffer-state-icon)
     (doom-modeline--buffer-name)))

  (doom-modeline-def-segment
    buffer-type
    "Buffer icon and version if it exists"
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-mode-icon)
     (when (and doom-modeline-env-version doom-modeline-env--version)
       (propertize
        (format "%s " doom-modeline-env--version)
        'face '(:height 0.7)))))

  (doom-modeline-def-modeline
    'main
    '(bar workspace-name window-number modals matches buffer-type buffer-info remote-host)
    '(company-backend misc-info persp-name battery debug lsp input-method buffer-encoding  process vcs checker)))

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

(use-package all-the-icons
  :if (display-graphic-p))

(defvar ts-default 120 "Default amount for text to be scaled.")
(defvar ts-factor 1.1 "Amount to scale text per step.")

(cl-defmacro deffacescale (face)
  `(progn
     (defun ,(intern (format "%s-ts-increase" face)) ()
       (interactive)
       (set-face-attribute ',face nil :height (truncate (* ts-factor (face-attribute ',face :height nil 'default) ))))

     (defun ,(intern (format "%s-ts-reset" face)) ()
       (interactive)
       (set-face-attribute ',face nil :height ts-default))

     (defun ,(intern (format "%s-ts-decrease" face)) ()
       (interactive)
       (set-face-attribute ',face nil :height (truncate (/ (face-attribute ',face :height nil 'default) ts-factor))))))

(deffacescale default)
(deffacescale solaire-default-face)
(deffacescale variable-pitch)

(bind-keys ("H-0" . solaire-default-face-ts-reset)
           ("H--" . solaire-default-face-ts-decrease)
           ("H-=" . solaire-default-face-ts-increase)
           ("H-_" . default-ts-decrease)
           ("H-+" . default-ts-increase))

(advice-add 'solaire-default-face-ts-increase :after 'display-line-numbers-scale-linum)
(advice-add 'solaire-default-face-ts-decrease :after 'display-line-numbers-scale-linum)
(advice-add 'default-ts-increase :after 'display-line-numbers-scale-linum)
(advice-add 'default-ts-decrease :after 'display-line-numbers-scale-linum)

(display-battery-mode 1)

(fringe-mode 0)
(set-window-fringes nil 0 0)
(setq-default use-file-dialog nil
              use-dialog-box nil
              inhibit-startup-screen t
              inhibit-startup-echo-area-message t

              truncate-lines t

              ns-use-thin-smoothing t
              ns-antialias-text t
              ns-pop-up-frames nil)


;; COMPLETION
(use-package ido-completing-read+
  :init (ido-ubiquitous-mode 1))

(use-package amx
  :init
  (setq amx-history-length 20
        amx-show-key-bindings t))

(use-package counsel
  :after ivy
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("s-V"     . counsel-yank-pop))
  :config
  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n----------\n"
        counsel-describe-function-function 'helpful-callable
        counsel-describe-variable-function 'helpful-variable))

(use-package counsel-projectile
  :after (projectile counsel)
  :commands counsel-projectile-switch-to-buffer
  :bind (("C-x C-b" . counsel-projectile-switch-to-buffer)
         ("C-o"     . counsel-projectile-find-file)
         ("M-s"     . counsel-projectile-rg)))

(use-package company-emoji
  :bind ("C-M-:" . company-emoji))

(use-package swiper
  :bind (("C-;" . swiper-thing-at-point)
         ("C-:" . swiper-all-thing-at-point)))

(use-package ivy-hydra :after ivy)
(use-package ivy
  :defer 1
  :defines (ivy-set-font)
  :config
  (ivy-mode 1)
  (setq ivy-height 20
        ivy-count-format "(%d/%d) ")
  (defun ivy-set-font (&optional face)
    (interactive)
    (let* ((re "-\\*-\\([a-z0-9/ ]+\\)-\\([a-z]+\\).*" )
           (candidates (--map
                        (cons (nth 2 (s-split "-" it)) it)
                        (-uniq (--filter (let ((split (s-split "-" it)))
                                           (and (string= "m" (nth 11 split))
                                                (string= "normal" (nth 3 split))
                                                (string= "normal" (nth 4 split))))
                                         (x-list-fonts "*" nil (selected-frame)))))))
      (ivy-read "Font: " candidates
                :action (lambda (x) (funcall 'set-face-font
                                             (or face 'default)
                                             (cdr x)
                                             (selected-frame))))))

  (defun ivy-set-face-font ()
    (interactive)
    (let ((face (read-face-name "Face: " (face-at-point t))))
      (ivy-set-font face)))

  :bind (:map ivy-minibuffer-map
              ("s-s"         . (lambda () (interactive) (ivy-quit-and-run (rg-project ivy-text "*"))))
              ("s-f"         . (lambda () (interactive) (ivy-quit-and-run (counsel-rg))))
              ("s-o"         . (lambda () (interactive) (ivy-quit-and-run (counsel-projectile))))
              ("s-p"         . (lambda () (interactive) (ivy-quit-and-run (counsel-projectile-switch-project))))
              ("<backspace>" . delete-backward-char)
              ("C-d"         . delete-forward-char)))

(use-package ivy-prescient
  :init (ivy-prescient-mode 1))

(use-package ivy-yasnippet
  :after (ivy yasnippet)
  :bind ("C-\\" . ivy-yasnippet))

(use-package ivy-posframe
  :if window-system
  :hook ((minibuffer-setup ivy-rich-mode) . (lambda () (set-window-fringes nil 0 0)))
  :after ivy
  :config
  (setq ivy-posframe-border-width 30
        ivy-posframe-min-width 80
        ivy-posframe-width 120
        ivy-posframe-min-height 20
        ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (bind-keys :map ivy-minibuffer-map
             ("C-'"        . ivy-posframe-avy)
             ("<C-return>" . ivy-read-action))
  (ivy-posframe-mode))

(use-package ivy-rich
  :after ivy
  :config (ivy-rich-mode))

(use-package which-key
  :disabled
  :config
  (which-key-setup-side-window-right)
  (which-key-mode))

(use-package company
  :config
  (setq company-show-numbers t
        company-tooltip-limit 15
        company-tooltip-align-annotations t
        company-idle-delay 0.2
        company-minimum-prefix-length 3
        company-require-match 'never)
  :hook (after-init    . global-company-mode)
  :bind (("<kp-enter>" . company-complete)
         ("<M-tab>"    . company-begin-backend)
         ("M-/"        . company-complete)
         :map company-active-map
         ("<tab>"      . company-complete-common-or-cycle)
         ("<backtab>"  . company-select-previous)
         ("C-n"        . company-select-next)
         ("C-p"        . company-select-previous)))

(use-package company-racer
  :config (add-to-list 'company-backends 'company-racer))

(use-package company-tabnine
  :after company
  :custom (company-tabnine-max-num-results 9)
  :bind (:map company-active-map
              ("M-q" . company-other-backend))
  :config
  (add-to-list 'company-backends 'company-tabnine)
  (setq company-tabnine-always-trigger t))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :bind (:map company-active-map
              ("M-h" . company-box-doc-manually))
  :config
  (defun company-box--common-make-line (candidate)
    (-let* (((candidate annotation len-c len-a backend) candidate)
            (candidate-i (--find-index (s-equals-p candidate it) company-candidates))
            (candidate-num (format "%s " (if (< candidate-i 9) (1+ candidate-i) " ")))
            (candidate-kind (company-box--get-kind candidate))
            (annotation-str (format "%s [%s]" (or annotation "") candidate-kind))
            (annotation-len (length annotation-str))
            (color (company-box--get-color backend))
            ((c-color a-color i-color s-color) (company-box--resolve-colors color))
            (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
            (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                      (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
            (align-string (concat " " (and company-tooltip-align-annotations
                                           (propertize " " 'display `(space :align-to (- right-fringe ,(or annotation-len 0) 1))))))
            (space company-box--space)
            (icon-p company-box-enable-icon)
            (annotation-string (propertize annotation-str 'face 'company-box-annotation))
            (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                            (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                          (company-box--apply-color icon-string i-color)
                          (company-box--apply-color candidate-num i-color)
                          (company-box--apply-color candidate-string c-color)
                          align-string
                          (company-box--apply-color annotation-string a-color)))
            (len (length line)))
      (add-text-properties 0 len (list 'company-box--len (+ 2 len-c annotation-len)
                                       'company-box--color s-color)
                           line)
      line))
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-icons-all-the-icons
        `((Unknown       . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
          (Text          . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
          (Method        . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05))
          (Function      . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05))
          (Constructor   . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05))
          (Field         . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0))
          (Variable      . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0))
          (Class         . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2))
          (Interface     . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2))
          (Module        . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2))
          (Property      . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
          (Unit          . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
          (Value         . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2))
          (Enum          . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2))
          (Keyword       . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
          (Snippet       . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
          (Color         . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
          (File          . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
          (Reference     . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
          (Folder        . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
          (EnumMember    . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2))
          (Constant      . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
          (Struct        . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2))
          (Event         . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0))
          (Operator      . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
          (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
          (Template      . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
        )

  (advice-add 'company-box--make-line :override 'company-box--common-make-line)
  )

;;NAVIGATION
(use-package uniquify
  :straight
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package recentf
  :straight
  :ensure nil
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300))

(use-package treemacs
  :disabled
  :bind (("C-c t" . treemacs-select-window)
         ("s-)" . treemacs-kill-buffer)
         ("<mouse-1>" . treemacs-RET-action))
  :hook (treemacs-mode
         . (lambda ()
             (face-remap-add-relative 'hl-line :background (face-background 'default))
             (face-remap-add-relative 'fringe :background (face-background 'default))))
  :config
  (advice-add 'doom-themes-hide-fringes :after (lambda () (set-window-fringes nil 6 0)))
  (with-no-warnings
    (treemacs-follow-mode)
    (treemacs-filewatch-mode)
    (treemacs-fringe-indicator-mode)
    (treemacs-git-mode 'simple)
    (setq treemacs-is-never-other-window t
          treemacs-position 'left)))


(use-package dired
  :straight
  :ensure nil
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (insert-directory-program "gls")
  (dired-listing-switches "-alh --group-directories-first")

  :bind (:map dired-mode-map
              ("W" . wdired-change-to-wdired-mode)))

(use-package dired-quick-sort
  :after dired
  :bind (:map dired-mode-map
              ("S" . dired-quick-sort)))

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil
        undo-tree-history-directory-alist
        `(("."      . ,(locate-user-emacs-file "var/undo-tree-hist/"))))
  :bind (("C-c C-u" . undo-tree-visualize)
         ("C-_"     . undo-tree-undo)
         ("C-+"     . undo-tree-redo)))


(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :commands (flycheck-list-errors
             flycheck-previous-error
             flycheck-next-error)
  :bind (("M-}" . flycheck-mode))
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit
                flycheck-check-syntax-automatically '(save mode-enabled)
                flycheck-disabled-checkers '(javascript-jshint scss))
  (set-face-background 'flycheck-fringe-warning (face-foreground 'flycheck-fringe-warning))
  (set-face-background 'flycheck-fringe-error (face-foreground 'flycheck-fringe-error))
  (bind-keys :prefix "C-c e"
             :prefix-map flycheck-keymap-prefix
             ("l" . flycheck-list-errors)
             ("p" . flycheck-previous-error)
             ("n" . flycheck-next-error))

  (defun flycheck-debug-eslint ()
    "Run the checker config to see what the error is."
    (interactive)
    (let* ((executable (flycheck-find-checker-executable 'javascript-eslint))
           (cmd (format "%s --print-config %s" executable (or buffer-file-name
                                                              "index.js"))))
      (message "%s" (shell-command-to-string cmd))))

  (with-eval-after-load 'js2-mode
    (setq flycheck-javascript-eslint-executable "eslint_d")))


(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (prog-mode . lsp-ui-mode)
  :bind (("C-." . lsp-ui-imenu)
         :map lsp-ui-imenu-mode-map
         ("n"   . next-line)
         ("p"   . previous-line)
         ("M-n" . lsp-ui-imenu--next-kind)
         ("M-p" . lsp-ui-imenu--prev-kind))
  :config
  (defvar lsp-ui-non-flycheck-modes
    '(rjsx-mode js2-mode rust-mode rustic-mode))
  (setq lsp-prefer-flymake nil)

  (advice-add 'lsp-ui-flycheck-enable :after
              '(lambda (&rest args)
                 (when (memq major-mode lsp-ui-non-flycheck-modes)
                   (flycheck-disable-checker 'lsp-ui)
                   (setq-local flycheck-checker nil)))))


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2d2683f3aa3b111ca0c7fb8ae48add4c5c37c7e99c322228430fde045c16e014" "6e06d003a3544721ee830a7b0140b548e1445eac3a088be929a0ceb5895fb224" default))
 '(warning-suppress-types '((use-package) (use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
