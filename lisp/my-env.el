;;; package -- Summary

;;; Commentary:

;;; Code:

(use-package recentf
  :defer t
  :init
  (setq recentf-save-file (expand-file-name "recentf" my-tmp-dir)
	recentf-max-menu-items 50
	recentf-max-saved-items 200)
  :config
  (add-to-list 'recentf-exclude (expand-file-name ".*" my-tmp-dir))
  (add-to-list 'recentf-exclude (expand-file-name "elpa/.*" my-emacs-dir))
  (add-to-list 'recentf-exclude (expand-file-name "~/.type-break"))
  (add-to-list 'recentf-exclude (expand-file-name "~/#.type-break#"))
  (recentf-mode +1))

(use-package session
  :init
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist nil)
                                  (file-name-history 10000)))
  (setq session-globals-max-string 100000000)
  (setq history-length t)
  (setq session-undo-check -1)
  (setq session-save-file (expand-file-name "session" my-tmp-dir))
  (setq session-store-buffer-places (expand-file-name "places" my-tmp-dir))
  :config
  (add-hook 'after-init-hook 'session-initialize))

(use-package counsel
  :diminish counsel-mode
  :defer t
  :bind
  (("C-x C-d" . counsel-dired-jump)
   ("C-x C-h" . counsel-minibuffer-history)
   ("C-x C-l" . counsel-find-library)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-x C-u" . counsel-unicode-char)
   ("C-x C-v" . counsel-set-variable))
  :config
  (counsel-mode))

(use-package ivy
  :defer t
  :diminish ivy-mode
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)
   ("M-H" . ivy-resume)
   :map ivy-minibuffer-map
   ("C-i" . ivy-partial-or-done)
   :map ivy-switch-buffer-map
   ("C-k" . ivy-switch-buffer-kill))
  :after counsel
  :init
  (setq ivy-use-virtual-buffers t
        ivy-display-style 'fancy
        ivy-case-fold-search-default t
        ivy-count-format "(%d/%d) "
        ivy-re-builders-alist '((t . ivy--regex-plus)))
  :config
  (ivy-mode))

(use-package smex
  :defer t
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :init
  (setq smex-save-file (expand-file-name "smex-items" my-tmp-dir))
  :config
  (smex-initialize))

(use-package avy
  :defer t
  :bind
  ("C-." . avy-goto-char)
  ("M-s" . avy-goto-word-1)
  :init
  :config
  (avy-setup-default))

(use-package hl-line
  :defer t
  :config
  (set-face-background 'hl-line "#3e4446")
  (set-face-background 'highlight nil)
  (global-hl-line-mode +1))

(use-package expand-region
  :defer t
  :bind
  ("C-=" . er/expand-region))

(use-package which-key
  :diminish which-key-mode
  :defer 3
  :commands
  which-key-mode
  :init
  (setq which-key-idle-delay 3.0)
  :config
  (which-key-mode +1))

(use-package rainbow-mode
  :diminish rainbow-mode
  :defer t
  :preface
  (defun enable-rainbow-mode ()
    (when (string-match "\\(\\.emacs\\|color-theme-\\|-theme\\|init\\.el\\)" (buffer-name))
      (rainbow-mode +1)))
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-rainbow-mode)
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook #'rainbow-mode)))

(use-package swiper
  :ensure t
  :defer t
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   :map swiper-map
   ("M-%" . swiper-query-replace))
  :defer 5
  :config
  (define-key swiper-map (kbd "C-.")
    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))
  (define-key swiper-map (kbd "M-.")
    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word))))))
  :init
  (setq swiper-stay-on-quit t)
  ;;advise swiper to recenter on exit
  (defun swiper-recenter (&rest args)
    "recenter display after swiper"
    (recenter))
  (defadvice swiper-recenter (after swiper)))

(use-package beacon
  :defer t
  :diminish beacon-mode
  :init
  (setq beacon-push-mark 35
	beacon-color "#cccc00"
	beacon-blink-when-point-moves-vertically nil ; default nil
	beacon-blink-when-point-moves-horizontally nil ; default nil
	beacon-blink-when-buffer-changes t ; default t
	beacon-blink-when-window-scrolls t ; default t
	beacon-blink-when-window-changes t ; default t
	beacon-blink-when-focused nil ; default nil

	beacon-blink-duration 0.3 ; default 0.3
	beacon-blink-delay 0.3 ; default 0.3
	beacon-size 20 ; default 40
	;; (setq beacon-color "yellow") ; default 0.5
	beacon-color 0.5) ; default 0.5
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'term-mode)
  (beacon-mode +1))

;; flyspell
(use-package flyspell
  :diminish flyspell-mode
  :defer t
  :init
  (flyspell-mode))

;; spell check
(use-package flyspell-correct-ivy
  :ensure t
  :defer t
  :after (flyspell ivy)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  (add-hook 'text-mode-hook
            (lambda()
              (flyspell-mode)
              (flycheck-mode)))
  (add-hook 'org-mode-hook
            (lambda()
              (flyspell-mode +1)
              (flycheck-mode +1)))
  :config
  (with-eval-after-load "flyspell"
    '(add-hook flyspell-mode-hook #'flyspell-correct-auto-mode))
  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-previous-word-generic))

;;; Auto complete (company -> COMPlete ANYthing)
(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :commands company-mode
  :init
  (setq company-begin-commands '(self-insert-command)
        company-dabbrev-ignore-case nil
        company-dabbrev-code-ignore-case nil
        company-dabbrev-downcase nil
        company-idle-delay 1
        company-show-numbers t
        company-tooltip-align-annotations 't
        company-minimum-prefix-length 2)
  :config
  (global-company-mode)
  (global-set-key (kbd "C-c C-\t") 'company-complete-common)
  (global-set-key (kbd "C-c C-n") 'company-select-next)
  (global-set-key (kbd "C-c C-p") 'company-select-previous)
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
    (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p) ad-do-it)))

;; savehist
(use-package savehist
  :defer t
  :init
  (setq savehist-file (expand-file-name "savehist" my-tmp-dir)
        history-length 10000
        history-delete-duplicates t
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
        savehist-save-minibuffer-history 1)
  :config
  (savehist-mode +1))

;;; Smart mode line
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/name-width 30)
  :config
  (sml/setup)
  (sml/apply-theme 'respectful))

;; ibuffer
(use-package ibuffer
  :defer t
  :config
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (autoload 'ibuffer "ibuffer" "List buffers." t))

;; ibuffer-projectile
(use-package ibuffer-projectile
  :defer t
  :after ibuffer
  :preface
  (defun my/ibuffer-projectile ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook (ibuffer . my/ibuffer-projectile))

;; write good
(use-package writegood-mode
  :defer t
  :diminish writegood-mode
  :config
  (writegood-mode))

;; nyan-mode
(use-package nyan-mode
 :defer 10
 :ensure t
 :config
 (nyan-mode))

;; xkcd
(use-package xkcd
  :defer t
  :bind
  (("C-c x r" . xkcd-rand)
   ("C-c x g" . xkcd-get-latest)
   ("C-c x G" . xkcd-get)
   ("C-c x c" . xkcd-get-latest-cached)
   ("C-c x p" . xkcd-prev)
   ("C-c x n" . xkcd-next)))

;; popup-imenu
(use-package popup-imenu
  :defer t
  :commands popup-imenu
  :bind
  ("M-i" . popup-imenu))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :defer t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; discover
(use-package discover
  :defer t
  :config
  (global-discover-mode +1))

(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always))

;; dashboard
(use-package dashboard
  :preface
  (defun my/dashboard-banner ()
    "Set a dashboard banner including information on package initialization
     time and garbage collections."
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %.2f seconds with %d garbage collections."
                  (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
  :init
  ;; (setq dashboard-startup-banner nil)
  (setq dashboard-items '((agenda . 5)
                          (projects . 5)
                          (recents  . 5)
                          (bookmarks . 5))
        dashboard-set-navigator t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-show-shortcuts nil
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
  :custom
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))

;; emojis :+1:
(use-package emojify
  :defer t
  :config
  (global-emojify-mode))

(use-package all-the-icons
  :defer t)

(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(use-package paradox
  :defer t
  :custom
  (paradox-column-width-package 27)
  (paradox-column-width-version 13)
  (paradox-execute-asynchronously t)
  (paradox-hide-wiki-packages t)
  :init
  (setq paradox-github-token "15320b3eed96db050a2e3d4e787d822b62ca49a2")
  :config
  (paradox-enable)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

(use-package window
  :ensure nil
  :bind (("C-x 3" . hsplit-last-buffer)
         ("C-x 2" . vsplit-last-buffer))
  :preface
  (defun hsplit-last-buffer ()
    "Gives the focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1)
    (switch-to-next-buffer))

  (defun vsplit-last-buffer ()
    "Gives the focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (switch-to-next-buffer)))

;; multiterm
(use-package multi-term
  :defer t
  :config
  (add-hook 'term-mode-hook
            (lambda()
              ;; (global-unset-key (kbd "C-r"))
	      (local-unset-key (kbd "C-r"))
              (message "%s" "This is in term mode and hook enabled.")))
  :init
  (setq multi-term-program "/bin/zsh")
  (setq multi-term-dedicated-close-back-to-open-buffer-p t))

;; osx-location
(use-package osx-location
  :defer t)

;; ag
(use-package ag
  :defer t
  :init
  (setq ag-highlight-search t
        ag-reuse-window 't))

;; tramp
(use-package tramp
  :defer t)

(provide 'my-env)
;;; my-env.el ends here
