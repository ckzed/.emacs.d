;;; package -- Summary

;;; Commentary:

;;; Code:

(use-package recentf
  :defer 5
  :init
  (setq recentf-save-file (expand-file-name "recentf" my-tmp-dir))
  (setq recentf-max-menu-items 50)
  :config
  (add-to-list 'recentf-exclude (expand-file-name ".*" my-tmp-dir))
  (add-to-list 'recentf-exclude (expand-file-name ".*" org-directory))
  (add-to-list 'recentf-exclude (expand-file-name "elpa/.*" my-emacs-dir))
  (add-to-list 'recentf-exclude (expand-file-name "~/.type-break"))
  (add-to-list 'recentf-exclude (expand-file-name "~/#.type-break#"))
  (recentf-mode +1))

(use-package session
  :defer 5
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

(use-package counsel)
(use-package ivy
  :defer 5
  :diminish ivy-mode
  :after counsel
  :init
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf))
  ;; (global-set-key (kbd "C-x C-r") 'ivy-recentf))

(use-package smex
  :defer 5
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :init
  (setq smex-save-file (expand-file-name "smex-items" my-tmp-dir))
  :config
  (smex-initialize))

(use-package avy
  :defer 5
  :bind
  ("C-." . avy-goto-char)
  ("M-s" . avy-goto-word-1)
  :init
  :config
  (avy-setup-default))

;; Draw a tail in the trail of typed characters; disabled as of now;
;; to enable it, put the following in the :init section
;;  (highlight-tail-mode)
;; (use-package highlight-tail
;;   :diminish highlight-tail-mode
;;   :config
;;   (setq highlight-tail-steps 14
;; 	highlight-tail-timer 1
;; 	highlight-colors '(("black" . 0 )
;;                            ("#bc2525" . 25)
;;                            ("black" . 66))
;;         highlight-tail-posterior-type 'const))

(use-package hl-line
  :defer 5
  :config
  (set-face-background 'hl-line "#3e4446")
  (set-face-background 'highlight nil)
  (global-hl-line-mode +1))

(use-package win-switch
  :defer 5
  :bind
  ("C-x o" . win-switch-dispatch)
  :config
  (setq win-switch-feedback-background-color "#6c96af")
  (setq win-switch-feedback-foreground-color "#000000")
  (setq win-switch-window-threshold 1)
  (setq win-switch-idle-time 0.7)
  (win-switch-set-keys '("o") 'next-window)
  (win-switch-set-keys '("p") 'previous-window))

(use-package expand-region
  :defer 5
  :bind
  ("C-=" . er/expand-region))

(use-package which-key
  :diminish which-key-mode
  :defer 5
  :commands
  which-key-mode
  :init
  (setq which-key-idle-delay 3.0)
  :config
  (which-key-mode +1))

;; rainbow-mode to display colors as the background of the hex code
(use-package rainbow-mode
  :diminish rainbow-mode
  :defer 5
  :preface
  (defun enable-rainbow-mode ()
    (when (string-match "\\(\\.emacs\\|color-theme-\\|-theme\\|init\\.el\\)" (buffer-name))
      (rainbow-mode +1)))
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-rainbow-mode)
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook #'rainbow-mode)))

;; swiper
(use-package swiper
  :ensure t
  :defer 5
  :config
  (global-set-key (kbd "C-s") 'swiper)
  (define-key swiper-map (kbd "C-.")
    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))
  (define-key swiper-map (kbd "M-.")
    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word))))))
  :init
  (setq ivy-display-style 'fancy)
  ;;advise swiper to recenter on exit
  (defun swiper-recenter (&rest args)
    "recenter display after swiper"
    (recenter))
  (defadvice swiper-recenter (after swiper)))

;; beacon
(use-package beacon
  :defer 5
  :diminish beacon-mode
  :config
  (setq beacon-push-mark 35)
  ;; (setq beacon-color "#666600")
  (setq beacon-color "#cccc00")
  :init
  (beacon-mode))

;; flyspell
(use-package flyspell
  :diminish flyspell-mode
  :defer 5
  :init
  (flyspell-mode))

;; spell check
(use-package flyspell-correct-ivy
  :ensure t
  :defer 5
  :after flyspell
  :init
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
  :defer 5
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
    (when (company-explicit-action-p) ad-do-it))
  ;; company colors with dark background
;;  (require 'color)
;;  (let ((bg (face-attribute 'default :background)))
;;    (custom-set-faces
;;     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
  )

;; (use-package company-quickhelp
;;  :after company
;;  :init
;;  (setq company-quickhelp-
;;  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

;; savehist
(use-package savehist
  :defer 5
  :init
  (setq savehist-file (expand-file-name "savehist" my-tmp-dir)
        history-length 10000
        history-delete-duplicates t)
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

;; Take a break
(use-package type-break
  :defer 5
  :bind
  ("<f12>" . type-break)
  :config
  (type-break-mode))

;; Screens
;; (use-package escreen
;;  :config
;;  (escreen-install)
;;  (escreen-number-mode))

;; ibuffer
(use-package ibuffer
  :defer 5
  :config
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (autoload 'ibuffer "ibuffer" "List buffers." t))

;; ibuffer-projectile
(use-package ibuffer-projectile
  :defer 5
  :after ibuffer
  :preface
  (defun my/ibuffer-projectile ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook (ibuffer . my/ibuffer-projectile))

;; write good
(use-package writegood-mode
  :defer 5
  :diminish writegood-mode
  :config
  (writegood-mode))

;; nyan-mode
(use-package nyan-mode
  :defer 5
  :ensure t
  :config
  (nyan-mode))

;; xkcd
(use-package xkcd
  :defer 5
  :config
  (global-set-key (kbd "C-c C-r") 'xkcd-rand))

;; popup-imenu
(use-package popup-imenu
  :defer 5
  :commands popup-imenu
  :bind
  ("M-i" . popup-imenu))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :defer 5
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; discover
(use-package discover
  :defer 5
  :config
  (global-discover-mode +1))

(use-package dired
  :ensure nil
  :defer 5
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always))

;; engine
(use-package engine-mode
  :defer 5
  :init
  (setq engine/browser-function 'eww-browse-url)
  :config
  (engine/set-keymap-prefix (kbd "C-c s"))
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")
  (engine-mode t))

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
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
  :custom
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))

;; emojis :+1:
(use-package emojify
  :defer 10
  :config
  (global-emojify-mode))

(provide 'my-env)
;;; my-env.el ends here
