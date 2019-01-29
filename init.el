;; 2019

(defconst my-start-time (current-time))

(setq user-full-name "Chirag Kantharia"
      user-login-name "chirag"
      user-mail-host "zededa.com"
      user-mail-address "chirag@zededa.com")

(defvar my-home-dir (concat (getenv "HOME") "/"))
(defvar my-emacs-dir (concat my-home-dir ".emacs.d/"))
(defvar my-tmp-dir (concat my-emacs-dir "tmp"))

(add-to-list 'load-path "/usr/local/share/emacs/27.0.50/lisp")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path (concat my-emacs-dir "lisp"))
(add-to-list 'load-path (concat my-emacs-dir "pkgs"))
(add-to-list 'load-path (concat my-emacs-dir "elpa"))
(let ((default-directory (concat my-emacs-dir "elpa")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'install-packages)
(require 'sensible-defaults)

(setq custom-theme-directory
      (concat my-emacs-dir "themes"))
(load-theme 'black t)

(require 'my-env)
(require 'my-keybindings)
(require 'my-utils)
(require 'my-devel)
(require 'my-org)
(require 'my-email-setup)

;;; Elapsed time
(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       my-start-time))))
               (message "Loading %s...done (%.3fs)", load-file-name elapsed))
             (org-agenda nil "a")) t)


;; /end
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode xkcd xcscope writegood-mode win-switch whitespace-cleanup-mode which-key web-mode volatile-highlights use-package tickscript-mode smex smartparens smart-mode-line session rainbow-mode rainbow-delimiters python-mode py-yapf protobuf-mode projectile org-bullets offlineimap nyan-mode notmuch magithub magit-gh-pulls json-mode js3-mode hl-todo goto-last-change go-guru go-eldoc go-add-tags function-args focus flyspell-correct-ivy flymake-python-pyflakes flycheck expand-region exec-path-from-shell eink-theme dockerfile-mode diminish diff-hl darktooth-theme counsel company-go better-defaults beacon avy auto-compile anaconda-mode alect-themes ag)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
