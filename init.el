;; 2019

(defconst my-start-time (current-time))

(setq user-full-name "Chirag Kantharia"
      user-login-name "chirag"
      user-mail-host "zededa.com"
      user-mail-address "chirag@zededa.com")

(eval-after-load 'osx-location
  '(when (eq system-type 'darwin)
     (add-hook 'osx-location-changed-hook
               (lambda ()
                 (setq calendar-latitude osx-location-latitude
                       calendar-longitude osx-location-longitude
                       calendar-location-name (format "%s, %s" osx-location-latitude osx-location-longitude))))))

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
    (flycheck-kotlin zone-nyan yaml-mode xkcd xcscope writegood-mode win-switch whitespace-cleanup-mode which-key web-mode volatile-highlights use-package-ensure-system-package tickscript-mode switch-window smex smartparens smart-mode-line slime-company session rainbow-mode rainbow-delimiters py-yapf py-autopep8 protobuf-mode popup-imenu paradox osx-location org-gcal org-bullets org-alert offlineimap nyan-mode nov notmuch mustache-mode multi-term magithub magit-gh-pulls json-mode js3-mode idle-highlight-mode ibuffer-projectile hl-todo highlight-symbol goto-last-change go-guru go-eldoc go-add-tags git function-args focus flyspell-correct-ivy flymake-python-pyflakes flymake-json flycheck-color-mode-line expand-region exec-path-from-shell engine-mode emojify elscreen elpy dockerfile-mode discover diminish diff-hl delight dashboard counsel-projectile company-terraform company-go company-emoji company-anaconda better-defaults beacon avy auto-compile all-the-icons ag)))
 '(paradox-automatically-star t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
