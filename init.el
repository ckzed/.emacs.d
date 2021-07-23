;;; package -- Summary
;;; Commentary:
;; 2019

;;; Code:
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
(add-to-list 'load-path (concat my-emacs-dir "lisp"))
(add-to-list 'load-path (concat my-emacs-dir "pkgs"))
(add-to-list 'load-path (concat my-emacs-dir "elpa"))
(let ((default-directory (concat my-emacs-dir "elpa")))
  (normal-top-level-add-subdirs-to-load-path))

(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
	("org" . "http://orgmode.org/elpa/")))

(setq package-pinned-packages
      '(
	;; "unstable" package
	(flycheck           . "melpa")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package use-package-ensure-system-package :ensure t)

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


(provide 'init)
;;; init.el ends here
