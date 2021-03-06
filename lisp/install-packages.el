;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'package)

(defvar my-packages
  '(
    ag
    all-the-icons
    anaconda-mode
    async
    auto-compile
    avy
    beacon
    better-defaults
    bind-key
    company
    company-anaconda
    company-emoji
    company-go
    company-terraform
    counsel
    counsel-projectile
    dabbrev
    dashboard
    diff-hl
    diminish
    discover
    dotenv-mode
    dumb-jump
    dockerfile-mode
    elpy
    emojify
    engine-mode
    exec-path-from-shell
    expand-region
    flycheck
    flycheck-pycheckers
    flymake-python-pyflakes
    flyspell
    flyspell-correct-ivy
    focus
    function-args
    git-commit
    go-add-tags
    go-eldoc
    golint
    ;; go-flycheck
    ;; go-flymake
    go-guru
    go-mode
    goto-last-change
    ;; groovy
    hl-line
    hl-todo
    ibuffer
    ibuffer-projectile
    ivy
    js3-mode
    json-mode
    magit
    ;; magithub ; commented out on Feb 26, 2021
    markdown-mode
    multi-term
    mustache-mode
    notmuch
    nyan-mode
    offlineimap
    org
    org-bullets
    org-jira
    osx-location
    paradox
    projectile
    protobuf-mode
    py-yapf
    python
    rainbow-delimiters
    rainbow-mode
    recentf
    savehist
    saveplace
    session
    smart-mode-line
    smartparens
    smex
    subword
    swiper
    ;; terraform
    tickscript-mode
    type-break
    use-package
    volatile-highlights
    web-mode
    which-key
    whitespace
    whitespace-cleanup-mode
    win-switch
    writegood-mode
    ;; xcscope  ; commented out on Feb 26, 2021
    xkcd
    yaml-mode
    ))

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

;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'install-packages)
;;; install-packages.el ends here
