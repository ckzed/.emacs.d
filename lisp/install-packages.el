(require 'package)

(defvar my-packages
  '(
    ag
    anaconda-mode
    auto-compile
    avy
    beacon
    better-defaults
    bind-key
    company
    company-go
    counsel
    dabbrev
    diff-hl
    diminish
    dockerfile-mode
    exec-path-from-shell
    expand-region
    flycheck
    flymake-python-pyflakes
    flyspell
    flyspell-correct-ivy
    focus
    function-args
    go-add-tags
    go-eldoc
    ;; go-flycheck
    ;; go-flymake
    go-guru
    go-mode
    goto-last-change
    hl-line
    hl-todo
    ibuffer
    ivy
    js3-mode
    json-mode
    magit
    magithub
    magit-gh-pulls
    markdown-mode
    notmuch
    nyan-mode
    offlineimap
    org
    org-bullets
    projectile
    protobuf-mode
    py-yapf
    python
    python-mode
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
    xcscope
    xkcd
    yaml-mode
    ))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://stable.melpa.org/packages/")
	;; ("marmalade" . "https://marmalade-repo.org/packages/")
	("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'install-packages)
