(use-package xcscope
  :config
  (setq cscope-option-do-not-update-database t)
  (setq cscope-close-window-after-select t)
  (setq cscope-option-use-inverted-index t)
  :init
  (cscope-setup))

;; highlight uncommitted changes
(use-package diff-hl
  :ensure t
  :bind
  ("C-c r" . diff-hl-revert-hunk)
  :bind
  ("C-c n" . diff-hl-next-hunk)
  :commands global-diff-hl-mode
  :init
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; highlight todos
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; make the whitespace standout
(use-package whitespace
  :init
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  :diminish whitespace-mode)

;;;; make the whitespace cleanup as default
(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode)
  :diminish whitespace-cleanup-mode)

;;(use-package subword
;;  :diminish subword-mode
;;  :config
;;  (global-subword-mode +1))

;; highlight the changes to the buffer
(use-package volatile-highlights
  :diminish volatile-highlights-mode)

;;;; Draw a tail in the trail of typed characters
;;(use-package highlight-tail
;;  :disabled t
;;  :diminish highlight-tail-mode
;;  :init
;;  (setq highlight-tail-steps 14
;;	highlight-tail-timer 1
;;	highlight-colors '(("black" . 0 )
;;                           ("#bc2525" . 25)
;;                           ("black" . 66))
;;        highlight-tail-posterior-type 'const)
;;  :config
;;  (highlight-tail-mode))
;;
;;(use-package undo-tree
;;  :bind
;;  ("C-x u" . undo-tree-visualize)
;;  :diminish undo-tree-mode
;;  :commands global-undo-tree-mode
;;  :config
;;  (global-undo-tree-mode))
;;
;;(use-package visual-regexp
;;  :bind
;;  ("C-c r" . vr/replace)
;;  ("C-c q" . vr/query-replace))
;;
;;;;  (global-git-gutter-mode t)
;;;;  (global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
;;;;  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;;;;  (global-set-key (kbd "C-x v p") 'git-gutter:previous-hunk)
;;;;  (global-set-key (kbd "C-x v n") 'git-gutter:next-hunk)
;;;;  (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
;;;;  (global-set-key (kbd "C-x v SPC") 'git-gutter:mark-hunk)
;;;;  (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk))

(use-package rainbow-delimiters
  :defer t
  :diminish rainbow-delimiters-mode)

;; syntax check
(use-package flycheck
  :diminish flycheck-mode
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;;;; function-args
;;;;(use-package function-args
;;;;  :diminish function-args-mode
;;;;  :bind
;;;;  ("C-c d" . fa-idx-cycle-down)
;;;;  :defer t)
;;
;;;; (use-package mic-paren
;;;;   :commands paren-activate
;;;;   :config
;;;;   (paren-activate))

(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-use-smartparens-bindings)
  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

;;;; Projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" my-tmp-dir))
  (setq projectile-use-git-grep t)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t))

;;; git
(use-package git
  :defer t
  :bind
  ("<f9>"  . git-blame-mode))

;; magit
(use-package magit
  :ensure t
  :bind
  (:prefix-map magit-prefix-map
               :prefix "C-c g"
               (("a" . magit-stage-file) ; the closest analog to git add
                ("c" . magit-checkout)
                ("C" . magit-branch-and-checkout)
                ("d" . magit-diff-range)
                ("D" . magit-discard)
                ("f" . magit-fetch-other)
                ("g" . vc-git-grep)
                ("G" . magit-gitignore-globally)
                ("i" . magit-init)
                ("l" . magit-log-other)
                ("m" . magit-merge-plain)
                ("M" . magit)
                ("n" . magit-notes-edit)
                ("p" . magit-pull-branch)
                ("P" . magit-push-other)
                ("r" . magit-show-refs-head)
                ("R" . magit-rebase-branch)
                ("s" . magit-status)
                ("S" . magit-stash-both)
                ("t" . magit-tag-create)
                ("T" . magit-tag-delete)
                ("u" . magit-unstage)
                ("U" . magit-update-index))))

;; magithub
(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t))

;; magit-gh-pulls
(use-package magit-gh-pulls
  :after magit
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

;;; last change
(use-package goto-last-change
  :defer t
  :bind
  ("C-x ," . goto-last-change))

;;;; semantic
;;;; (use-package semantic
;;;;  :defer t
;;;;  :config
;;;;  ;; Increase the delay before activation
;;;;  (setq semantic-idle-scheduler-idle-time 30)
;;;;  ;; Don't reparse really big buffers.
;;;;  (setq semantic-idle-scheduler-max-buffer-size 100000)
;;;;  ;; Increase the delay before doing slow work
;;;;  (setq semantic-idle-scheduler-work-idle-time 30)
;;;;  :init
;;;;  (semantic-mode 1))
;;
;;;; smart scan
;;;; (use-package smartscan
;;;;   :defer t
;;;;  :bind
;;;;  ("C-c n" . smartscan-symbol-go-forward)
;;;;  ("C-c p" . smartscan-symbol-go-backward)
;;;;  ;; :init
;;;;  ;; (global-smartscan-mode 1)
;;;;  )


;;;
;;; Format code the Juniper way
;;;
;; (require 'cc-mode) ;; should not be required
(c-add-style "juniper"
             '(
               (c-basic-offset . 4)
               (c-comment-only-line-offset . 0)
               (c-offsets-alist
                . (
                   ;; first line of a new statement block
                   (statement-block-intro . +)

                   ;; First line of a K&R C argument declaration.
                   (knr-argdecl-intro . +)

                   ;; The brace that opens a substatement block.
                   (substatement-open . 0)

                   ;; Any non-special C label.
                   (label . 2)

                   ;; A `case' or `default' label.
                   (case-label . 0)

                   ;; The first line in a case block that starts with
                   ;; a brace.
                   (statement-case-open . +)

                   ;; A continuation of a statement.
                   (statement-cont . +)

                   ;; The first line after a conditional or loop
                   ;; construct.
                   (substatement . +)

                   ;; The first line in an argument list.
                   (arglist-intro . c-lineup-arglist-intro-after-paren)

                   ;; The solo close paren of an argument list.
                   (arglist-close . c-lineup-arglist)

                   ;; Brace that opens an in-class inline method.
                   (inline-open . 0)

                   ;; Open brace of an enum or static array list.
                   (brace-list-open . 0)))

               (c-special-indent-hook . c-gnu-impose-minimum)
               (c-block-comment-prefix . "")))

(defun juniper-c-default-style ()
  "Set the default c-style for Juniper."
  (interactive)
  (message "Loading default c-style for Juniper")
  (c-set-style "juniper")
  (define-key c-mode-map "\C-cd" 'vc-diff)
  (font-lock-add-keywords 'c-mode
                          '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))
  (defun toggle-tab-width ()
    "Toggle the value of tab-width between 4 and 8"
    (interactive)
    (if (= tab-width 8)
        (setq tab-width 4)
      ((insert )f (= tab-width 4)
       (setq tab-width 8)))
    (scroll-up 0))
  (defun toggle-c-offset ()
    "Toggle the value of c-basic-offset between 4 and 8"
    (interactive)
    (if (= c-basic-offset 8)
        (setq c-basic-offset 4)
      (if (= c-basic-offset 4)
          (setq c-basic-offset 8)))
    (scroll-up 0))
  ;; Complement to next-error
  (defun previous-error (n)
    "Visit previous compilation error message and corresponding source code."
    (interactive "p")
    (next-error (- n)))
  (abbrev-mode nil)
  (message "Loaded Juniper C default style"))

(add-hook 'c-mode-common-hook 'juniper-c-default-style)

;;;; (font-lock-add-keywords nil '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))
;;;; (rainbow-delimiters-mode)
;;;; all the three below do not work :(
;;;; (diminish abbrev-mode "")
;;;; (diminish flyspell-mode "")
;;;; (diminish function-args-mode "")

;; python
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(use-package python-mode
  :defer t
  :config
  (elpy-enable)
  (autoload 'py-yapf "yapf" "Yet Another Python Formatter" t)
  ;; (py-yapf-enable-on-save)
  (anaconda-mode)
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    ;;(add-hook 'elpy-mode-hook 'py-yapf-enable-on-save)
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  ;; (anaconda-eldoc-mode)
  :init
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq py-indent-offset tab-width)
  (setq py-smart-indentation t)
  (setq python-shell-native-complete nil)
  (setq python-indent-offset 4))

;;;(use-package jedi
;;;  :defer t
;;;  :config
;;;  (autoload 'jedi-setup "jedi" nil t)
;;;  (jedi:setup)
;;;  :init
;;;  (setq jedi:setup-keys t
;;;        jedi:complete-on-dot t))
;;
;;;; Versa XML file settings
;;;; (setq nxml-child-indent 4
;;;;      nxml-attribute-indent 4
;;;;      nxml-slash-auto-complete-flag t)
;;
;;;; waf
;;;; (setq auto-mode-alist (cons '("wscript" . python-mode) auto-mode-alist))
;;
;;;; autoload gdb-script-mode while editing .gdb files
;;;; (add-to-list 'auto-mode-alist '("\\.gdb$" . gdb-script-mode))
;;
;;;; use js3-mode instead of default javascript mode
;;;; (add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
;;;; (add-to-list 'auto-mode-alist '("\\.json$" . js3-mode))
;;;; (use-package js3-mode
;;;;   :defer t
;;;;  :init
;;;;  (setq js3-indent-level 4))

;; go lang
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(require 'go-guru)
(use-package go-flymake
  :diminish flymake-mode)
(require 'go-flycheck)
(require 'company-go)
(use-package go-mode
  :ensure t
  :init
  (setq gofmt-command "goimports")
  (setq c-basic-offset 8)
  (set (make-local-variable 'company-backends) '(company-go))
  (add-hook 'before-save-hook 'gofmt-before-save)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH"))
  :bind
  ("C-c m" . gofmt)
  ("M-." . godef-jump)
  ("C-c C-r" . go-remove-unused-imports)
  ("C-c i" . go-goto-imports)
  ;;  ("M-/" . company-go)
  :config
  (go-guru-hl-identifier-mode)
  (go-eldoc-setup))

;;;; ;; focus
;;;; (use-package focus
;;;;   :defer t)

;; eldoc
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :commands eldoc-mode)

;; protobuf
(use-package protobuf-mode
   :config
   (add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
   :init
   (setq c-basic-offset 8))

;;;; ;; vdiff
;;;; (use-package vdiff
;;;;   :config
;;;;   (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map))


;; tickscripts
(use-package tickscript-mode)

;; dockerfile
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; hook for all programming mode
(defun my-common-prog-settings()
  (diff-hl-mode +1)
  ;; (diff-hl-margin-mode t)
  ;; (diff-hl-flydiff-mode t)
  (electric-indent-mode +1)
  ;; (eldoc-mode +1)             ;; eldoc-mode can be irritating
  ;; (fa-config-default)
  (flycheck-mode)
  (flyspell-prog-mode)
  (projectile-mode +1)
  ;; (focus-mode +1)
  ;; (function-args-mode +1)
  ;; (highlight-symbol-mode +1)
  ;; (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "C-<backspace>") 'c-hungry-backspace)
  (prettify-symbols-mode +1)
  (rainbow-delimiters-mode +1)
  ;; (setq-default indent-tabs-mode nil)                 ;; use spaces, not tabs
  (setq-default indent-tabs-mode t)                      ;; use tabs, not spaces
  (setq-default tab-width 8)

  (setq show-paren-style 'parenthesis)
  (setq vc-svn-diff-switches '"-u")
  (show-paren-mode +1)
  (smartparens-mode +1)
  (volatile-highlights-mode +1)
  (whitespace-mode +1)
  (font-lock-add-keywords nil
                          '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))

  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace
  ;; (git-gutter-mode)
  ;; (setq tab-always-indent 'complete)
)

;; (with-eval-after-load 'python
;;   (defun python-shell-completion-native-try ()
;;     "Return non-nil if can trigger native completion."
;;     (let ((python-shell-completion-native-enable t)
;;           (python-shell-completion-native-output-timeout
;;            python-shell-completion-native-try-output-timeout))
;;      (python-shell-completion-native-get-completions
;;        (get-buffer-process (current-buffer)) nil "_"))))

(mapc (lambda(mode-hook)
       (add-hook mode-hook 'my-common-prog-settings))
     '(c-mode-common-hook python-mode-hook js3-mode-hook
     java-mode-hook sh-mode-hook go-mode-hook json-mode-hook))

(require 'my-sandboxes)

;;;; test section
;;;; (require 'magit-gh-pulls)
;;;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
;;
(provide 'my-devel)
