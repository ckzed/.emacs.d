(deftheme black "Created 2019-01-12.")
(let ((class '((class color) (min-colors 89)))
      ;; (fg           "#aaaaaa") ;; low contrast colors fg/bg
      ;; (bg           "#222222")
      ;; (fg           "wheat")
      ;; (bg           "#111111")
      ;; (fg           "#c0c0c0")
      (fg                     "#999999")
      (fg1                    "#d7d7d7")
      (fg2                    "#c6c6c6")
      (fg3                    "#b5b5b5")
      (fg4                    "#a3a3a3")
      (fg5                    "#999999")
      (bg                     "#000000")
      (bg1                    "#000000")
      (bg2                    "#141414")
      (bg3                    "#292929")
      (bg4                    "#3d3d3d")
      (background-blue        "#444444")
      (background-orange      "#5f5f5f")
      (background-red         "#5f5f5f")
      (baregray               "gray13")
      (blue                   "#0000ff")
      (blue                   "#5fafff")
      (blue-dark              "#005f87")
      (bright-background-blue "#4e5079")
      (bright-background-red  "#744a5b")
      (bright-background-red  "#744a5b")
      (builtin                "#e6a5ff")
      (comment                "#dddd00")
      (comment                "#fefb00")
      (const                  "#00d8f3")
      (const                  "#d7af87")
      (cursor                 "#ffff00")
      (cyan                   "#00ffff")
      (dgreen                 "#00aa00")
      (func                   "#7ffcd4")
      (func                   "#ffa565")
      (green                  "#00ff00")
      (indianred              "#cd5c5c")
      (keyword                "#27c7ff")
      (keyword                "#87ceff")
      (lightgreen             "#adff2f")
      (magenta                "#ff00ff")
      (orange                 "#d7875f")
      (orange-light           "#d7af87")
      (purple                 "#d787d7")
      (purple-dark            "#5f5f5f")
      (red                    "#ff0000")
      (searchcolor1           "#228b22")
      (searchcolor2           "#6b8e35")
      (searchcolor3           "#556b2f")
      (selected               "#00868b")
      (skyblue                "#00bfff")
      (steelblue              "#4682b4")
      (str                    "#ff80b0")
      (str                    "#ffbbff")
      (type                   "#3ad798")
      (var                    "#bdc553")
      (warning                "#ff0000")
      (warning2               "#ff8800")
      (white                  "#ffffff")
      (yellow                 "#ffff00")
      ;; (baregray     "#666666"))
      ;; (comment "#8a0f00")
      ;; (keyword "#00bfff")
      ;; (keyword "#27c7ff")
      (black                  "#000000"))

  (custom-theme-set-faces
   'black
   `(default ((t (:background ,bg1 :foreground ,fg5))))
   `(default-italic ((,class (:italic t))))

   `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
   `(company-echo-common ((,class (:foreground ,bg1 :background ,fg1))))
   `(company-preview ((,class (:background ,bg1 :foreground ,var))))
   `(company-preview-common ((,class (:foreground ,bg2 :foreground ,fg3))))
   `(company-preview-search ((,class (:foreground ,type :background ,bg1))))
   `(company-scrollbar-bg ((,class (:background ,bg3))))
   `(company-scrollbar-fg ((,class (:foreground ,keyword))))
   `(company-template-field ((,class (:inherit region))))
   `(company-tooltip ((,class (:foreground ,fg2 :background ,bg1 :bold t))))
   `(company-tooltip-common ((,class ( :foreground ,fg3))))
   `(company-tooltip-common-selection ((,class (:foreground ,str))))
   `(company-tooltip-mouse ((,class (:inherit highlight))))
   `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg3))))
   `(company-tooltop-annotation ((,class (:foreground ,const))))
   `(cursor ((,class (:background ,cursor))))
   `(diff-hl-change ((,class :background ,skyblue)))
   `(diff-hl-delete ((,class :background ,indianred)))
   `(diff-hl-insert ((,class :background ,dgreen)))
   `(diff-hl-unknown ((,class :background ,cyan)))
   `(dired-directory ((t  (:foreground ,yellow))))
   `(ffap ((,class (:foreground ,fg4))))
   `(flycheck-error ((,class (:underline (:color ,red :style wave)))))
   `(flycheck-fringe-error ((,class (:foreground ,red :background ,background-red :weight bold :inverse-video t))))
   `(flycheck-fringe-info ((,class (:background ,background-blue :foreground ,blue :weight bold :inverse-video t))))
   `(flycheck-fringe-warning ((,class (:background ,background-orange :foreground ,orange :weight bold :inverse-video t))))
   `(flycheck-warning ((,class (:underline (:color ,red :style wave)))))
   `(font-latex-bold-face ((,class (:foreground ,type))))
   `(font-latex-italic-face ((,class (:foreground ,var :italic t))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
   `(font-latex-string-face ((,class (:foreground ,str))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,func ))))
   `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-type-face ((,class (:foreground ,type ))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg1))))
   `(fringe ((,class (:background ,bg2 :foreground ,fg4))))
   `(gnus-header-content ((,class (:foreground ,keyword))))
   `(gnus-header-from ((,class (:foreground ,var))))
   `(gnus-header-name ((,class (:foreground ,type))))
   `(gnus-header-subject ((,class (:foreground ,func :bold t))))
   `(helm-bookmark-w3m ((,class (:foreground ,type))))
   `(helm-buffer-not-saved ((,class (:foreground ,type :background ,bg1))))
   `(helm-buffer-process ((,class (:foreground ,builtin :background ,bg1))))
   `(helm-buffer-saved-out ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-buffer-size ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-candidate-number ((,class (:foreground ,bg1 :background ,fg1))))
   `(helm-ff-directory ((,class (:foreground ,func :background ,bg1 :weight bold))))
   `(helm-ff-executable ((,class (:foreground ,var :background ,bg1 :weight normal))))
   `(helm-ff-file ((,class (:foreground ,fg1 :background ,bg1 :weight normal))))
   `(helm-ff-invalid-symlink ((,class (:foreground ,warning2 :background ,bg1 :weight bold))))
   `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
   `(helm-ff-symlink ((,class (:foreground ,keyword :background ,bg1 :weight bold))))
   `(helm-grep-cmd-line ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-grep-file ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-grep-finish ((,class (:foreground ,fg2 :background ,bg1))))
   `(helm-grep-lineno ((,class (:foreground ,fg1 :background ,bg1))))
   `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((,class (:foreground ,func :background ,bg1))))
   `(helm-header ((,class (:foreground ,fg2 :background ,bg1 :underline nil :box nil))))
   `(helm-moccur-buffer ((,class (:foreground ,func :background ,bg1))))
   `(helm-selection ((,class (:background ,bg2 :underline nil))))
   `(helm-selection-line ((,class (:background ,bg2))))
   `(helm-separator ((,class (:foreground ,type :background ,bg1))))
   `(helm-source-go-package-godoc-description ((,class (:foreground ,str))))
   `(helm-source-header ((,class (:foreground ,keyword :background ,bg1 :underline nil :weight bold))))
   `(helm-time-zone-current ((,class (:foreground ,builtin :background ,bg1))))
   `(helm-time-zone-home ((,class (:foreground ,type :background ,bg1))))
   `(helm-visible-mark ((,class (:foreground ,bg1 :background ,bg3))))
   `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
   `(highlight-face ((,class :foreground ,orange :background ,bg)))
   `(hl-line  ((t (:background ,baregray))))
   ;; `(hl-line ((,class (:underline t))))
   `(icompletep-determined ((,class :foreground ,builtin)))
   `(ido-first-match ((,class (:foreground ,keyword :bold t))))
   `(ido-only-match ((,class (:foreground ,warning))))
   `(info-quoted-name ((,class (:foreground ,builtin))))
   `(info-string ((,class (:foreground ,str))))
   `(isearch ((,class (:bold t :foreground ,warning :background ,bg3))))
   `(ivy-current-match ((,class :foreground ,yellow :background ,bg1)))
   `(ivy-subdir ((,class :foreground ,green :background ,bg1)))
   `(jde-java-font-lock-constant-face ((t (:foreground ,const))))
   `(jde-java-font-lock-modifier-face ((t (:foreground ,fg2))))
   `(jde-java-font-lock-number-face ((t (:foreground ,var))))
   `(jde-java-font-lock-package-face ((t (:foreground ,var))))
   `(jde-java-font-lock-private-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-public-face ((t (:foreground ,keyword))))
   `(jde-jave-font-lock-protected-face ((t (:foreground ,keyword))))
   `(js2-external-variable ((,class (:foreground ,type  ))))
   `(js2-function-param ((,class (:foreground ,const))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,var))))
   `(js2-jsdoc-value ((,class (:foreground ,str))))
   `(js2-private-function-call ((,class (:foreground ,const))))
   `(js2-private-member ((,class (:foreground ,fg3))))
   `(js3-error-face ((,class (:underline ,warning))))
   `(js3-external-variable-face ((,class (:foreground ,var))))
   `(js3-function-param-face ((,class (:foreground ,fg2))))
   `(js3-instance-member-face ((,class (:foreground ,const))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
   `(js3-warning-face ((,class (:underline ,keyword))))
   `(lazy-highlight ((,class (:foreground ,fg2 :background ,bg3))))
   `(link ((,class (:foreground ,const :underline t))))
   `(magit-branch ((,class (:foreground ,const :weight bold))))
   `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,fg3))))
   `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg3))))
   `(magit-diffstat-added   ((,class (:foreground ,type))))
   `(magit-diffstat-removed ((,class (:foreground ,var))))
   `(magit-hash ((,class (:foreground ,fg2))))
   `(magit-hunk-heading           ((,class (:background ,bg3))))
   `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
   `(magit-item-highlight ((,class :background ,bg3)))
   `(magit-log-author ((,class (:foreground ,fg3))))
   `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
   `(magit-process-ok ((,class (:foreground ,func :weight bold))))
   `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
   `(magit-section-highlight      ((,class (:background ,bg2))))
   `(message-cited-text ((,class (:foreground ,comment))))
   `(message-header-cc ((,class (:foreground ,purple))))
   `(message-header-name ((,class (:foreground ,blue :weight bold))))
   `(message-header-other ((,class (:foreground ,purple))))
   `(message-header-subject ((,class (:foreground ,green))))
   `(message-header-to ((,class (:foreground ,purple))))
   `(message-separator ((,class (:foreground ,red :weight bold))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))
   `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :line-width 1 :color nil :foreground ,orange-light :bold t :background ,bg3))))
   ;; `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,fg4 :background ,bg4))))
   `(mode-line-buffer-id ((,class (:bold t :foreground ,func :background nil))))
   `(mode-line-emphasis ((,class (:foreground ,fg1))))
   `(mode-line-highlight ((,class (:foreground ,keyword :box nil :weight bold))))
   `(mode-line-inactive ((,class (:background ,bg2 :foreground ,fg))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,var :background ,bg1 :weight normal))))
   `(mu4e-cited-1-face ((,class (:foreground ,fg2))))
   `(mu4e-cited-7-face ((,class (:foreground ,fg3))))
   `(mu4e-header-marks-face ((,class (:foreground ,type))))
   `(mu4e-view-url-number-face ((,class (:foreground ,type))))
   `(notmuch-crypto-decryption ((,class (:foreground ,purple))))
   `(notmuch-crypto-part-header ((,class (:foreground ,blue))))
   `(notmuch-crypto-signature-bad ((,class (:background ,red :foreground ,bg :weight bold))))
   `(notmuch-crypto-signature-good ((,class (:background ,blue :foreground ,bg :weight bold))))
   `(notmuch-crypto-signature-good-key ((,class (:background ,blue :foreground ,bg :weight bold))))
   `(notmuch-crypto-signature-unknown ((,class (:foreground ,red))))
   `(notmuch-message-summary-face ((,class (:foreground ,purple :box (:line-width 1 :color ,bg)))))
   `(notmuch-search-count ((,class (:foreground ,blue :weight bold))))
   `(notmuch-search-date ((,class (:foreground ,purple))))
   `(notmuch-search-matching-authors ((,class (:foreground ,comment))))
   `(notmuch-search-subject ((,class (:foreground ,fg))))
   `(notmuch-search-unread-face ((,class (:weight bold))))
   `(notmuch-tag-face ((,class (:foreground ,green :weight bold))))
   `(notmuch-tree-match-author-face ((,class (:foreground ,orange))))
   `(notmuch-tree-match-tag-face ((,class (:foreground ,green :weight bold))))
   `(notmuch-widget-face ((,class (:foreground ,orange :weight bold))))
   `(org-agenda-date ((,class (:foreground ,var))))
   `(org-agenda-date-today ((,class (:weight bold :foreground ,purple))))
   `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg4))))
   `(org-agenda-done ((,class (:foreground ,bg4))))
   `(org-agenda-structure ((,class (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg3))))
   `(org-block ((,class (:foreground ,fg3))))
   `(org-code ((,class (:foreground ,fg2))))
   `(org-date ((,class (:underline t :foreground ,var) )))
   `(org-document-info-keyword ((,class (:foreground ,func))))
   `(org-done ((,class (:box (:line-width 1 :color ,bg3) :bold t :foreground ,bg4))))
   `(org-ellipsis ((,class (:foreground ,builtin))))
   `(org-footnote  ((,class (:underline t :foreground ,fg4))))
   `(org-hide ((,class (:foreground ,fg4))))
   `(org-level-1 ((,class (:bold t :foreground ,fg2))))
   `(org-level-2 ((,class (:bold nil :foreground ,fg3))))
   `(org-level-3 ((,class (:bold t :foreground ,fg4))))
   `(org-level-4 ((,class (:bold nil :foreground ,bg4))))
   `(org-link ((,class (:underline t :foreground ,type ))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-scheduled ((,class (:foreground ,type))))
   `(org-scheduled-today ((,class (:foreground ,green :weight bold))))
   `(org-sexp-date ((,class (:foreground ,fg4))))
   `(org-special-keyword ((,class (:foreground ,func))))
   `(org-todo ((,class (:box (:line-width 1 :color ,fg3) :foreground ,keyword :bold t))))
   `(org-verbatim ((,class (:foreground ,fg4))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-warning ((,class (:underline t :foreground ,warning))))
   `(persp-selected-face               ((t (:foreground ,green :weight bold))))
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg1)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,var)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,const)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,keyword)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,fg1)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,var)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))
   `(rainbow-delimiters-unmatched-face ((t (:inherit 'error))))
   `(region                            ((t (:background ,selected))))
   `(region ((,class (:background ,fg1 :foreground ,bg1))))
   `(show-paren-match-face ((,class (:background ,warning))))
   `(show-paren-match-face ((,class (:background ,warning))))
   `(slack-message-output-header ((,class :foreground ,blue :background ,background-blue :weight bold)))
   `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
   `(swiper-line-face                  ((t (:foreground ,white :background ,baregray))))
   `(swiper-match-face-1               ((t (:foreground ,white :background ,baregray))))
   `(term ((,class (:foreground ,fg1 :background ,bg1))))
   `(term-color-black ((,class (:foreground ,bg3 :background ,bg3))))
   `(term-color-blue ((,class (:foreground ,func :background ,func))))
   `(term-color-cyan ((,class (:foreground ,str :background ,str))))
   `(term-color-green ((,class (:foreground ,type :background ,bg3))))
   `(term-color-magenta ((,class (:foreground ,builtin :background ,builtin))))
   `(term-color-red ((,class (:foreground ,keyword :background ,bg3))))
   `(term-color-white ((,class (:foreground ,fg2 :background ,fg2))))
   `(term-color-yellow ((,class (:foreground ,var :background ,var))))
   `(trailing-whitespace ((,class :foreground nil :background ,warning)))
   `(trailing-whitespace ((,class :foreground nil :background ,warning)))
   `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
   `(vertical-border ((,class (:foreground ,fg3))))
   `(warning ((,class (:foreground ,warning :weight bold))))
   `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
   `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
   `(web-mode-html-tag-face ((,class (:foreground ,builtin))))
   `(web-mode-keyword-face ((,class (:foreground ,keyword))))
   `(web-mode-string-face ((,class (:foreground ,str))))
   `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
   `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'black)


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; black-theme.el ends here
