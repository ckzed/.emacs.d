;; Org setup

(defvar my-status-dir (expand-file-name "~/Documents/org")
  "Directory where I store my status reports")

(global-set-key (kbd "C-c t") (lambda() (interactive)(find-file (expand-file-name ".todo.org" my-home-dir))))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

;; Org
(use-package org
  :ensure t
  :bind
  ("C-c f" . org-toggle-todo-and-fold)

  :config
  (defun jump-to-org-agenda ()
    (interactive)
    (let ((buf (get-buffer "*Org Agenda*"))
          wind)
      (if buf
          (if (setq wind (get-buffer-window buf))
              (select-window wind)
            (if (called-interactively-p)
                (progn
                  (select-window (display-buffer buf t t))
                  (org-fit-window-to-buffer)
                  (org-agenda-redo)
                  )
              (with-selected-window (display-buffer buf)
                (org-fit-window-to-buffer)
                (org-agenda-redo)
                )))
        (call-interactively 'org-agenda-list)))
    (let ((buf (get-buffer "*Calendar*")))
      (unless (get-buffer-window buf)
     	(org-agenda-goto-calendar)))
    )
  (run-with-idle-timer 600 t 'jump-to-org-agenda)

  (defadvice org-agenda (around org-agenda-fullscreen activate)
    (window-configuration-to-register :org-agenda-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice org-agenda-quit (around org-agenda-quit-fullscreen activate)
    ad-do-it
    (jump-to-register :org-agenda-fullscreen))

  (unless (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))
  (add-to-list 'org-export-latex-classes
               '("article"
                 "\\documentclass{article}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;;   (add-to-list 'org-export-latex-classes
  ;;               '("moderncv"
  ;;                 "\\documentclass{moderncv}"
  ;;                 ("\\

  ;; (require 'india-holidays-2018)

  (defun my-org-insert-week-template ()
    "Insert org headline template for the week.

Example:

** 2016-07-04 (week 27)
*** <2016-07-04 Mon - day 186>
*** <2016-07-05 Tue - day 187>
*** <2016-07-06 Wed - day 188>
*** <2016-07-07 Thu - day 189>
*** <2016-07-08 Fri - day 190>
*** <2016-07-09 Sat - day 191>
*** <2016-07-10 Sun - day 192>"

    (interactive)
    (let ((week-start (current-time))
          (oneday (seconds-to-time 86400)))
      (loop
       (if (string= (format-time-string "%w" week-start) "1")
           (return)
         (setq week-start (time-subtract week-start oneday))))
      (let ((week-hdr "%Y-%m-%d (week %U)")
            (day-hdr "<%Y-%m-%d %a - day %j>"))
        (save-excursion
          (insert "** " (format-time-string week-hdr week-start) "\n")
          (dotimes (i 7)
            (insert "*** " (format-time-string day-hdr week-start) "\n")
            (setq week-start (time-add week-start oneday)))))))

  ;; Mark item on org-agenda as done
  (defun org-toggle-todo-and-fold ()
    (interactive)
    (save-excursion
      (org-back-to-heading t) ;; Make sure command works even if point is
      ;; below target heading
      (cond ((looking-at "\*+ TODO")
             (org-todo "DONE")
             (hide-subtree))
            ((looking-at "\*+ DONE")
             (org-todo "TODO")
             (hide-subtree))
            (t (message "Can only toggle between TODO and DONE.")))))



  ;; Set up variables
  (setq org-agenda-files (list (expand-file-name ".todo.org" my-home-dir))
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@)" "HOLD(h@)" "|" "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("NEXT" :foreground "blue" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold))
	org-columns-default-format "%25ITEM %TODO %3PRIORITY %TIMESTAMP"

        ;; Add a timestamp when a certain TODO item was finished.
        org-log-done 'time

        ;;open agenda in current window
        org-agenda-window-setup (quote current-window)
        ;;warn me of any deadlines in next 7 days
        org-deadline-warning-days 7
        ;;show me tasks scheduled or due in next fortnight
        ;; org-agenda-span (quote fortnight)
	org-agenda-span 'day
        ;;don't show tasks as scheduled if they are already shown as a deadline
        org-agenda-skip-scheduled-if-deadline-is-shown t
        ;;don't give awarning colour to tasks with impending deadlines
        ;;if they are scheduled to be done
        org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
        ;;don't show tasks that are scheduled or have deadlines in the
        ;;normal todo list
        org-agenda-todo-ignore-deadlines (quote all)
        org-agenda-todo-ignore-scheduled (quote all)
        ;;sort tasks in order of when they are due and then by priority
        org-agenda-sorting-strategy
        (quote
         ((agenda deadline-up priority-down)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep)))

        ;; Custom timestamp formats.
        ;; org-display-custom-times t
        ;; org-time-stamp-custom-formats '("<%d-%m-%Y %a>" . "<%d-%m-%Y %a %H:%M>")
        org-ellipsis "⤵"
        org-M-RET-my-split-line nil)

  ;; templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (expand-file-name ".todo.org" my-home-directory)))
          "* TODO %?\n  %i\n  %a"))
  (add-hook 'org-mode-hook
            (lambda ()
              (flycheck-mode)
	      (flyspell-mode)
	      (writegood-mode)
              (add-hook 'before-save-hook 'org-align-all-tags nil t))))

;; org-bullets
(use-package org-bullets
  :defer t
  :commands org-bullets-mode
  :preface
  (defun org-bullets-mode-hook ()
    (setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇"))
    (org-bullets-mode +1))
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode-hook))

;; start org-mode upon start
(org-agenda nil "a")

;; idle-org-agenda
;; (use-package idle-org-agenda
;;     :after org-agenda
;;     :ensure t
;;     :config (idle-org-agenda-mode))

(provide 'my-org)
