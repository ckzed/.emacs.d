;;; package --- Summary

;;; Commentary:
;; Org setup

;;; Code:
(defvar my-org-dir (expand-file-name "~/Dropbox/Agenda/org/zededa") "Directory for org files.")
(defvar my-default-todo (expand-file-name "todo.org" my-org-dir) "Default todo list.")
(defvar work-log (expand-file-name "work.org" my-org-dir) "Work log.")
(defvar jira-dir (expand-file-name "~/.org-jira") "Directory for Jira.")
(defvar sre-tickets (expand-file-name "SRE.org" jira-dir) "SRE Jira Tickets.")

(with-eval-after-load "org-faces"
  (defface org-agenda-deadline-overdue '((t (:foreground "cyan")))
    "Face used to highlight tasks whose due date is in the past.")
  (defface org-agenda-deadline-today '((t (:weight bold :foreground "#4F4A3D" :background "#FFFFCC")))
    "Face used to highlight tasks whose due date is today.")
  (defface org-agenda-deadline-tomorrow '((t (:foreground "#40A80B")))
    "Face used to highlight tasks whose due date is tomorrow.")
  (defface org-agenda-deadline-future '((t (:foreground "#40A80B")))
    "Face used to highlight tasks whose due date is for later."))

(global-set-key (kbd "C-c t") (lambda() (interactive)(find-file (expand-file-name "todo.org" my-org-dir))))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

;; Org
(use-package org
  :defer 5
  :ensure t
  :init
  (setq org-agenda-files (list my-default-todo sre-tickets
                               (expand-file-name "schedule.org" my-org-dir)
                               (expand-file-name "someday.org" my-org-dir)
                               (expand-file-name "pivotal.org" my-org-dir)
                               (expand-file-name "sprint.org" my-org-dir))

        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path t
        org-refile-allow-creating-parent-nodes 'confirm
        org-agenda-include-diary t
        org-agenda-start-day "-1d"  ; start from yesterday
        org-agenda-start-on-weekday nil
        org-agenda-span 'week
        org-agenda-skip-scheduled-if-deadline-is-shown t
        ;; org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
        ;;                           (todo . " %i %-12:c%l")
        ;; (tags . " %i %-12:c")
        ;;                           (search . " %i %-12:c"))

        org-log-done t
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "UNDER REVIEW(r)" "DONE(d)")
          (sequence "WAITING(w@)" "HOLD(h@)" "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("UNDER REVIEW" :foreground "orange" :weight bold :border 1)
          ("CANCELLED" :foreground "brown" :weight bold))
        org-ellipsis "⤵")

  ;; Capture templates
  (setq org-capture-templates '(
                                ("w" "Work log" entry
                                 (file+olp+datetree work-log) "* %<%Y-%m-%d %a %H:%M> %?" :tree-type week :empty-lines 1)
                                ("t" "TODO" entry
                                 (file+headline my-default-todo "Tasks") "* TODO %? %^G \n  %U" :empty-lines 1)
                                ("s" "Scheduled TODO" entry
                                 (file+headline my-default-todo "Tasks") "* TODO %? %^G \nSCHEDULED: %^t\n  %U" :empty-lines 1)
                                ("d" "Deadline" entry
                                 (file+headline my-default-todo "Deadlines") "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)
                                ("p" "Priority" entry
                                 (file+headline my-default-todo "Priority") "* TODO [#A] %? %^G \n  SCHEDULED: %^t")
                                ("a" "Appointment" entry
                                 (file+headline my-default-todo "Appointment") "* %? %^G \n  %^t")
                                ("l" "Link" entry
                                 (file+headline my-default-todo "Link") "* TODO %a %? %^G\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
                                ("n" "Note" entry
                                 (file+headline my-default-todo "Notes") "* %? %^G\n%U" :empty-lines 1)
                                ("i" "Ideas" entry
                                 (file+olp+datetree "~/Dropbox/Agenda/org/ideas.org") "* %?\nEntered on %U\n  %i\n  %a" :empty-lines 1)
                                ("j" "Journal" entry
                                 (file+datetree "~/Dropbox/Agenda/org/journal.org")
                                 "* %? %^G\nEntered on %U\n")))
  :config
  ;; (defun jump-to-org-agenda ()
  ;;   (interactive)
  ;;   (let ((buf (get-buffer "*Org Agenda*"))
  ;;         wind)
  ;;     (if buf
  ;;         (if (setq wind (get-buffer-window buf))
  ;;             (select-window wind)
  ;;           (if (called-interactively-p)
  ;;               (progn
  ;;                 (select-window (display-buffer buf t t))
  ;;                 (org-fit-window-to-buffer)
  ;;                 (org-agenda-redo)
  ;;                 )
  ;;             (with-selected-window (display-buffer buf)
  ;;               (org-fit-window-to-buffer)
  ;;               (org-agenda-redo)
  ;;               )))
  ;;       (call-interactively 'org-agenda-list)))
  ;;   (let ((buf (get-buffer "*Calendar*")))
  ;;     (unless (get-buffer-window buf)
  ;;    	(org-agenda-goto-calendar)))
  ;;   )
  ;; (run-with-idle-timer 600 t 'jump-to-org-agenda)

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
  (add-hook 'org-mode-hook
	    (lambda ()
	      (flycheck-mode)
	      (flyspell-mode)
	      (writegood-mode)
	      (org-bullets-mode-hook)
	      (add-hook 'before-save-hook 'org-align-all-tags nil t))))

;; org-bullets
(use-package org-bullets
  :defer 5
  :commands org-bullets-mode
  :preface
  (defun org-bullets-mode-hook ()
    (org-bullets-mode +1)))

;; org-gcal
(use-package org-gcal
  :defer 3
  :init
  (setq org-gcal-client-id "795438884323-7rhs1rn1firp36kgj03t215dk383peks.apps.googleusercontent.com"
        org-gcal-client-secret "H9rNjIj6rJ7I4fe2CTAxXvYG"
        org-gcal-file-alist '(("chirag@zededa.com" .  "~/.schedule.org"))))

(use-package org-alert
  :defer 3
  :init
  (setq alert-default-style 'osx-notifier
	org-alert-notification-title "Org Scheduler"))

(use-package idle-org-agenda
  :after org-agenda
  :ensure t
  :config (idle-org-agenda-mode))

(require 'india-holidays-2021)

(provide 'my-org)
;;; my-org.el ends here
