;; Utility functions

;; functions (probably these need to go into different file?)
(defun file-reopen-as-root ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(defun kill-some-buffers (&optional list)
  "Kill each buffer in LIST, with prompting for modified files.
LIST defaults to all existing live buffers.
Modified 2002/02/26.C.16:26 from \"files.el\""
  (interactive)
  (if (null list) (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
	   (name (buffer-name buffer)))
      (and (not (string-equal name ""))
	   (/= (aref name 0) ? )
	   (or (not (buffer-modified-p buffer))
	       (equal (string-to-char name) ?*)
	       (eq major-mode 'dired-mode)
	       (yes-or-no-p (format "Kill buffer %s? " name)))
	   (kill-buffer buffer)))
    (setq list (cdr list)))
  (find-file "~"))

;; Useful functions
;; custom screen splitting
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)


;; I want an easy command for opening new shells:
(defun new-shell (name)
  "Opens a new shell buffer with the given name in
    asterisks (*name*) in the current directory and changes the
    prompt to 'name>'."
  (interactive "sName: ")
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
    (ansi-term (current-buffer))
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer))
                        (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))
(global-set-key (kbd "C-c C-s") 'new-shell)


(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     ((and (boundp 'subword-mode)
           subword-mode)
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))
(global-set-key (kbd "C-<backspace>") 'contextual-backspace)


(defun switch-to-last-buffer ()
  "Switch to the previously selected buffer in the current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer)) nil t))
(global-set-key (kbd "M-o") 'switch-to-last-buffer)

(defun indent-buffer ()
  "Indent the entire buffer"
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(provide 'my-utils)
