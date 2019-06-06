;;; package -- Summary

;;; Commentary:
;; Utility functions

;;; Code:
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

(global-set-key [remap kill-buffer] #'kill-this-buffer)

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

(defun it-multi-term-dedicated-toggle ()
  "jump back to previous location after toggling ded term off"
  (interactive)
  (if (multi-term-dedicated-exist-p)
      (progn
  (multi-term-dedicated-toggle)
  (switch-to-buffer-other-window old-buf))
    (progn
      (setq old-buf (current-buffer))
      (multi-term-dedicated-toggle))
    )
  )

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))
(global-set-key (kbd "C-c 5") 'revert-buffer-no-confirm)

(provide 'my-utils)
