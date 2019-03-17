;;; package -- Summary

;;; Commentary:
;; Key bindings

;;; Code:
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'copy-line)
(global-set-key (kbd "M-C-r") 'query-replace)
(global-set-key (kbd "M-r") 'replace-string)
(global-set-key (kbd "M-h") 'help-command)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-right>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-left>") 'enlarge-window-horizontally)

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-c M-m") 'multi-term)

(global-set-key (kbd "C-z") 'bury-buffer)

(global-set-key (kbd "<f6>") (lambda() (interactive)(load-file "~/.emacs.d/init.el")))

(provide 'my-keybindings)
;;; my-keybindings ends here
