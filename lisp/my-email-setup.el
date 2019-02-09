;;; package -- Summary
;;; Commentary:

;;; -*-Emacs-Lisp-*-

;;; Code:

;; offlineimap
(use-package offlineimap
  :config
  (offlineimap))

;; write good
(add-hook 'message-mode-hook '(lambda()
                                (writegood-mode)
                                (flyspell-mode +1)
                                (flycheck-mode +1)))

;; mail variables
(require 'smtpmail-async)
(setq mm-text-html-renderer 'shr    	;; use shr to view html mail
      message-fill-column 72        	;; wrap text at column 72
      message-kill-buffer-on-exit t 	;; kill buffer after sending mails
      message-mode-hook (quote (flyspell-mode)) 	;; spell check
      message-citation-line-function 'message-insert-formatted-citation-line 	;; citation line
      message-citation-line-format "On %a, %b %d %Y at %r, %f wrote:"
      ;; message-directory "~/Maildir/[Gmail].Drafts"

      message-send-mail-function 'smtpmail-send-it
      send-mail-function 'async-smtpmail-send-it

      ;; smtpmail setting; disable if using sendmail
      smtpmail-stream-type 'starttls
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      ;; smtpmail-auth-credentials '(("smtp.gmail.com" 587 "chirag@zededa.com" nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-user "chirag@zededa.com"
      smtpmail-smtp-service 587
      starttls-extra-arguments '("--x509cafile" "/etc/ssl/cert.pem")
      message-send-mail-function 'async-smtpmail-send-it

      ;; message-send-mail-function 'message-send-mail-with-sendmail
      ;; message-auto-save-directory "~/Maildir/[Gmail].Drafts"
      message-auto-save-directory "~/.drafts"
      message-sendmail-f-is-evil t
      mail-envelope-from 'header
      mail-specify-envelope-from 'header
      message-sendmail-envelope-from 'header

      ;; this validates the cert, I have no idea why it's nil by default
      gnutls-verify-error t
      ;; probably too high for general usage,
      ;; but have no effect in the tests regardless
      gnutls-min-prime-bits 2048
      network-security-level 'high
      nsm-save-host-names t)

;; notmuch config
(require 'notmuch-config)

;; nevermore config
;; (require 'nevermore-config)

(provide 'my-email-setup)
