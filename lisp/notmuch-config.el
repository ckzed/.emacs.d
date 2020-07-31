;;; package --- Summary
;;; Commentary:
;;; -*-Emacs-Lisp-*-

;;; Code:
;; not much config comes here
(use-package notmuch
  :defer t
  :config
  (setq notmuch-fcc-dirs nil
        notmuch-hello-logo nil
        notmuch-saved-searches
        '((:name "to me" :query "(to:chirag OR cc:chirag) AND tag:unread" :key "m")
          (:name "imp"   :query "tag:important AND tag:unread"            :key "i")
          (:name "new"   :query "tag:unread"                              :key "u")
          (:name "flagged" :query "tag:flagged"                           :key "f")
          (:name "today" :query "date:today..now"                         :key "t")
          (:name "yesterday" :query "date:yesterday..yesterday"           :key "y")
          (:name "this week" :query "date:sunday..now"                    :key "w")
          (:name "last week" :query "date:last_week..sat"                 :key "l")
;;	  (:name "devt"   :query "tag:devt"                               :key "d")
;;	  (:name "devt-cloud" :query "tag:devt-cloud"                     :key "d")
;;        (:name "iot"   :query "tag:iot"                                 :key "i")
;;	  (:name "team-india"   :query "tag:team-india")
;;	  (:name "prod-announce"   :query "tag:prod-announce")
;;	  (:name "github" :query "tag:github"                             :key "g")
;;	  (:name "pivotal" :query "tag:pivotal")
;;	  (:name "adp" :query "tag:adp")
;;        (:name "omniwifi" :query "tag:omniwifi"                         :key "o")
          (:name "later"           :query "tag:later"                     :key "L")
          (:name "sent this week"  :query "tag:sent AND date:sun..now")
          (:name "sent last week"  :query "tag:sent AND date:last_week..sat")
          (:name "all"             :query "*")
          (:name "all sent"        :query "tag:sent OR tag:replied"))
        notmuch-search-result-format
        '(("date"    . "%12s ")
          ("count"   . "%6s ")
          ("authors" . "%-23s ")
          ("subject" . "%s ")
          ("tags"    . "(%s)"))
        notmuch-search-line-faces
        '(("unread"  .   (:foreground "green"))
          ("deleted" .   (:foreground "red"))
          ("important" . (:foreground "lightblue"))
          ("flagged" .   (:foreground "orange")))
	notmuch-address-command 'internal)

  ;; spam
  (define-key notmuch-show-mode-map "S"
    (lambda ()
      "mark message as spam"
      (interactive)
      (notmuch-show-tag (list "+spam" "-inbox"))))
  (define-key notmuch-search-mode-map "S"
    (lambda ()
      "mark message as spam"
      (interactive)
      (notmuch-search-tag (list "-inbox" "+spam"))
      (next-line) ))

  ;; flag message to respond later
  (define-key notmuch-show-mode-map "L"
    (lambda ()
      "mark message to be responded Later"
      (interactive)
      (notmuch-show-tag (list "+flagged" "-inbox"))))
  (define-key notmuch-search-mode-map "L"
    (lambda ()
      "mark message to be responded Later"
      (interactive)
      (notmuch-search-tag (list "+flagged" "-inbox"))
      (next-line) ))

  ;; delete mail
  (define-key notmuch-show-mode-map "d"
    (lambda ()
      "mark message to be deleted"
      (interactive)
      (notmuch-show-tag (list "+deleted" "-inbox"))))
  (define-key notmuch-search-mode-map "d"
    (lambda ()
      "mark message to be deleted"
      (interactive)
      (notmuch-search-tag (list "-inbox" "+deleted"))
      (next-line) ))

  ;; bounce mail
  (define-key notmuch-show-mode-map "b"
      (lambda (&optional address)
        "Bounce the current message."
        (interactive "sBounce To: ")
        (notmuch-show-view-raw-message)
        (message-resend address)))

  ;; start polling for mails using offlineimap
  ;; (offlineimap)
  )

(provide 'notmuch-config)
