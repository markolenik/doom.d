;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! bibtex-completion org-roam)

;; Change frame splits
(package! transpose-frame)

;; At the moment I like `evil-commentary' more than
;; `evil-nerd-commenter'. Might change in future if I understand how
;; to replicate all features.
(package! evil-nerd-commenter :disable t)
(package! evil-commentary)
(package! visual-fill-column)
(package! jupyter)
(package! org-ref)
(package! org-roam-bibtex)
(package! org-roam-server)
;; company-org-roam makes writing text very slow
(package! company-org-roam :disable t)
;; Toggle org-mode latex fragment on cursor hover
(package! org-fragtog)

(package! el-patch)
